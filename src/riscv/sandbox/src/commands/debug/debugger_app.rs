// SPDX-FileCopyrightText: 2024 Nomadic Labs <contact@nomadic-labs.com>
// SPDX-FileCopyrightText: 2024-2025 TriliTech <contact@trili.tech>
//
// SPDX-License-Identifier: MIT

use std::borrow::Cow;
use std::collections::BTreeMap;
use std::collections::HashMap;
use std::collections::HashSet;
use std::ops::Bound;

use color_eyre::Result;
use crossterm::event;
use crossterm::event::Event;
use crossterm::event::KeyCode;
use crossterm::event::KeyEventKind;
use goblin::elf;
use goblin::elf::Elf;
use goblin::elf::header::ET_DYN;
use octez_riscv::bits::Bits64;
use octez_riscv::kernel_loader::Error;
use octez_riscv::machine_state::AccessType;
use octez_riscv::machine_state::CacheLayouts;
use octez_riscv::machine_state::MachineCoreState;
use octez_riscv::machine_state::block_cache::bcall::InterpretedBlockBuilder;
use octez_riscv::machine_state::csregisters::satp::Satp;
use octez_riscv::machine_state::csregisters::satp::SvLength;
use octez_riscv::machine_state::csregisters::satp::TranslationAlgorithm;
use octez_riscv::machine_state::memory;
use octez_riscv::machine_state::memory::Address;
use octez_riscv::machine_state::mode::Mode;
use octez_riscv::program::Program;
use octez_riscv::pvm::PvmHooks;
use octez_riscv::state_backend::ManagerReadWrite;
use octez_riscv::stepper::StepResult;
use octez_riscv::stepper::Stepper;
use octez_riscv::stepper::StepperStatus;
use octez_riscv::stepper::pvm::PvmStepper;
use octez_riscv::stepper::test::TestStepper;
use ratatui::prelude::*;
use ratatui::style::palette::tailwind;
use ratatui::widgets::*;
use rustc_demangle::demangle;
use tezos_smart_rollup::utils::inbox::Inbox;

use super::errors;
use super::tui;
use crate::commands::debug::DebugOptions;
mod render;
mod updates;

const GREEN: Color = tailwind::GREEN.c400;
const YELLOW: Color = tailwind::YELLOW.c400;
const RED: Color = tailwind::RED.c500;
const BLUE: Color = tailwind::BLUE.c400;
const ORANGE: Color = tailwind::ORANGE.c500;
const GRAY: Color = tailwind::GRAY.c500;
const SELECTED_STYLE_FG: Color = BLUE;
const NEXT_STYLE_FG: Color = GREEN;
const PC_CONTEXT: u64 = 12;

#[derive(Debug, Clone)]
pub struct Instruction {
    address: u64,
    pub text: String,
    jump: Option<(u64, Option<String>)>,
}

impl Instruction {
    fn new(address: u64, text: String, symbols: &HashMap<u64, Cow<str>>) -> Self {
        let jump = match text
            .split(' ')
            .next()
            .expect("Unexpected instruction format")
        {
            "jal" | "beq" | "bne" | "blt" | "bge" | "bltu" | "bgeu" => {
                text.split(',').last().map(|jump_address| {
                    let addr = address.wrapping_add(jump_address.parse::<i64>().unwrap() as u64);
                    (addr, symbols.get(&addr).map(|s| s.to_string()))
                })
            }
            _ => None,
        };
        Self {
            address,
            text,
            jump,
        }
    }
}

enum EffectiveTranslationState {
    Off,
    On,
    Faulting,
}

impl EffectiveTranslationState {
    pub fn text(&self) -> &'static str {
        match self {
            Self::Off => "Off",
            Self::On => "On",
            Self::Faulting => "Faulting",
        }
    }

    pub fn fg(&self) -> Color {
        match self {
            Self::Off => GRAY,
            Self::On => GREEN,
            Self::Faulting => RED,
        }
    }
}

enum SATPModeState {
    // Translation is BARE mode
    Bare,
    // Translation is SvXY mode
    Sv(SvLength),
}

struct TranslationState {
    mode: SATPModeState,
    base: Address,
    effective: EffectiveTranslationState,
}

impl TranslationState {
    pub(super) fn update(
        &mut self,
        faulting: bool,
        effective_mode: TranslationAlgorithm,
        satp_val: Satp,
    ) {
        self.effective = if faulting {
            EffectiveTranslationState::Faulting
        } else {
            match effective_mode {
                Bare => EffectiveTranslationState::Off,
                Sv39 | Sv48 | Sv57 => EffectiveTranslationState::On,
            }
        };

        self.base = satp_val.ppn().to_bits();

        use TranslationAlgorithm::*;
        self.mode = match satp_val.mode() {
            Bare => SATPModeState::Bare,
            Sv39 => SATPModeState::Sv(SvLength::Sv39),
            Sv48 => SATPModeState::Sv(SvLength::Sv48),
            Sv57 => SATPModeState::Sv(SvLength::Sv57),
        }
    }
}

struct DebuggerState {
    pub result: StepperStatus,
    pub prev_pc: Address,
    pub translation: TranslationState,
}

struct ProgramView<'a> {
    state: ListState,
    instructions: Vec<Instruction>,
    next_instr: usize,
    breakpoints: HashSet<u64>,
    symbols: HashMap<u64, Cow<'a, str>>,
}

pub struct DebuggerApp<'a, S: Stepper> {
    title: &'a str,
    stepper: &'a mut S,
    program: ProgramView<'a>,
    state: DebuggerState,
    max_steps: Option<usize>,
}

fn get_elf_symbols(
    contents: &[u8],
    demangle_symbols: bool,
) -> Result<HashMap<u64, Cow<str>>, Error> {
    let mut symbols = HashMap::new();
    let elf = Elf::parse(contents)?;

    let offset = if elf.header.e_type == ET_DYN {
        // Symbol addresses in relocatable executables are relative addresses. We need to offset
        // them by the start address of the main memory where the executable is loaded.
        memory::FIRST_ADDRESS
    } else {
        0
    };

    for symbol in elf.syms.iter() {
        let name = Cow::Borrowed(elf.strtab.get_at(symbol.st_name).expect("Symbol not found"));
        if !name.is_empty()
            && u32::try_from(symbol.st_shndx).expect("Symbol not valid address")
                != elf::section_header::SHN_UNDEF
        {
            if demangle_symbols {
                let demangled = demangle(&name).to_string();
                symbols.insert(symbol.st_value + offset, Cow::Owned(demangled));
            } else {
                symbols.insert(symbol.st_value + offset, name);
            }
        }
    }
    Ok(symbols)
}

impl<'a, MC: memory::MemoryConfig> DebuggerApp<'a, TestStepper<MC>> {
    pub fn launch(
        fname: &str,
        program: &[u8],
        initrd: Option<&[u8]>,
        exit_mode: Mode,
        demangle_sybols: bool,
        max_steps: Option<usize>,
    ) -> Result<()> {
        let block_builder = InterpretedBlockBuilder;

        let (mut interpreter, prog) =
            TestStepper::<MC>::new_with_parsed_program(program, initrd, exit_mode, block_builder)?;
        let symbols = get_elf_symbols(program, demangle_sybols)?;
        errors::install_hooks()?;
        let terminal = tui::init()?;
        DebuggerApp::new(&mut interpreter, fname, &prog, symbols, max_steps)
            .run_debugger(terminal)?;
        tui::restore()?;
        Ok(())
    }
}

impl<'hooks, MC: memory::MemoryConfig, CL: CacheLayouts>
    DebuggerApp<'_, PvmStepper<'hooks, MC, CL>>
{
    /// Launch the Debugger app for a PVM.
    pub fn launch(
        fname: &str,
        program: &[u8],
        initrd: Option<&[u8]>,
        inbox: Inbox,
        rollup_address: [u8; 20],
        opts: &DebugOptions,
    ) -> Result<()> {
        let hooks = PvmHooks::new(|_| {});
        let block_builder = InterpretedBlockBuilder;

        let mut stepper = PvmStepper::<'_, MC, CL>::new(
            program,
            initrd,
            inbox,
            hooks,
            rollup_address,
            opts.common.inbox.origination_level,
            block_builder,
        )?;

        let symbols = get_elf_symbols(program, opts.demangle)?;
        let program = Program::<MC>::from_elf(program)?.parsed();

        errors::install_hooks()?;
        let terminal = tui::init()?;
        DebuggerApp::new(
            &mut stepper,
            fname,
            &program,
            symbols,
            opts.common.max_steps,
        )
        .run_debugger(terminal)?;

        tui::restore()?;
        Ok(())
    }
}

impl<'a, S> DebuggerApp<'a, S>
where
    S: Stepper,
{
    fn new(
        stepper: &'a mut S,
        title: &'a str,
        program: &'a BTreeMap<u64, String>,
        symbols: HashMap<u64, Cow<'a, str>>,
        max_steps: Option<usize>,
    ) -> Self {
        Self {
            title,
            stepper,
            program: ProgramView::with_items(
                program
                    .iter()
                    .map(|x| Instruction::new(*x.0, x.1.to_string(), &symbols))
                    .collect::<Vec<Instruction>>(),
                symbols,
            ),
            state: DebuggerState {
                result: StepperStatus::default(),
                prev_pc: 0,
                translation: TranslationState {
                    mode: SATPModeState::Bare,
                    base: 0,
                    effective: EffectiveTranslationState::Off,
                },
            },
            max_steps,
        }
    }

    fn run_debugger(&mut self, mut terminal: Terminal<impl Backend>) -> Result<()>
    where
        S::Manager: ManagerReadWrite,
    {
        loop {
            self.draw(&mut terminal)?;
            if let Event::Key(key) = event::read()? {
                if key.kind == KeyEventKind::Press {
                    use KeyCode::*;
                    match key.code {
                        Char('q') | Esc => return Ok(()),
                        Char('s') => self.step(1),
                        Char('b') => self.program.set_breakpoint(),
                        Char('r') => self.step_until_breakpoint(),
                        Char('n') => self.step_until_next_symbol(),
                        Char('j') | Down => {
                            self.program.next();
                            self.update_selected_context();
                        }
                        Char('k') | Up => {
                            self.program.previous();
                            self.update_selected_context();
                        }
                        Char('g') | Home => {
                            self.program.go_top();
                            self.update_selected_context();
                        }
                        Char('G') | End => {
                            self.program.go_bottom();
                            self.update_selected_context();
                        }
                        _ => {}
                    }
                }
            }
        }
    }

    fn draw(&mut self, terminal: &mut Terminal<impl Backend>) -> Result<()>
    where
        S::Manager: ManagerReadWrite,
    {
        terminal.draw(|f| f.render_widget(self, f.size()))?;
        Ok(())
    }

    fn step(&mut self, max_steps: usize)
    where
        S::Manager: ManagerReadWrite,
    {
        let result = self
            .stepper
            .step_max(Bound::Included(max_steps))
            .to_stepper_status();
        self.update_after_step(result);
    }

    fn step_until_breakpoint(&mut self)
    where
        S::Manager: ManagerReadWrite,
    {
        // perform at least a step to progress if already on a breakpoint
        let mut result = self
            .stepper
            .step_max(Bound::Included(1))
            .to_stepper_status();

        // usize::MAX is used to represent an infinite number of steps as users will quit well before this.
        let max_steps = self.max_steps.unwrap_or(usize::MAX);

        let should_continue = |machine: &MachineCoreState<_, _>| {
            let raw_pc = machine.hart.pc.read();
            let pc = machine
                .translate_without_cache(raw_pc, AccessType::Instruction)
                .unwrap_or(raw_pc);
            !self.program.breakpoints.contains(&pc)
        };

        while should_continue(self.stepper.machine_state())
            && matches!(result, StepperStatus::Running {steps, .. } if steps < max_steps)
        {
            result += self
                .stepper
                .step_max(Bound::Included(1))
                .to_stepper_status();
        }

        self.update_after_step(result);
    }

    fn step_until_next_symbol(&mut self)
    where
        S::Manager: ManagerReadWrite,
    {
        // perform at least a step to progress if already on a breakpoint/symbol
        let mut result = self
            .stepper
            .step_max(Bound::Included(1))
            .to_stepper_status();

        // usize::MAX is used to represent an infinite number of steps as users will quit well before this.
        let max_steps = self.max_steps.unwrap_or(usize::MAX);

        let should_continue = |machine: &MachineCoreState<_, _>| {
            let raw_pc = machine.hart.pc.read();
            let pc = machine
                .translate_without_cache(raw_pc, AccessType::Instruction)
                .unwrap_or(raw_pc);

            !(self.program.breakpoints.contains(&pc) || self.program.symbols.contains_key(&pc))
        };

        while should_continue(self.stepper.machine_state())
            && matches!(result, StepperStatus::Running { steps, .. } if steps < max_steps)
        {
            result += self
                .stepper
                .step_max(Bound::Included(1))
                .to_stepper_status();
        }

        self.update_after_step(result);
    }
}

impl<'a> ProgramView<'a> {
    fn with_items(
        instructions: Vec<Instruction>,
        symbols: HashMap<u64, Cow<'a, str>>,
    ) -> ProgramView<'a> {
        ProgramView {
            state: ListState::default().with_selected(Some(0)),
            instructions,
            next_instr: 0,
            breakpoints: HashSet::new(),
            symbols,
        }
    }

    fn set_breakpoint(&mut self) {
        let instr = self.state.selected().unwrap_or(self.next_instr);
        let address = self.instructions[instr].address;
        if self.breakpoints.contains(&address) {
            self.breakpoints.remove(&address);
        } else {
            self.breakpoints.insert(address);
        }
    }

    fn next(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i >= self.instructions.len() - 1 {
                    0
                } else {
                    i + 1
                }
            }
            None => self.next_instr,
        };
        self.state.select(Some(i));
    }

    fn previous(&mut self) {
        let i = match self.state.selected() {
            Some(i) => {
                if i == 0 {
                    self.instructions.len() - 1
                } else {
                    i - 1
                }
            }
            None => self.next_instr,
        };
        self.state.select(Some(i));
    }

    fn go_top(&mut self) {
        self.state.select(Some(0));
    }

    fn go_bottom(&mut self) {
        self.state.select(Some(self.instructions.len() - 1));
    }

    pub fn partial_update(&mut self, mut new_instructions: Vec<Instruction>) {
        // Update / Insert new_instructions to existing instructions.
        self.instructions.retain(|i| {
            new_instructions
                .iter()
                .all(|new_i| i.address != new_i.address)
        });
        self.instructions.append(&mut new_instructions);
        self.instructions.sort_by(|a, b| a.address.cmp(&b.address));
    }
}

#[cfg(test)]
mod test {
    use std::fs;

    use octez_riscv::machine_state::memory::M1G;

    use super::*;
    use crate::ExitMode;
    use crate::posix_exit_mode;

    #[test]
    fn test_max_steps_respected() {
        let progpath = "../assets/hermit-loader";
        let program = match fs::read(progpath) {
            Ok(data) => data,
            Err(e) => panic!("Failed to read program file: {}", e),
        };

        let block_builder = InterpretedBlockBuilder;

        let (mut interpreter, prog) = TestStepper::<M1G>::new_with_parsed_program(
            program.as_slice(),
            None,
            posix_exit_mode(&ExitMode::User),
            block_builder,
        )
        .unwrap();

        let symbols: HashMap<u64, Cow<'_, str>> = HashMap::new();

        let maxstep = 10;
        let mut debugger =
            DebuggerApp::new(&mut interpreter, progpath, &prog, symbols, Some(maxstep));
        debugger
            .program
            .breakpoints
            .insert(debugger.program.instructions[1].address);

        debugger.step_until_breakpoint();
        assert!(matches!(debugger.state.result, StepperStatus::Running {
            steps: 1,
            ..
        }));

        debugger.step_until_breakpoint();
        assert_eq!(maxstep, debugger.state.result.steps())
    }
}
