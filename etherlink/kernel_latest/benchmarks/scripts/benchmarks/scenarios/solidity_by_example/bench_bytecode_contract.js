const utils = require('../../utils');
const { contracts_directory, compile_contract_file } = require("../../../lib/contract");
let faucet = require('../../players/faucet.json');

const path = require('path');
const scenario_type = "solidity_by_example"

let contract = compile_contract_file(path.join(contracts_directory, scenario_type), "bytecode_contract.sol")[0];
let create_data = contract.bytecode;
let player1 = require('../../players/player1.json');

let deploy = contract.interface.encodeFunctionData("deploy", []);

let txs = [];
txs.push(utils.transfer(faucet, player1, 100000000))
let create = utils.create(player1, 0, create_data)
txs.push(create.tx)

txs.push(utils.send(player1, create.addr, 0, deploy))

let mode = utils.bench_args(process.argv);

utils.print_bench([txs], mode)
