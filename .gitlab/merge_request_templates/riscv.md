<!--
    Link Linear issues using magic words. Examples of these are "Closes RV-XXX", "Part of RV-YYY"
    or "Relates to RV-ZZZ".
-->

# What

<!--
    Summarise the changes in this MR.
-->

# Why

<!--
    Explain why this MR is needed.
-->

# How

<!--
    Explain how the MR achieves its goal. If this is trivial, you may omit it.
-->

# Manually Testing

```
make -C src/riscv all
```

# Tasks for the Author

- [ ] Link all Linear issues related to this MR using magic words (e.g. part of, relates to, closes).
- [ ] Eliminate dead code and other spurious artefacts introduced in your changes.
- [ ] Document new public functions, methods and types.
- [ ] Make sure the documentation for updated functions, methods, and types is correct.
- [ ] Add tests for bugs that have been fixed.
- [ ] Put in reasonable effort to ensure that CI will pass.
  - `make -C src/riscv`
  - `dune test src/lib_riscv`
  - `dune build src/rust_deps`
- [ ] If applicable, trigger the `tezt-riscv-slow-sequential` test job.
- [ ] Write commit messages to reflect the changes they're about.
- [ ] Self-review your changes to ensure they are high-quality.
- [ ] Complete all of the above before assigning this MR to reviewers.

/label ~riscv
/draft
/assign me

<!--
    Once the MR is ready, run the following GitLab commands.
-->

```
/assign @ole.kruger
/assign @victor-dumitrescu
/assign @felix.puscasu1
/assign @emturner
/assign @santnr
/assign @hantang.sun
/assign @kurtis.charnock

/assign_reviewer @ole.kruger
/assign_reviewer @victor-dumitrescu
/assign_reviewer @felix.puscasu1
/assign_reviewer @emturner
/assign_reviewer @santnr
/assign_reviewer @hantang.sun
/assign_reviewer @jobjo
/assign_reviewer @kurtis.charnock

/unassign me
/unassign_reviewer me

/ready
```
