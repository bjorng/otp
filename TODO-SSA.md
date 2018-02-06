TO DO
=====

## beam_kernel_to_ssa ##

* Rewrite second clause in `protected_cg/6`.

* Implement an `invoke` instruction to call functions
  and BIFs within the scope of a try/catch or catch.

* Fix bugs so that the compiler test suite can be compile.

* Rip out register and stack state handling.

## beam_ssa_codegen ##

* Implement everything.

## Needed optimizations ##

Some optimizations that are in v3_codegen are not included
in beam_kernel_to_ssa. To avoid producing worse code, the
following optimizations must be done in some optimization
pass run before beam_ssa_codegen.

* In binary mapping, if a matched out segment is not used,
replace the bs_get* instruction with a bs_skip_* instruction,
test_unit instruction, or nothing (if skipping the tail of
1-bit aligned bitstring).

* Perhaps optimizations of size calculation for binary
construction is needed.

* Optimize map matching. If an extracted element is not used,
it is enough to test that the field exists. Also combine
several `get_map_element` instructions with literal keys
to a single `get_map_elements` instruction.
