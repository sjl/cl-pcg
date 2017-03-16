# API Reference

The following is a list of all user-facing parts of `cl-pcg`.

If there are backwards-incompatible changes to anything listed here, they will
be noted in the changelog and the author will feel bad.

Anything not listed here is subject to change at any time with no warning, so
don't touch it.

[TOC]

## Package `PCG`

### `MAKE-PCG` (function)

    (MAKE-PCG &KEY (SEED NIL) (STREAM-ID 0))

Create and return a new PCG.

  If `seed` is `nil`, a fresh random seed will be generated with the
  implementation's `cl:random` function, as if by:

    (random ... (make-random-state t))

  

### `MAKE-PCG%` (function)

    (MAKE-PCG% SEED STREAM-ID)

Create and return a new `pcg` for the given `seed` and `stream-id`.

  This is a low-level function that assumes you are passing in the correct types.

  

### `PCG` (struct)

Slots: `STATE`, `INCREMENT`

### `PCG-ADVANCE` (function)

    (PCG-ADVANCE PCG STEPS)

Advance the state of `pcg` by `steps` steps.

  This function returns `nil` and is only useful for its side effects.

  

### `PCG-ADVANCE%` (function)

    (PCG-ADVANCE% PCG STEPS)

Advance the state of `pcg` by `steps` steps.

  This function returns `nil` and is only useful for its side effects.

  This is a low-level function that assumes you are passing in the correct types.

  

### `PCG-RANDOM` (function)

    (PCG-RANDOM PCG BOUND &OPTIONAL MAX INCLUSIVE?)

### `PCG-RANDOM%` (function)

    (PCG-RANDOM% PCG)

Return a random `(unsigned-byte 32)`.

  As a side effect, the state of `pcg` will be advanced.

  This is a low-level function that assumes you are passing in the correct types.

  

### `PCG-RANDOM-BOUNDED%` (function)

    (PCG-RANDOM-BOUNDED% PCG BOUND)

Return a random integer between `0` (inclusive) and `bound` (exclusive).

  As a side effect, the state of `pcg` will be advanced.

  This is a low-level function that assumes you are passing in the correct types.

  

### `PCG-RANDOM-FLOAT%` (function)

    (PCG-RANDOM-FLOAT% PCG)

Return a random `single-float` between `0.0` and `1.0`.

  As a side effect, the state of `pcg` will be advanced.

  This is a low-level function that assumes you are passing in the correct types.

  

### `PCG-REWIND` (function)

    (PCG-REWIND PCG STEPS)

Rewind the state of `pcg` by `steps` steps.

  This function returns `nil` and is only useful for its side effects.

  

### `PCG-REWIND%` (function)

    (PCG-REWIND% PCG STEPS)

Rewind the state of `pcg` by `steps` steps.

  This function returns `nil` and is only useful for its side effects.

  This is a low-level function that assumes you are passing in the correct types.

  

