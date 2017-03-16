Usage
=====

`cl-pcg` is a [permuted congruential generator][pcg] implementation in pure
Common Lisp.  It provides a high-level API and a low-level API.

[pcg]: http://www.pcg-random.org/

[TOC]

The High-Level API
------------------

The high-level API is what you should start (and probably end) with.  It
typechecks the arguments you pass in and offers a nice interface for generating
random numbers.

### Creating a Generator

To create a generator you can use the `make-pcg` function:

    :::lisp
    (defparameter *gen* (make-pcg))

`make-pcg` takes two keyword parameters:

* `:seed` should be an `(unsigned-byte 64)`.  If omitted a random seed will be
  generated with the underlying implementation's `cl:random` function.
* `:stream-id` should be an `(unsigned-byte 32)`.  The default is `0`.  Streams
  provide a way to "split" a PCG into multiple generators — check out the PCG
  site for more information on this.

Once you've got a PCG object you can use it to generate some numbers.

### Generating Numbers

You can use the `pcg-random` function to generate random numbers:

    :::lisp
    (defparameter *gen* (make-pcg))

    (pcg-random *gen* 10)
    ; => a random number from 0 (inclusive) to 10 (exclusive)

`pcg-random` is flexible and takes a number of optional arguments to help you
generate the kinds of numbers you need.  Its lambda list looks like this:

    :::lisp
    (pcg bound &optional max inclusive?)

If only `bound` is given, the function acts much like `cl:random`.

If `max` is also given, a random number in `[bound, max)` is chosen.

If `inclusive?` is also given, a random number in `[bound, max]` is chosen.

For example:

    :::lisp
    (defparameter *gen* (make-pcg))

    (pcg-random *gen* 10)      ; => [0, 10)
    (pcg-random *gen* 15 28)   ; => [15, 28)
    (pcg-random *gen* 15 28 t) ; => [15, 28] <- inclusive endpoint!

`pcg-random` can also generate `single-float`s if `bound` and `max` are given as
`single-float`s.

### The Global Generator

If you don't want to bother creating a fresh PCG object you can pass `t` to the
high-level API to use a globally-defined one:

    :::lisp
    (pcg-random t 10)

### Advancing & Rewinding

Sometimes it can be useful to advance or rewind a generator by a certain number
of steps.  The `(pcg-advance pcg steps)` and `(pcg-rewind pcg steps)` can be
used to do this:

    :::lisp
    (defparameter *gen* (make-pcg))

    ;; Get three numbers
    (pcg-random *gen* 1000) ; => 708
    (pcg-random *gen* 1000) ; => 964
    (pcg-random *gen* 1000) ; => 400

    ;; Rewind three steps
    (pcg-rewind *gen* 3)

    ;; Get the same three numbers
    (pcg-random *gen* 1000) ; => 708
    (pcg-random *gen* 1000) ; => 964
    (pcg-random *gen* 1000) ; => 400

These functions are `O(log₂(steps))` so they'll be fast even for ludicrously
large values of `steps`.


The Low-Level API
-----------------

The low-level API is what you want if you need raw speed.  It consists of all
functions in the API whose names end in `%`, like `pcg-random%`.  All of these
functions are `declaim`ed inline for easy embedding into hot loops.

As an arbitrary example, the main function in this API (`pcg-random%`) is about
100 bytes of machine code, so it's suitable for inlining when you really need
performance.

The low-level API assumes you will pass in arguments of the correct type.  If
you fuck this up, all bets are off.  Read the code to figure out exactly what
you need to pass in (or just use the high-level API like a sane person).
