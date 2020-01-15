`cl-pcg` is a [permuted congruential generator][pcg] implementation in pure
Common Lisp.

Permuted congruential generators are seedable, small, fast,
fairly-hard-to-predict random number generators.  They can be useful for things
like games.  They can also be advanced and rewound efficiently.

PCGs are **not** cryptographically secure.  If you need that, look elsewhere.

[pcg]: http://www.pcg-random.org/

* **License:** MIT
* **Documentation:** <https://docs.stevelosh.com/cl-pcg/>
* **Mercurial:** <https://hg.stevelosh.com/cl-pcg/>
* **Git:** <https://github.com/sjl/cl-pcg/>

Testing with Dieharder
----------------------

There's a Roswell script you can use to make a little executable that will spew
random bytes to stdout, suitable for piping into `dieharder`:

```
make build/pcg
./build/pcg | dieharder -a -g 200
```

`build/pcg` will dump out infinite random bytes until stdout breaks, so maybe
don't run it in a bare terminal unless you want to just totally hose it.
