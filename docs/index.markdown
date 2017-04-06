`cl-pcg` is a [permuted congruential generator][pcg] implementation in pure
Common Lisp.

Permuted congruential generators are seedable, small, fast,
fairly-hard-to-predict random number generators.  They can be useful for things
like games.  They can also be advanced and rewound efficiently.

PCGs are **not** cryptographically secure.  If you need that, look elsewhere.

[pcg]: http://www.pcg-random.org/

* **License:** MIT
* **Documentation:** <https://sjl.bitbucket.io/cl-pcg/>
* **Mercurial:** <https://bitbucket.org/sjl/cl-pcg/>
* **Git:** <https://github.com/sjl/cl-pcg/>
