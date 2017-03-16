(defpackage :pcg
  (:use :cl :pcg.quickutils)
  (:export
    ;; High-Level API
    :pcg
    :make-pcg
    :pcg-random
    :pcg-advance
    :pcg-rewind

    ;; Low-Level API
    :make-pcg%
    :pcg-random%
    :pcg-random-bounded%
    :pcg-random-float%
    :pcg-advance%
    :pcg-rewind%

    ))
