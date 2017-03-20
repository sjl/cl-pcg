#+ecl (setf compiler:*user-cc-flags* "-Wno-shift-negative-value")

(ql:quickload 'cl-pcg)
(time (asdf:test-system 'cl-pcg))
(quit)
