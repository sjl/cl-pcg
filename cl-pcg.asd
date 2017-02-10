(asdf:defsystem :cl-pcg
  :description "A bare-bones Permuted Congruential Generator implementation in pure Common Lisp."

  :author "Steve Losh <steve@stevelosh.com>"

  :license "MIT/X11"
  :version "1.0.0"

  :depends-on (#+sbcl :sb-rotate-byte)

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "pcg")))))
