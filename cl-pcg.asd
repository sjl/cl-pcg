(asdf:defsystem :cl-pcg
  :description "A bare-bones Permuted Congruential Generator implementation in pure Common Lisp."
  :author "Steve Losh <steve@stevelosh.com>"
  :homepage "https://docs.stevelosh.com/cl-pcg/"

  :license "MIT"
  :version "1.0.0"

  :depends-on (#+sbcl :sb-rotate-byte)

  :in-order-to ((asdf:test-op (asdf:test-op :cl-pcg.test)))

  :serial t
  :components ((:module "vendor" :serial t
                :components ((:file "quickutils-package")
                             (:file "quickutils")))
               (:file "package")
               (:module "src" :serial t
                :components ((:file "pcg")))))
