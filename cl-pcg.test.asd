(asdf:defsystem :cl-pcg.test
  :description "Test suite for cl-pcg"

  :author "Steve Losh <steve@stevelosh.com>"
  :license "MIT"

  :depends-on (:cl-pcg :1am)

  :serial t
  :components ((:file "package.test")
               (:module "test"
                :serial t
                :components ((:file "tests"))))

  :perform (asdf:test-op
             (op system)
             (uiop:symbol-call :pcg.test :run-tests)))
