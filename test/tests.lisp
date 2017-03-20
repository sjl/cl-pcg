(in-package :pcg.test)


;;;; Utils --------------------------------------------------------------------
(defmacro define-test (name &body body)
  `(test ,(symb 'test- name)
    (let ((*package* ,*package*))
      ,@body)))

(defmacro gimme (n &body body)
  `(loop :repeat ,n :collect (progn ,@body)))


(defun run-tests ()
  (1am:run))


;;;; Tests --------------------------------------------------------------------
(define-test rewind
  (let ((g (make-pcg))
        (a nil)
        (b nil))
    (setf a (gimme 20 (pcg-random g 50000)))
    (pcg-rewind g 20) ; Rewind to start
    (setf b (gimme 20 (pcg-random g 50000)))
    (is (equal a b))
    (setf a (gimme 10 (pcg-random g 50000)))
    (pcg-rewind g 10) ; Rewind back to midpoint
    (setf b (gimme 10 (pcg-random g 50000)))
    (is (equal a b))
    ))
