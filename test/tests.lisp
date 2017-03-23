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
(defparameter *iterations* 200)
(defparameter *run-length* 5000)


(define-test seeding
  (loop :repeat *iterations*
        :for seed = (random (expt 2 64))
        :for stream = (random (expt 2 32))
        :for a = (make-pcg :seed seed :stream-id stream)
        :for b = (make-pcg :seed seed :stream-id stream)
        :for a-vals = (gimme 200 (pcg-random a (1- (expt 2 32))))
        :for b-vals = (gimme 200 (pcg-random b (1- (expt 2 32))))
        :do (is (equal a-vals b-vals))))

(define-test rewind
  (loop :repeat *iterations*
        :do (let ((g (make-pcg))
                  (a nil)
                  (b nil)
                  (bound (1+ (random 50000))))
              (setf a (gimme 20 (pcg-random g bound)))
              (pcg-rewind g 20) ; Rewind to start
              (setf b (gimme 20 (pcg-random g bound)))
              (is (equal a b))

              (setf a (gimme 10 (pcg-random g bound)))
              (pcg-rewind g 10) ; Rewind back to midpoint
              (setf b (gimme 10 (pcg-random g bound)))
              (is (equal a b)))))

(define-test upper-bound
  (loop :repeat *iterations*
        :for bound = (1+ (random 50))
        :for gen = (make-pcg)
        :for vals = (gimme *run-length* (pcg-random gen bound))
        :do (is (every (lambda (n) (<= 0 n (1- bound)))
                       vals))))

(define-test bounds-exclusive
  (loop :repeat *iterations*
        :for min = (- (random 50) 25)
        :for max = (+ min 1 (random 10))
        :for gen = (make-pcg)
        :for vals = (gimme *run-length* (pcg-random gen min max))
        :do (is (every (lambda (n) (<= min n (1- max)))
                       vals))))

(define-test bounds-inclusive
  (loop :repeat *iterations*
        :for min = (- (random 50) 25)
        :for max = (+ min 1 (random 10))
        :for gen = (make-pcg)
        :for vals = (gimme *run-length* (pcg-random gen min max t))
        :do (is (every (lambda (n) (<= min n max))
                       vals))))

(define-test bounds-floats
  (loop :repeat *iterations*
        :for min = (- (random 50.0) 25)
        :for max = (+ min 1 (random 10.0))
        :for gen = (make-pcg)
        :for vals = (gimme *run-length* (pcg-random gen min max))
        :do (is (every (lambda (n) (<= min n max))
                       vals))))

(define-test degenerate-bounds
  (loop :repeat *iterations*
        :for gen = (make-pcg)
        :for vals = (gimme *run-length* (pcg-random gen 1))
        :do (is (every #'zerop vals))))

(define-test type-coercion
  (let ((gen (make-pcg)))
    (is (floatp (pcg-random gen 1.0)))
    (is (floatp (pcg-random gen 0.0 1.0)))
    (is (floatp (pcg-random gen 0 1.0)))
    (is (floatp (pcg-random gen 0.0 1)))
    (is (integerp (pcg-random gen 10)))
    (is (integerp (pcg-random gen 0 10)))
    (is (integerp (pcg-random gen 0 10 t)))))
