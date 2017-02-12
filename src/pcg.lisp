(in-package :pcg)

;;;; Constants ----------------------------------------------------------------
(defconstant +multiplier+ 6364136223846793005)
(defconstant +modulus+ (expt 2 64))
(defconstant +limit+ (1- (expt 2 64)))


;;;; Utils ---------------------------------------------------------------------

(defmacro defun-inline (name &body body)
  "Like `defun`, but declaims `name` to be `inline`."
  `(progn
     (declaim (inline ,name))
     (defun ,name ,@body)
     ',name))

(defmacro check-types (&rest place-type-pairs)
  `(progn ,@(loop :for (place type . nil) :on place-type-pairs :by #'cddr
                  :collect `(check-type ,place ,type))))


(defun rotate-byte (count bytespec integer)
  "Rotates a field of bits within INTEGER; specifically, returns an
integer that contains the bits of INTEGER rotated COUNT times
leftwards within the byte specified by BYTESPEC, and elsewhere
contains the bits of INTEGER. See http://www.cliki.net/ROTATE-BYTE"
  (declare (optimize (speed 3) (safety 0) (space 0) (debug 1)))
  (let ((size (byte-size bytespec)))
    (when (= size 0)
      (return-from rotate-byte integer))
    (let ((count (mod count size)))
      (flet ((rotate-byte-from-0 (count size integer)
                 (let ((bytespec (byte size 0)))
                   (if (> count 0)
                       (logior (ldb bytespec (ash integer count))
                               (ldb bytespec (ash integer (- count size))))
                       (logior (ldb bytespec (ash integer count))
                               (ldb bytespec (ash integer (+ count size))))))))
        (dpb (rotate-byte-from-0 count size (ldb bytespec integer))
             bytespec
             integer)))))


(defun-inline % (n)
  (mod n +modulus+))

(defun-inline *+ (a b increment)
  (+ (* a b) increment))


(deftype u64 ()
  '(unsigned-byte 64))


;;;; Data ---------------------------------------------------------------------
(defstruct (pcg (:constructor actually-make-pcg))
  (state 0 :type u64)
  (increment 0 :type u64))

(defun-inline compute-increment (stream-id)
  (logior 1 (ash stream-id 1)))


;;;; Permutations -------------------------------------------------------------
(defun-inline permute-xor-shift (data)
  (declare (optimize speed)
           (type (unsigned-byte 37) data))
  ;; The reference implemtation does this:
  ;;
  ;;   uint32_t xorshifted = ((oldstate >> 18u) ^ oldstate) >> 27u;
  ;;
  ;; Which is a bunch of shit packed into one line because C programmers don't
  ;; like typing or something.
  ;;
  ;; oldstate starts as a 64-bit value, which looks roughly like this:
  ;;
  ;;   SSSSSbbb bbbbbbbb bbbbbbbb bbbbbbbb bbbbbxxx xxxxxxxx xxxxxxxx xxxxxxxx
  ;;
  ;; * S - 5 "selector" bits for the permutation.  Confusingly the xorshift
  ;;       permutation doesn't use these to select anything, but instead mixes
  ;;       them into the other random bits.  The next permutation uses them to
  ;;       decide how far to rotate.
  ;; * b - 32 decently random bits, which are going to be permuted and used for
  ;;       the output value.
  ;; * x - 27 not-very-random garbage low-order bits that we throw away.
  ;;
  ;; What this permutation does is xor the top half of the good bits into the
  ;; bottom half of the good bits to mix them up a bit:
  ;;
  ;;   oldstate:       SSSSSbbb bbbbbbbb bbbbbbbb bbbbbbbb bbbbbxxx xxxxxxxx xxxxxxxx xxxxxxxx
  ;;   oldstate >> 18: 00000000 00000000 00SSSSSb bbbbbbbb bbbbbbbb bbbbbbbb bbbbbbxx xxxxxxxx ...lost...
  ;;   result:         SSSSSbbb bbbbbbbb bbBBBBBB BBBBBBBB BBBBBxxx xxxxxxxx xxxxxxxx xxxxxxxx
  ;;
  ;; Then it shifts by 27 to drop the garbage bits:
  ;;
  ;;   SSSSS bbbbbbbb bbbbbBBB BBBBBBBB BBBBBBBB
  ;;
  ;; And finally it assigns the resulting 37-bit value to a uint32_t which
  ;; clears out the 5 remaining high-order selector bits.
  (ldb (byte 32 0)
       (logxor data (ash data -18))))

(defun-inline permute-rotate (data selector)
  (declare (optimize speed)
           (type (unsigned-byte 32) data)
           (type (unsigned-byte 5) selector))
  #+sbcl (sb-rotate-byte:rotate-byte selector (byte 32 0) data)
  #-sbcl (rotate-byte selector (byte 32 0) data))


;;;; State Advancement --------------------------------------------------------
(defun-inline advance-state (pcg)
  (declare (optimize speed)
           (type pcg pcg))
  (setf (pcg-state pcg)
        (% (*+ (pcg-state pcg) +multiplier+ (pcg-increment pcg))))
  nil)


;;;; Low-Level API ------------------------------------------------------------
(declaim
  (ftype (function (pcg) (unsigned-byte 32)) pcg-random%)

  ;; 2^32 streams ought to be enough for anybody
  (ftype (function (u64 (unsigned-byte 32))
                   pcg)
         make-pcg%)

  (ftype (function (pcg (integer 1 (#.(expt 2 32))))
                   (unsigned-byte 32))
         pcg-random-bounded%)

  (ftype (function (pcg (integer 1 32))
                   (unsigned-byte 32))
         pcg-random-bits%)

  (ftype (function (pcg u64)) pcg-advance% pcg-rewind%)

  (ftype (function (pcg) single-float) pcg-random-float%)

  (ftype (function (pcg) double-float) pcg-random-double%))


(defun-inline pcg-random% (pcg)
  "Return a random `(unsigned-byte 32)`.

  As a side effect, the state of `pcg` will be advanced.

  This is a low-level function that assumes you are passing in the correct types.

  "
  (declare (optimize speed))
  (let* ((state (pcg-state pcg))
         (data (ldb (byte 37 (- 64 37)) state))
         (selector (ldb (byte 5 (- 64 5)) state)))
    (advance-state pcg)
    (permute-rotate (permute-xor-shift data)
                    selector)))


(defun-inline make-pcg% (seed stream-id)
  "Create and return a new `pcg` for the given `seed` and `stream-id`.

  This is a low-level function that assumes you are passing in the correct types.

  "
  (declare (optimize speed))
  (let* ((increment (compute-increment stream-id))
         (pcg (actually-make-pcg :state 0 :increment increment)))
    (pcg-random% pcg)
    (setf (pcg-state pcg)
          (% (+ (pcg-state pcg) seed)))
    (pcg-random% pcg)
    pcg))


(defun-inline pcg-random-bounded% (pcg bound)
  "Return a random integer between `0` (inclusive) and `bound` (exclusive).

  As a side effect, the state of `pcg` will be advanced.

  This is a low-level function that assumes you are passing in the correct types.

  "
  (declare (optimize speed))
  (loop :with threshold = (mod (expt 2 32) bound)
        :for n = (pcg-random% pcg)
        :when (>= n threshold)
        :do (return (values (mod n bound)))))

(defun-inline pcg-random-bits% (pcg count)
  "Return a random `(unsigned-byte COUNT)`.

  As a side effect, the state of `pcg` will be advanced.

  `count` must be between `1` and `32` (though `32` would be identical to just
  calling `pcg-random%`).

  This is a low-level function that assumes you are passing in the correct types.

  "
  (declare (optimize speed))
  (ldb (byte count 0) (pcg-random% pcg)))

(defun-inline pcg-random-float% (pcg)
  "Return a random `single-float` between `0.0` and `1.0`.

  As a side effect, the state of `pcg` will be advanced.

  This is a low-level function that assumes you are passing in the correct types.

  "
  (declare (optimize speed))
  (/ (pcg-random% pcg)
     (coerce (expt 2 32) 'single-float)))


(defun-inline pcg-advance% (pcg steps)
  (declare (optimize speed))
  ;; See "Random Number Generation with Arbitrary Strides", Forrest B. Brown:
  ;; https://laws.lanl.gov/vhosts/mcnp.lanl.gov/pdf_files/anl-rn-arb-stride.pdf
  ;;
  ;; The original paper uses single-letter names like G, C, i, f, h because
  ;; math, but we're all literate here so let's use words instead.
  (loop
    :with increment = (pcg-increment pcg)
    :with multiplier-accumulator :of-type u64 = 1 ; G
    :with increment-accumulator :of-type u64 = 0 ; C
    :for i :of-type u64 = steps :then (ash i -1) ; i
    :for inc :of-type u64 = increment :then (% (* inc (1+ mul))) ; f
    :for mul :of-type u64 = +multiplier+ :then (% (expt mul 2)) ; h
    :while (plusp i)
    :when (oddp i)
    :do (setf multiplier-accumulator (% (* multiplier-accumulator mul))
              increment-accumulator (% (*+ increment-accumulator mul inc)))
    :finally
    (setf (pcg-state pcg)
          (% (*+ (pcg-state pcg) multiplier-accumulator increment-accumulator)))))

(defun-inline pcg-rewind% (pcg steps)
  (declare (optimize speed))
  (when (plusp steps)
    (pcg-advance% pcg (- +limit+ (1- steps)))))


;;;; High-Level API -----------------------------------------------------------
(defun make-pcg (&key (seed 0) (stream-id 0))
  (check-types stream-id (unsigned-byte 32)
               seed u64)
  (make-pcg% seed stream-id))


(defparameter *global-pcg* (make-pcg))

(deftype pcg-designator ()
  '(or (eql t) pcg))

(defun-inline resolve-pcg (pcg-designator)
  (if (eq t pcg-designator)
    *global-pcg*
    pcg-designator))


(defun pcg-random (pcg)
  (check-types pcg pcg-designator)
  (pcg-random% (resolve-pcg pcg)))

(defun pcg-random-float (pcg)
  (check-types pcg pcg-designator)
  (pcg-random-float% (resolve-pcg pcg)))

(defun pcg-random-double (pcg)
  (check-types pcg pcg-designator)
  (pcg-random-double% (resolve-pcg pcg)))

(defun pcg-random-bounded (pcg bound)
  (check-types pcg pcg-designator
               bound (and (unsigned-byte 32)
                          (integer 1)))
  (pcg-random-bounded% (resolve-pcg pcg) bound))

(defun pcg-random-range (pcg min max)
  (check-types pcg pcg-designator
               min (unsigned-byte 32)
               max (unsigned-byte 32))
  (assert (< min max) (min max))
  (+ min (pcg-random-bounded% (resolve-pcg pcg) (- max min))))

(defun pcg-random-range-inclusive (pcg min max)
  (check-types pcg pcg-designator
               min (unsigned-byte 32)
               max (unsigned-byte 32))
  (assert (<= min max) (min max))
  (pcg-random-range (resolve-pcg pcg) min (1+ max)))

(defun pcg-advance (pcg steps)
  (check-types pcg pcg-designator
              steps u64)
  (pcg-advance% (resolve-pcg pcg) steps))

(defun pcg-rewind (pcg steps)
  (check-types pcg pcg-designator
               steps u64)
  (pcg-rewind% (resolve-pcg pcg) steps))


;;;; Scratch ------------------------------------------------------------------
; (defun data (n)
;   (loop :repeat n :collect (pcg-random-single-float% *p*)))

; (losh:gnuplot
;   (data 10000)
;   :x #'identity
;   :y (lambda (i) (/ 1.0 10000))
;   :line-width 1
;   :smooth :cumulative)

; (defun slow-advance (pcg n)
;   (dotimes (_ n) (pcg-random pcg))
;   nil)

; (defun test (n)
;   (let ((a (make-pcg))
;         (b (make-pcg)))
;     (pcg-advance% b n)
;     (assert (= (pcg-random a) (pcg-random b)))))

;;;; TODO ----------------------------------------------------------------------
;;; https://experilous.com/1/blog/post/perfect-fast-random-floating-point-numbers
;;; as an alternative float generation scheme.


