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

(deftype u32 ()
  '(unsigned-byte 32))


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
  ;; The reference implementation does this:
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
  ;;       permutation doesn't use these to SELECT anything, but instead mixes
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
  (ftype (function (pcg) u32) pcg-random%)

  ;; 2^32 streams ought to be enough for anybody
  (ftype (function (u64 u32)
                   pcg)
         make-pcg%)

  (ftype (function (pcg (and u32 (integer 1))) u32)
         pcg-random-bounded%)

  (ftype (function (pcg (signed-byte 32) (signed-byte 32))
                   (signed-byte 32))
         pcg-random-range%)

  (ftype (function (pcg u64)) pcg-advance% pcg-rewind%)

  (ftype (function (pcg) single-float) pcg-random-float%))


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

(defun-inline pcg-random-range% (pcg min max)
  (declare (optimize speed))
  (+ min (pcg-random-bounded% pcg (- max min))))


(defun-inline pcg-random-float% (pcg)
  "Return a random `single-float` between `0.0` and `1.0`.

  As a side effect, the state of `pcg` will be advanced.

  This is a low-level function that assumes you are passing in the correct types.

  "
  (declare (optimize speed))
  (/ (pcg-random% pcg)
     (coerce (expt 2 32) 'single-float)))


(defun-inline pcg-advance% (pcg steps)
  "Advance the state of `pcg` by `steps` steps.

  This function returns `nil` and is only useful for its side effects.

  This is a low-level function that assumes you are passing in the correct types.

  "
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
  "Rewind the state of `pcg` by `steps` steps.

  This function returns `nil` and is only useful for its side effects.

  This is a low-level function that assumes you are passing in the correct types.

  "
  (declare (optimize speed))
  (when (plusp steps)
    (pcg-advance% pcg (- +limit+ (1- steps)))))


;;;; High-Level API -----------------------------------------------------------
(defun resolve-seed (seed)
  (if (null seed)
    (random (expt 2 64) (make-random-state t))
    seed))

(defun make-pcg (&key (seed nil) (stream-id 0))
  "Create and return a new PCG.

  If `seed` is `nil`, a fresh random seed will be generated with the
  implementation's `cl:random` function, as if by:

    (random ... (make-random-state t))

  "
  (check-types stream-id u32
               seed (or null u64))
  (make-pcg% (resolve-seed seed) stream-id))


(defparameter *global-pcg* (make-pcg))

(deftype pcg-designator ()
  '(or (eql t) pcg))

(defun-inline resolve-pcg (pcg-designator)
  (if (eq t pcg-designator)
    *global-pcg*
    pcg-designator))


(defun pcg-random-integer (pcg min max)
  "Return a random integer.

  As a side effect, the state of `pcg` will be advanced.

  "
  (check-types pcg pcg-designator
               min integer
               max integer)
  (+ min (pcg-random-bounded% (resolve-pcg pcg)
                              (+ (- max min)))))

(defun pcg-random-float (pcg min max)
  "Return a random `single-float`.

  As a side effect, the state of `pcg` will be advanced.

  "
  (check-types pcg pcg-designator
               min single-float
               max single-float)
  (+ min (* (- max min)
            (pcg-random-float% (resolve-pcg pcg)))))

(defun pcg-random (pcg bound &optional max inclusive?)
  "Generate and return a random number in the specified interval.

  If `max` is omitted the interval will be `[0, bound)`.

  If `max` is given the interval will be `[bound, max)`.

  If `inclusive?` is given the interval will be `[bound, max]`.

  If either of `bound` or `max` are floats, the result will be a float.
  Otherwise the result will be an integer.

  As a side effect, the state of `pcg` will be advanced.

  "
  (let* ((float? (or (floatp bound)
                     (floatp max)))
         (result-type (if float? 'single-float 'integer))
         (delta (if (and inclusive? (not float?)) 1 0))
         (min (coerce (if (null max) 0 bound) result-type))
         (max (coerce (+ (if (null max) bound max) delta) result-type)))
    (assert (< min max) ()
      "Invalid interval for generating a random number: [~S, ~S~A"
      min max (if inclusive? "]" ")"))
    (case result-type
      (integer (pcg-random-integer pcg min max))
      (single-float (pcg-random-float pcg min max)))))


(defun pcg-advance (pcg steps)
  "Advance the state of `pcg` by `steps` steps.

  This function returns `nil` and is only useful for its side effects.

  "
  (check-types pcg pcg-designator
               steps u64)
  (pcg-advance% (resolve-pcg pcg) steps))

(defun pcg-rewind (pcg steps)
  "Rewind the state of `pcg` by `steps` steps.

  This function returns `nil` and is only useful for its side effects.

  "
  (check-types pcg pcg-designator
               steps u64)
  (pcg-rewind% (resolve-pcg pcg) steps))


;;;; Scratch ------------------------------------------------------------------
;; (defparameter *p* (make-pcg))

;; (defun data (n)
;;   (loop :repeat n :collect (pcg-random *p* 9.0)))

;; (losh:gnuplot
;;   (data 10000)
;;   :x #'identity
;;   :y (lambda (i) (/ 1.0 10000))
;;   :line-width 2
;;   :smooth :cumulative)

;; (defun slow-advance (pcg n)
;;   (dotimes (_ n) (pcg-random pcg))
;;   nil)

;; (defun test (n)
;;   (let ((a (make-pcg))
;;         (b (make-pcg)))
;;     (time (slow-advance a n))
;;     (time (pcg-advance% b n))
;;     (assert (= (pcg-random a) (pcg-random b)))))

;; (losh:gnuplot
;;   (sort (losh:hash-table-contents
;;           (losh:proportions
;;             (loop :repeat 1000000
;;                   :collect (pcg-random-range-inclusive *p* -50 200))))
;;         #'<
;;         :key #'first)
;;   :x #'first
;;   :y #'second
;;   :min-y 0.00
;;   :max-y 0.01
;;   :style :lines
;;   :line-width 1)

;;;; TODO ----------------------------------------------------------------------
;;; https://experilous.com/1/blog/post/perfect-fast-random-floating-point-numbers
;;; as an alternative float generation scheme.


