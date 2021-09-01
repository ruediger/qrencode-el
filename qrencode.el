

;;; Reed solomon ECC implementation based on https://research.swtch.com/field

(defun qrencode--mul-no-lut (x y poly)
  "Caryless-multiply  X and Y modulo POLY."
  (let ((z 0))
    (while (> x 0)
      (when (/= (logand x 1) 0)
        (setq z (logxor z y)))
      (setq x (ash x -1))
      (setq y (ash y +1))
      ;; Modulo operation: If we shift y too far we have to subtract
      ;; poly again.
      (when (/= (logand y #x100) 0)
        (setq y (logxor y poly))))
    z))

(defun qrencode--init-field (poly a)
  "Initialise a field LUT with POLY and A."
  (let ((log (make-vector 256 0))
        (exp (make-vector 510 0))
        (x 1))
    (dotimes (i 255)
      (aset exp i x)
      (aset exp (+ i 255) x)
      (aset log x i)
      (setq x (qrencode--mul-no-lut x a poly)))
    (aset log 0 255)
    (list log exp)))

(defun qrencode--field-exp (field e)
  "Return exponential E in FIELD."
  (if (< e 0)
      0
    (aref (cadr field) (% e 255))))

(defun qrencode--field-log (field l)
  "Return log of L in FIELD."
  (if (= l 0)
      -1
    (aref (car field) l)))

(defun qrencode--field-mul (field x y)
  "Multiply X with Y in FIELD."
  (if (or (= x 0) (= y 0))
      0
    (qrencode--field-exp field
                         (+ (qrencode--field-log field x)
                            (qrencode--field-log field y)))))

(defun qrencode--gen (field e)
  "Return generator polynomial and its log in FIELD."
  (let ((p (make-vector (1+ e) 0))
        (lp (make-vector (1+ e) 0)))  ; log(p)
    ;; calculate p
    (aset p e 1)
    (dotimes (i e)
      (let ((c (qrencode--field-exp field i)))
        (dotimes (j e)
          (aset p j (logxor
                     (qrencode--field-mul field (aref p j) c)
                     (aref p (1+ j)))))))

    ;; calculate log(p)
    (dotimes (i (1+ e))
      (let ((c (aref p i)))
        (if (= c 0)
            (aset lp i 255)
          (aset lp i (qrencode--field-log field c)))))

    (list p (seq-subseq lp 1))))

(defun qrencode-ecc (data c &optional field)
  "Return ECC for DATA with length C"
  (setq field (or field (qrencode--init-field #x11d 2)))
  (let ((p (vconcat data (make-vector c 0)))   ; Data padded with 0 bytes
        (lgen (cadr (qrencode--gen field c))))

    (dotimes (i (length data))
      (unless (= (aref p i) 0)
        (let ((exp (qrencode--field-log field (aref p i))))
          (dotimes (j (length lgen))
            (let ((lg (aref lgen j)))
              (when (/= lg 255)
                (aset p (+ i j 1)
                      (logxor (aref p (+ i j 1))
                              (aref (cadr field) (+ exp lg))))))))))
    (seq-subseq p (length data))))

;;; Util

(defun qrencode--size (version)
  "Return number of modules for VERSION."
  (assert (<= 1 version 40) 'show-args "Version %d out of valid range [1, 40]" version)
  (+ (* (1- version) 4) 21))

(defvar qrencode--FINDER-PATTERN
  '[[1 1 1 1 1 1 1]
    [1 0 0 0 0 0 1]
    [1 0 1 1 1 0 1]
    [1 0 1 1 1 0 1]
    [1 0 1 1 1 0 1]
    [1 0 0 0 0 0 1]
    [1 1 1 1 1 1 1]]
  "QRCode Finder Pattern")

(defvar qrencode--ALIGNMENT-PATTERN
  '[[1 1 1 1 1]
    [1 0 0 0 1]
    [1 0 1 0 1]
    [1 0 0 0 1]
    [1 1 1 1 1]]
  "QRCode Alignment Pattern")

(defvar qrencode--ALIGNMENT-PATTERN-PLACEMENT
  '[nil
    nil
    [6 18]
    ;; TODO...
    ]
  "Placement of alignment pattern.  Vector index is the version."
  )

(defun qrencode--p2s (pattern)
  )

(defun qrencode--square (n &optional init)
  "Return a square of size N."
  (let ((sq (make-vector n '[])))
    (dotimes (i n)
      (aset sq i (make-vector n (or init 0))))
    sq))

(defun qrencode--aaset (dst x y val)
  "Set in sequence of sequences DST position X (inner), Y (outer) to VAL"
  (aset (aref dst y) x val))

(defun qrencode--aaref (src x y)
  "Return from sequence of sequences SRC value at position X (inner), Y (outer)."
  (aref (aref src y) x))

(defun qrencode--copy-square (dst pattern x y)
  "Copy to DST from PATTERN at X and Y."
  (dotimes (i (length pattern))
    (dotimes (j (length (aref pattern i)))
      (qrencode--aaset dst (+ y i) (+ x j) (qrencode--aaref pattern j i)))))

(defun qrencode--set-rect (dst x y width height &optional value)
  "Set a rectangle in DST at X, Y of WIDTH and HEIGHT to VALUE."
  (dotimes (i height)
    (dotimes (j width)
      (qrencode--aaset dst (+ y i) (+ x j) (or value 1)))))

(defun qrencode--template (version)
  "Return basic QRCode template for VERSION."
  (let* ((size (qrencode--size version))
         (qrcode (qrencode--square size))
         (function-pattern (qrencode--square size)))  ; Keeps track of location of function patterns
    ;; Finder pattern
    ;; Top Left
    (qrencode--copy-square qrcode qrencode--FINDER-PATTERN 0 0)
    (qrencode--set-rect function-pattern 0 0 9 9) ; finder pattern + separator

    ;; Top Right
    (qrencode--copy-square qrcode qrencode--FINDER-PATTERN (- size 7) 0)
    (qrencode--set-rect function-pattern (- size 8) 0 8 8)  ; finder pattern + separator
    (when (>= version 7)
      (qrencode--set-rect function-pattern (- size 10) 0 3 6))  ; version info
    (qrencode--set-rect function-pattern (- size 8) 8 8 1) ; format info

    ;; Bottom Left
    (qrencode--copy-square qrcode qrencode--FINDER-PATTERN 0 (- size 7))
    (qrencode--set-rect function-pattern 0 (- size 8) 8 8)  ; finder pattern + separator
    (when (>= version 7)
      (qrencode--set-rect function-pattern 0 (- size 10) 6 3))  ; version info
    (qrencode--set-rect function-pattern 8 (- size 8) 1 8)  ; format info

    ;; Alignment patterns
    (let ((alignment-pattern (aref qrencode--ALIGNMENT-PATTERN-PLACEMENT version)))
      (when alignment-pattern
        ;; Alignment patterns are placed centred at all row/column
        ;; combinations.
        (seq-doseq (r alignment-pattern)
          (seq-doseq (c alignment-pattern)
            (when (and (>= r 12) (>= c 12)  ; Skip functional patterns
                       (<= c (+ size 12)) (>= r 11)
                       (>= c 10) (<= r (+ size 12)))
              (qrencode--copy-square qrcode qrencode--ALIGNMENT-PATTERN (- c 2) (- r 2))
              (qrencode--set-rect function-pattern c r 5 5))))))


    ;; Timing pattern
    (cl-loop for i from 8 to (- size 8)
             do (qrencode--aaset qrcode 6 i (% (1+ i) 2))
             do (qrencode--aaset function-pattern 6 i 1)
             do (qrencode--aaset qrcode i 6 (% (1+ i) 2))
             do (qrencode--aaset function-pattern i 6 1))

    (cons qrcode function-pattern)))
