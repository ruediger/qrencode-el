;;; qrencode.el --- QRCode encoder

;; Copyright (C) 2021 Rüdiger Sonderfeld <ruediger@c-plusplus.net>

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.net>
;; Keywords: qrcode
;; Version: 0.1
;; Package: qrencode-el

;; This file is NOT part of GNU Emacs.

;;; Commentary:

;; qrencode-el provides a QRCode (ISO/IEC 18004:2015) encoder written
;; entirely in Emacs Lisp (elisp).  The encoder is not complete and
;; can currently only handle byte encoding for version 1 and 2 QRCodes
;; (version in QRCode means size and not version of the spec).  Work
;; on supporting larger versions and potentially more modes is
;; ongoing.

;;; Code:

;;; Error correction
;; Reed solomon ECC implementation based on https://research.swtch.com/field

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

;;; Data encoding
(defun qrencode--mode (mode)
  (pcase mode
    ('byte 4)  ; 0100
    ;; TODO
    (other (error "Mode %s not supported" other))))

(defun qrencode--encode-byte (input)
  (let* ((l (length input))
         (rest (logand l #xF)))
    (assert (<= l 255))
    (vconcat
     (vector (logior (ash (qrencode--mode 'byte) 4) (ash l -4))) 
     (cl-loop for d across input
              vconcat (vector (logior (ash rest 4) (ash d -4)))
              do (setq rest (logand d #xF))))))

;;; Basic patterns and templates handling
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

(defun qrencode--nextpos (row column size up)
  "Return next possible ROW, COLUMN and UP value."
  ;; A QRcode "column" (not to be confused with the column var here,
  ;; which is the x module position) is two modules (pixels).
  ;; Placement is from right to left and then upwards.
  ;;
  ;; Size or the QRCode is odd. So if a column number is even (except
  ;; if we are in column < 6 due to the timing pattern in col 6) the
  ;; go left.  Otherwise go upwards/downwards (depending on up) and
  ;; right.  Except if we hit the top/bottom then move a QCode column
  ;; (two modules) to the right and change direction (up).
  (when (= column 6)  ; Handle timing pattern column 6
    (setq column 5))

  (if (= (% column 2) (if (< column 6) 1 0))
      (if (<= column 0)
          (error "Overflowing pattern.  Data too large?")
        (list row (1- column) up))
    (if up
        (if (= row 0)
            (if (= column 0)
                (error "Overflowing pattern.  Data too large?")
              (list row (1- column) nil))
          (list (1- row) (1+ column) up))
      (if (>= row  (1- size))
          (if (= column 0)
              (error "Overflowing pattern.  Data too large?")
            (list row (1- column) t))
        (list (1+ row) (1+ column) up)))))

(defun qrencode--draw-data (qrcode function-pattern version data)
  (let* ((size (qrencode--size version))
         (up t)
         (row (1- size))
         (column (1- size)))
    (cl-loop for byte across data and didx from 0
             do (cl-loop for bidx from 7 downto 0
                         do (qrencode--aaset qrcode column row (logand (ash byte (- bidx)) 1))
                         do (unless (and (= didx (1- (length data))) (= bidx 0))
                              ;; Find new position avoiding function-patterns.
                              (cl-loop do (pcase-let ((`(,nrow ,ncolumn ,nup) (qrencode--nextpos row column size up)))
                                            (setq row nrow
                                                  column ncolumn
                                                  up nup))
                                       while (= (qrencode--aaref function-pattern row column) 1)))))))


;;; Data masking
(defun qrencode--penalty (qr)
  "Return penalty (the higher the worse) for a given QR pattern."

  (let ((size (length qr))
        (penalty 0))

    ;; 1. Adjacency:
    ;; More than 5 adjacent modules of same colour.
    (let ((N1 3))
      ;; Scan columns
      (let ((row 0) (col 0))
        (while (< row (1- size))
          (while (< col (1- size))
            (if (= (qrencode--aaref qr col row)
                   (qrencode--aaref qr (1+ col) row))
                (let ((i 0))
                  (while (and (< col (1- size)) (= (qrencode--aaref qr col row)
                                                   (qrencode--aaref qr (1+ col) row)))
                    (setq i (1+ i)
                          col (1+ col)))
                  (when (> i 5)
                    (setq penalty (+ penalty N1 (- i 5)))))
              (setq col (1+ col))))
          (setq row (1+ row))))

      ;; Scan rows
      (let ((row 0) (col 0))
        (while (< col (1- size))
          (while (< row (1- size))
            (if (= (qrencode--aaref qr col row)
                   (qrencode--aaref qr col (1+ row)))
                (let ((i 0))
                  (while (and (< row (1- size)) (= (qrencode--aaref qr col row)
                                                   (qrencode--aaref qr col (1+ row))))
                    (setq i (1+ i)
                          row (1+ row)))
                  (when (> i 5)
                    (setq penalty (+ penalty N1 (- i 5)))))
              (setq row (1+ row))))
          (setq col (1+ col)))))

    ;; 2. Block (2×2 block) of modules of the same colour
    (let ((N2 3))
      (setq penalty (+ penalty
                       (cl-loop for row from 0 below (1- size)
                                sum (cl-loop for col from 0 below (1- size)
                                             when (= (qrencode--aaref qr col row)
                                                     (qrencode--aaref qr col (1+ row))
                                                     (qrencode--aaref qr (1+ col) row)
                                                     (qrencode--aaref qr (1+ col) (1+ row)))
                                             sum N2)))))

    ;; 3. 1:1:3:1:1 pattern
    (let ((N3 40))
      ;; TODO
      )

    ;; 4. Ratio of dark to light
    (let ((N4 10)
          (dark (cl-loop for row across qr
                         sum (cl-loop for d across row sum d))))
      (setq penalty
            (+ penalty
               ;; Every 5% deviation from 50% dark/white ratio is penalised.
               (* (floor (/ (abs (- 0.5 (/ dark (* size size)))) 0.05) N4)))))

    penalty))

(defvar qrencode--MASKS
  [(lambda (i j) (= (% (+ i j) 2) 0))
   (lambda (i _j) (= (% i 2) 0))
   (lambda (_i j) (= (% j 3) 0))
   (lambda (i j) (= (% (+ i j) 3) 0))
   (lambda (i j) (= (% (+ (/ i 2) (/ j 3)) 2) 0))
   (lambda (i j) (= (+ (% (* i j) 2) (% (* i j) 3)) 0))
   (lambda (i j) (= (% (+ (% (* i j) 2) (% (* i j) 3)) 2) 0))
   (lambda (i j) (= (% (+ (% (+ i j) 2) (% (* i j) 3)) 2) 0))]
  "Mask patterns.")

(defun qrencode--copy (seq)
  "Copy a sequence of sequence SEQ."
  (let ((sq (make-vector (length seq) '[])))
    (dotimes (i (length seq))
      (aset sq i (seq-copy (aref seq i))))
    sq))

(defun qrencode--apply-mask (qrcode function-pattern datamask)
  "Return a copy of QRCODE with DATAMASK applied except for FUNCTION-PATTERN."
  (let ((qr (qrencode--copy qrcode))
        (size (length qrcode))
        (m (aref qrencode--MASKS datamask)))
    (cl-loop for i below size
             do (cl-loop for j below size
                         unless (= (qrencode--aaref function-pattern j i) 1)
                         do (qrencode--aaset qr j i
                                             (logxor (qrencode--aaref qr j i)
                                                     (if (funcall m i j) 1 0)))))
    qr))

(defun qrencode--find-best-mask (qr function-pattern)
  "Return cons of QR with best mask applied and mask number, avoiding FUNCTION-PATTERN."
  (let (bestqr (bestmask 0) (bestpenalty #xFFFFFFFF))
    (dotimes (mask (length qrencode--MASKS))
      (let* ((newqr (qrencode--apply-mask qr function-pattern mask))
             (penalty (qrencode--penalty newqr)))
        (when (< penalty bestpenalty)
          (setq bestqr newqr
                bestmask mask
                bestpenalty penalty))))
    (cons bestqr bestmask)))

;;; Version/Info encoding
(defun qrencode--bch-check-format (fmt)
  (let ((g #x537))  ; 10100110111
    (cl-loop for i from 4 downto 0
             when (logand fmt (ash 1 (+ 10 i)))
             do (setq fmt (logxor fmt (ash g i)))))
  fmt)

(defun qrencode--bch-encode (data &optional mask)
  (logxor (+ (ash data 10) (qrencode--bch-check-format (ash data 10)))
          (or mask #x5412))) ; 101010000010010

(defun qrencode--errcorr (errcorr)
  "Return info representation of error correction level ERRCORR."
  (pcase errcorr
    ('L 1)
    ('M 0)
    ('Q 3)
    ('H 2)
    (other (error "Unknown error correction level %s" other))))

(defun qrencode--encode-info (qr errcorr datamask)
  "Set info data on QR encoding ERRCORR and DATAMASK."
  (let* ((size (length qr))
         (info (qrencode--bch-encode (logior (ash (qrencode--errcorr errcorr) 3) datamask)))
         (r1 0) (c1 8)
         (r2 8) (c2 (1- size)))
    (dotimes (i 8)
      (let ((data (logand (ash info (- i)) 1)))
        (qrencode--aaset qr c1 r1 data)
        (setq r1 (1+ r1))
        (when (= r1 6)                  ; Avoid timing pattern
          (setq r1 (1+ r1)))
        (qrencode--aaset qr c2 r2 data)
        (setq c2 (1- c2))))

    (setq c1 (1- c1)
          r1 (1- r1)
          r2 (- size 7)
          c2 8)
    (qrencode--aaset qr c2 (1- r2) 1)   ; Dark module in corner
    (cl-loop for i from 8 to 14
             do (let ((data (logand (ash info (- i)) 1)))
                  (qrencode--aaset qr c1 r1 data)
                  (setq c1 (1- c1))
                  (when (= c1 6)        ; Avoid timing pattern
                    (setq c1 (1- c1)))
                  (qrencode--aaset qr c2 r2 data)
                  (setq r2 (1+ r2))))))

(defun qrencode--encode-version (qr version)
  "Set on QT the VERSION data."
  (unless (< version 7)  ; only version >= 7 have version encoding
    ;; TODO
    ))

;; Analyse data: sizing etc.
(defun qrencode--char-count-bits (version mode)
  "Return the number of bits per character given VERSION and MODE."
  (cdr (assq mode
             (pcase version
               ((and n (guard (<= 1 n 9)))
                '((numeric      . 10)
                  (alphanumeric .  9)
                  (byte         .  8)
                  (kanji        .  8)))
               ((and n (guard (<= 10 n 26)))
                '((numeric      . 12)
                  (alphanumeric . 11)
                  (byte         . 16)
                  (kanji        . 10)))
               ((and n (guard (<= 27 n 40)))
                '((numeric      . 14)
                  (alphanumeric . 13)
                  (byte         . 16)
                  (kanji        . 12)))
               (other (error "Unsupported version %d (range 1 to 40)" other))))))

(defun qrencode--length-in-version (n version mode)
  "Return length of a string of size N in VERSION and MODE."
  (+ n (ceiling (+ 4 (/ (qrencode--char-count-bits version mode) 8)))))

(defvar qrencode--SIZE-TABLE
  [(26 . ((L . ( 7 3 1))
          (M . (10 2 1))
          (Q . (13 1 1))
          (H . (17 1 1))))
   (44 . ((L . (10 2 1))
          (M . (16 0 1))
          (Q . (22 0 1))
          (H . (28 0 1))))
   ;; TODO ...
   ]
  "List of size table.  Index is version number - 1 and content is
  cons of size to an assoc list of error correction level to
  number of error correction code words, p, and error correction
  blocks.")

(defun qrencode--find-version (n mode &optional errcorr)
  "Return cons of version and error correction based on data length N, MODE."
  (or (cl-loop named outer-loop
               for entry across qrencode--SIZE-TABLE and version from 1
               do (pcase-let ((`(,num-codewords . ,errlevels) entry)
                              (m (qrencode--length-in-version n version mode)))
                    (if errcorr
                        (when (>= (- num-codewords (cadr (assq errcorr errlevels))) m)
                          (cl-return (cons version errcorr)))
                      (cl-loop for e in '(H Q M L) ; Go from highest err corr level to lowest
                               do (when (>= (- num-codewords (cadr (assq e errlevels))))
                                    (cl-return-from outer-loop (cons version e)))))))
      (error "No version found supporting %d in mode %s with error correction level %s" n mode errcorr)))

;;; QRCode encoding
(defun qrencode (s &optional mode errcorr return-raw)
  "Encode string S into a QRCode."
  ;; Following Section 7.1 from ISO/IEC 18004 2015

  (let (version data qr function-pattern datamask)
    ;; Step 1: Analyse data
    ;; TODO find suitable mode. For now we only support byte
    (setq mode (or mode 'byte))
    ;; Find the version with the highest error correction to fit the data
    (pcase-let ((`(,ver . ,ec) (qrencode--find-version (length s) mode errcorr)))
      (setq version ver
            errcorr ec))

    ;; Step 2: Encode data
    (setq data (qrencode--encode-byte s))
    ;; Add padding
    (let* ((errcorrlen (cadr (assq errcorr (cdr (aref qrencode--SIZE-TABLE (1- version))))))
           (datalen (- (car (aref qrencode--SIZE-TABLE (1- version))) errcorrlen))
           (padding [#xEC #x11])
           ecc)
      (setq data (vconcat data [#x40]  ; TODO: why the #x40?
                          (cl-loop for i from 0 below (- datalen (length data) 1)
                                   vconcat (vector (aref padding (% i 2))))))
      ;; Step 3: Error correction coding
      (setq ecc (qrencode-ecc data errcorrlen))
      (setq data (vconcat data ecc)))

    ;; Step 4: Structure final message
    ;; TODO

    ;; Step 5: Module placement
    (pcase-let ((`(,qrcode . ,fp) (qrencode--template version)))
      (qrencode--draw-data qrcode fp version data)
      (setq qr qrcode
            function-pattern fp))

    ;; Step 6: Data masking
    (pcase-let ((`(,qrcodemasked . ,mask) (qrencode--find-best-mask qr function-pattern)))
      (setq qr qrcodemasked
            datamask mask))

    ;; Step 7: Format and version information
    (qrencode--encode-info qr errcorr datamask)
    (qrencode--encode-version qr version)

    (if return-raw
        qr
      (qrencode-format qr))))

(defun qrencode-format (qr)
  "Format QR using utf-8 blocks."
  (let ((size (length qr)))
    (concat (make-string size ? ) "\n"
            (make-string size ? ) "\n"
            (cl-loop for row from 0 below (1- size) by 2
                     concat "    "
                     concat (cl-loop for col from 0 below size
                                     concat (if (and (/= (qrencode--aaref qr col row) 0)
                                                     (/= (qrencode--aaref qr col (1+ row)) 0))
                                                "█"
                                              (if (and (/= (qrencode--aaref qr col row) 0)
                                                       (= (qrencode--aaref qr col (1+ row)) 0))
                                                  "▀"
                                                (if (and (= (qrencode--aaref qr col row) 0)
                                                         (/= (qrencode--aaref qr col (1+ row)) 0))
                                                    "▄"
                                                  " "))))
                     concat "    \n")
            "    "
            (cl-loop for col from 0 below size
                     concat (if (/= (qrencode--aaref qr col (1- size)) 0) "▀" " "))
            "    \n"
            (make-string size ? ) "\n"
            (make-string size ? ) "\n")))

;; TODO qrencode-insert using faces

(provide 'qrencode)

;;; qrencode.el ends here
