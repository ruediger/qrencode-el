

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

