;;; qrencode-tests.el --- Tests for qrencode.el  -*- lexical-binding: t -*-

;; Copyright (C) 2021-2026 Free Software Foundation, Inc.

;; Author: Rüdiger Sonderfeld <ruediger@c-plusplus.de>

;; This file is NOT part of GNU Emacs.

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Tests for qrencode.el

;;; Code:

(require 'ert)
(require 'qrencode)

;;; Error correction code tests
;; Based on https://research.swtch.com/field

(ert-deftest qrencode-field-test ()
  (pcase-let* ((field (qrencode--init-field #x11d 2))
               (`(,log ,exp) field))
    (dotimes (i 255)
      (should (= (aref log (aref exp i)) i))
      (should (= (aref log (aref exp (+ i 255))) i))
      (should (= (aref exp (aref log (1+ i))) (1+ i))))
    (should (= (qrencode--field-exp field 0) 1))
    (should (= (qrencode--field-exp field 1) 2))))

(ert-deftest qrencode-ecc-test ()
  "Compare ECC encoding against known value."
  (let ((data [#x10 #x20 #x0c #x56 #x61 #x80 #xec #x11 #xec #x11 #xec #x11 #xec #x11 #xec #x11])
        (check [#xa5 #x24 #xd4 #xc1 #xed #x36 #xc7 #x87 #x2c #x55]))
    (should (equal (qrencode--ecc data (length check)) check))))

(ert-deftest qrencode-ecc-linear-test ()
  (let ((field (qrencode--init-field #x11d 2)))

    (should (equal (qrencode--ecc [#x00 #x00] 2 field) [#x00 #x00]))

    (let* ((c1 (qrencode--ecc [#x00 #x01] 2 field))
           (c2 (qrencode--ecc [#x00 #x02] 2 field))
           (cx (cl-loop for i across c1 for j across c2 vconcat (vector (logxor i j))))
           (c4 (qrencode--ecc [#x00 #x03] 2 field)))
      (should (equal c4 cx)))))

;;; Util

(ert-deftest qrencode-size-test ()
  "Verify known sizes."
  (should (= (qrencode--size 1) 21))
  (should (= (qrencode--size 2) 25))
  (should (= (qrencode--size 6) 41))
  (should (= (qrencode--size 7) 45))
  (should (= (qrencode--size 14) 73))
  (should (= (qrencode--size 21) 101))
  (should (= (qrencode--size 40) 177)))

;;; Data encoding
(ert-deftest qrencode-mode-test ()
  (should (= (qrencode--mode 'byte) 4)))

(ert-deftest qrencode-encode-byte-test ()
  (should (equal (qrencode--encode-byte "hello" 1) [#x40 #x56 #x86 #x56 #xc6 #xc6 #xf0]))
  (let* ((txt "https://github.com/ruediger/qrencode-el")
         (in (qrencode--encode-byte txt 4))
         (decode (let ((rest 0))
                   (cl-loop for b across in
                            collect (logior (ash rest 4) (ash b -4))
                            do (setq rest (logand b #xF))))))
    (should (= (elt decode 0) 4))
    (should (= (elt decode 1) (length txt)))
    (should (string= (apply #'string (seq-subseq decode 2)) txt)))
  (should (equal (seq-subseq (qrencode--encode-byte (qrencode--repeat-string "a" 256) 10) 0 3)
                 ;; Mode indicator (4), #x100, (ash #x61 04)
                 ;; 0100 0000 0001 0000 0000 0110
                 [#x40 #x10 #x6])))

(ert-deftest qrencode-encode-aa-test ()
  (let ((s (qrencode--square 5)))
    (dotimes (r 5)
      (dotimes (c 5)
        (should (= (qrencode--aaref s c r) 0))))
    (qrencode--aaset s 2 1 1)
    (should (= (qrencode--aaref s 2 1) 1))
    ;; Check that nothing else was changed
    (dotimes (r 5)
      (dotimes (c 5)
        (unless (and (equal (cons c r) '(2 . 1)))
          (should (= (qrencode--aaref s c r) 0)))))

    (qrencode--copy-square s [[2]] 1 2)
    (should (= (qrencode--aaref s 1 2) 2))
    (qrencode--copy-square s [[3 3] [3 3]] 1 1)
    (should (= (qrencode--aaref s 1 1) 3))
    (should (= (qrencode--aaref s 2 1) 3))
    (should (= (qrencode--aaref s 1 2) 3))
    (should (= (qrencode--aaref s 2 2) 3))

    (qrencode--set-rect s 2 2 2 2 4)
    (should (= (qrencode--aaref s 2 2) 4))
    (should (= (qrencode--aaref s 3 2) 4))
    (should (= (qrencode--aaref s 2 3) 4))
    (should (= (qrencode--aaref s 3 3) 4)))


  (let ((s (qrencode--square 20)))
    (qrencode--copy-square s [[9 9] [9 9]] 10 5)
    (should (= (qrencode--aaref s 10 5) 9))
    (should (= (qrencode--aaref s 10 6) 9))
    (should (= (qrencode--aaref s 11 5) 9))
    (should (= (qrencode--aaref s 11 6) 9))

    (should (= (qrencode--aaref s 5 10) 0))
    (should (= (qrencode--aaref s 6 10) 0))
    (should (= (qrencode--aaref s 5 11) 0))
    (should (= (qrencode--aaref s 6 11) 0))))

(ert-deftest qrencode-template-test ()
  "Test basic templates."
  (pcase-let ((`(,qr . ,fp) (qrencode--template  1))) ; TODO: Maybe test a version with alignment pattern
    (should (equal qr [[1 1 1 1 1 1 1 0 0 0 0 0 0 0 1 1 1 1 1 1 1]
                       [1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1]
                       [1 0 1 1 1 0 1 0 0 0 0 0 0 0 1 0 1 1 1 0 1]
                       [1 0 1 1 1 0 1 0 0 0 0 0 0 0 1 0 1 1 1 0 1]
                       [1 0 1 1 1 0 1 0 0 0 0 0 0 0 1 0 1 1 1 0 1]
                       [1 0 0 0 0 0 1 0 0 0 0 0 0 0 1 0 0 0 0 0 1]
                       [1 1 1 1 1 1 1 0 1 0 1 0 1 0 1 1 1 1 1 1 1]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 0 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 0 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 0 1 1 1 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]]))
    (should (equal fp [[1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 1 1 1 1 1 1 1 1]
                       [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [0 0 0 0 0 0 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]
                       [1 1 1 1 1 1 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0]]))))


;; TODO: Test qrencode--draw-data

;;; Data masking
(ert-deftest qrencode-penalty-adjacency-test ()
  "Test rule 1 penalties."
  ;; Exactly four same colour should not incur penalty
  (should (= (qrencode--penalty-adjacency [[1 1 1 1 0 0]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]])
             0))
  ;; Exactly five same colour should incur penalty
  (should (= (qrencode--penalty-adjacency [[1 1 1 1 1 0]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]])
             3))
  ;; Six of same colour should incurs penalty
  (should (= (qrencode--penalty-adjacency [[1 1 1 1 1 1]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]])
             4))
  ;; Six of same colour should incurs penalty (different row)
  (should (= (qrencode--penalty-adjacency [[0 1 0 1 0 1]
                                           [1 1 1 1 1 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]])
             4))
  ;; Six of same colour should incurs penalty (cols)
  (should (= (qrencode--penalty-adjacency [[0 1 0 1 0 1]
                                           [1 1 0 1 1 0]
                                           [1 1 1 0 1 0]
                                           [0 1 0 1 0 1]
                                           [1 1 1 0 1 0]
                                           [0 1 0 1 0 1]])
             4))
  ;; Six of same colour should incurs penalty (last row)
  (should (= (qrencode--penalty-adjacency [[0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 0]
                                           [0 1 0 1 0 1]
                                           [1 1 1 1 1 1]])
             4))
  ;; Six of same colour should incurs penalty (last cols)
  (should (= (qrencode--penalty-adjacency [[1 0 1 0 1 1]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 1]
                                           [0 1 0 1 0 1]
                                           [1 0 1 0 1 1]
                                           [0 1 0 1 0 1]])
             4)))

(ert-deftest qrencode-penalty-blocks-test ()
  "Test 2x2 block rule"
  (should (= (qrencode--penalty-blocks [[0 1 0 1 0 1]
                                        [1 1 0 1 1 0]
                                        [1 1 1 0 1 0]
                                        [0 1 0 1 0 1]
                                        [1 1 1 0 1 0]
                                        [0 1 0 1 0 1]])
             3)))

(ert-deftest qrencode-penalty-11311-test ()
 "Test 1:1:3:1:1 penalty"
 (should (= (qrencode--penalty-11311
              [[1 0 1 0 1 0 0 1 0 1 0]
               [0 0 0 0 1 0 1 1 1 0 1]
               [1 0 1 0 1 0 1 0 1 0 1]
               [0 1 0 1 0 1 0 1 0 1 0]
               [1 0 1 0 1 0 1 0 1 0 1]
               [0 1 0 1 0 1 0 1 0 1 0]
               [1 0 1 0 1 0 1 0 1 0 1]
               [0 1 0 1 0 1 0 1 0 1 0]
               [1 0 1 0 1 0 1 0 1 0 1]
               [0 1 0 1 0 1 0 1 0 1 0]
               [1 0 1 0 1 0 1 0 1 0 1]])
             40)))

(ert-deftest qrencode-penalty-dark-light-ratio-test ()
  "Test dark light ratio penalty."
  (should (= (qrencode--penalty-dark-light-ratio
              [[1 1 0 1 0 1]
               [1 0 1 0 1 0]
               [0 1 0 1 0 1]
               [1 0 1 0 1 0]
               [0 1 0 1 0 1]
               [1 0 1 0 1 1]])
             10)))

(ert-deftest qrencode-no-penalty-test ()
  "Checker board should be zero penalty."
  (should (= (qrencode--penalty [[0 1 0 1 0 1]
                                 [1 0 1 0 1 0]
                                 [0 1 0 1 0 1]
                                 [1 0 1 0 1 0]
                                 [0 1 0 1 0 1]
                                 [1 0 1 0 1 0]])
             0)))

(ert-deftest qrencode-penalty-test ()
  "`qrencode--penalty' must be the sum of all four rules."
  (let ((qr [[1 0 1 0 1 0 0 1 0 1 0]
             [0 0 0 0 1 0 1 1 1 0 1]
             [1 0 1 0 1 0 1 0 1 0 1]
             [1 1 0 1 0 1 0 1 0 1 0]
             [1 0 1 0 1 0 1 0 1 0 1]
             [0 1 0 1 1 1 0 1 0 1 0]
             [1 0 1 0 1 0 1 0 1 0 1]
             [0 1 0 1 0 1 0 1 1 1 0]
             [1 1 1 1 1 1 1 0 1 0 1]
             [1 1 0 1 0 1 0 1 0 1 0]
             [1 0 1 0 1 0 1 0 1 0 1]]))
    ;; This board trips every rule, so a missing term cannot cancel out:
    ;; 5 (a run of seven) + 3 (one 2x2 block) + 40 (one 1:1:3:1:1) +
    ;; 10 (67/121 = 55.4% dark, one 5% step from half).
    (should (= (qrencode--penalty-adjacency qr) 5))
    (should (= (qrencode--penalty-blocks qr) 3))
    (should (= (qrencode--penalty-11311 qr) 40))
    (should (= (qrencode--penalty-dark-light-ratio qr) 10))
    (should (= (qrencode--penalty qr) 58))))

(ert-deftest qrencode-masks-test ()
  (should (equal (qrencode--apply-mask (qrencode--square 10) (qrencode--square 10) 0)
                 [[1 0 1 0 1 0 1 0 1 0]
                  [0 1 0 1 0 1 0 1 0 1]
                  [1 0 1 0 1 0 1 0 1 0]
                  [0 1 0 1 0 1 0 1 0 1]
                  [1 0 1 0 1 0 1 0 1 0]
                  [0 1 0 1 0 1 0 1 0 1]
                  [1 0 1 0 1 0 1 0 1 0]
                  [0 1 0 1 0 1 0 1 0 1]
                  [1 0 1 0 1 0 1 0 1 0]
                  [0 1 0 1 0 1 0 1 0 1]]))
  (should (equal (qrencode--apply-mask (qrencode--square 10) (qrencode--square 10) 1)
                 [[1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]
                  [1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]
                  [1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]
                  [1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]
                  [1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]]))
  (should (equal (qrencode--apply-mask (qrencode--square 10) (qrencode--square 10) 1)
                 [[1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]
                  [1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]
                  [1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]
                  [1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]
                  [1 1 1 1 1 1 1 1 1 1]
                  [0 0 0 0 0 0 0 0 0 0]]))
  ;; TODO: remaining masks
  )

;; TODO: test find-best-mask

(ert-deftest qrencode-function-patterns-intact-test ()
  "Finder, separator, timing and alignment modules must survive encoding."
  (dolist (input '("hello" "https://github.com/ruediger/qrencode-el"))
    (let* ((qr (qrencode input nil nil 'return-raw))
           (size (length qr))
           (version (/ (- size 17) 4)))
      (pcase-let ((`(,template . ,fp) (qrencode--template version)))
        (dotimes (y size)
          (dotimes (x size)
            ;; Skip the areas written after templating: format info, version
            ;; info and the dark module.
            (unless (or (= x 8) (= y 8)
                        (and (>= version 7)
                             (or (and (< x 6) (>= y (- size 11)) (< y (- size 8)))
                                 (and (< y 6) (>= x (- size 11)) (< x (- size 8))))))
              (when (= 1 (qrencode--aaref fp x y))
                (should (= (qrencode--aaref qr x y)
                           (qrencode--aaref template x y)))))))
        ;; Dark module, section 7.9.1.
        (should (= 1 (qrencode--aaref qr 8 (- size 8))))))))

;;; Version/Info encoding

;; Helper functions
(defun qrencode-tests--format-bits (qr position)
  "Return the 15-bit format information word read from QR.
POSITION is `top-left' for the top-left copy, `bottom+right' for the split
copy along the right and bottom edges."
  (let ((size (length qr))
        (v 0))
    (dotimes (i 15)
      (let ((bit (if (eq position 'top-left)
                     (cond ((<= i 5) (qrencode--aaref qr 8 i))
                           ((= i 6)  (qrencode--aaref qr 8 7))
                           ((= i 7)  (qrencode--aaref qr 8 8))
                           ((= i 8)  (qrencode--aaref qr 7 8))
                           (t        (qrencode--aaref qr (- 14 i) 8)))
                   (if (<= i 7)
                       (qrencode--aaref qr (- size 1 i) 8)
                     (qrencode--aaref qr 8 (+ (- size 7) (- i 8)))))))
        (setq v (logior v (ash bit i)))))
    v))

(defun qrencode-tests--decode-format (qr)
  "Return (ERRCORR . MASK) decoded from QR's format information."
  (let* ((raw (qrencode-tests--format-bits qr 1))
         (data (ash (logxor raw #x5412) -10)))
    (cons (car (rassq (ash data -3) '((L . 1) (M . 0) (Q . 3) (H . 2))))
          (logand data 7))))

(ert-deftest qrencode-bch-encode-test ()
  ;; Section 7.9.1. Err corr: M, Mask 5 (101) -> 0b100000011001110
  (should (= (qrencode--bch-encode #x5) #x40CE)))

(ert-deftest qrencode--version-ecc-test ()
  ;; Section 7.10: Version 7 -> 000111110010010100
  (should (= (qrencode--version-ecc 7) #x7C94)))

(ert-deftest qrencode--mod-test ()
  (should (= (qrencode--mod 0 #x537) 0)))

(ert-deftest qrencode-format-info-conformance-test ()
  "Both copies of the format information must agree and be a valid BCH word."
  (dolist (input '("hello"
                   "https://github.com/ruediger/qrencode-el"
                   "0123456789"
                   "x"))
    (let* ((qr (qrencode input nil nil 'return-raw))
           (c1 (qrencode-tests--format-bits qr 'top-left))
           (c2 (qrencode-tests--format-bits qr 'bottom+right)))
      ;; The two copies are redundant: they must be identical.
      (should (= c1 c2))
      ;; It must be a well-formed BCH(15,5) word masked with 0x5412.
      (pcase-let ((`(,ec . ,mask) (qrencode-tests--decode-format qr)))
        (should (memq ec '(L M Q H)))
        (should (<= 0 mask 7))
        (should (= c1 (qrencode--bch-encode
                       (logior (ash (qrencode--errcorr ec) 3) mask))))))))

(ert-deftest qrencode-version-info-conformance-test ()
  "Version information must be present, doubled and correct for version >= 7.
  The expected word for version 7 is the literal from section 7.10 of the
  standard, so this does not lean on `qrencode--version-ecc' as its own oracle."
  (dolist (n '(150 271 900))
    (let* ((qr (qrencode (make-string n ?a) nil nil 'return-raw))
           (size (length qr))
           (version (/ (- size 17) 4))
           (bl 0) (tr 0))
      (should (>= version 7))
      (dotimes (i 18)
        (let ((a (/ i 3)) (b (% i 3)))
          (setq bl (logior bl (ash (qrencode--aaref qr a (+ (- size 11) b)) i))
                tr (logior tr (ash (qrencode--aaref qr (+ (- size 11) b) a) i)))))
      ;; The two copies are redundant: they must be identical.
      (should (= bl tr))
      (when (= version 7)
        (should (= bl #x07C94))))))

(ert-deftest qrencode--find-version-test ()
  "Selection must match the byte-mode capacities of ISO/IEC 18004 Table 7."
  ;; Version 1 holds 17 (L), 14 (M), 11 (Q), 7 (H) bytes.  Check each
  ;; capacity and the byte that overflows it: an error in the header
  ;; overhead shifts every one of these boundaries.
  (should (equal (qrencode--find-version  7 'byte) '(1 . H)))
  (should (equal (qrencode--find-version  8 'byte) '(1 . Q)))
  (should (equal (qrencode--find-version 11 'byte) '(1 . Q)))
  (should (equal (qrencode--find-version 12 'byte) '(1 . M)))
  (should (equal (qrencode--find-version 14 'byte) '(1 . M)))
  (should (equal (qrencode--find-version 15 'byte) '(1 . L)))
  (should (equal (qrencode--find-version 17 'byte) '(1 . L)))
  (should (equal (qrencode--find-version 18 'byte) '(2 . Q)))
  ;; The character count indicator grows from 8 to 16 bits at version 10,
  ;; so the overhead goes from 2 to 3 codewords across this boundary.
  ;; Table 7: 230 bytes at 9-L, 271 at 10-L.
  (should (equal (qrencode--find-version 230 'byte) '(9 . L)))
  (should (equal (qrencode--find-version 231 'byte) '(10 . L)))
  ;; Largest payload the format can carry, Table 7: 2953 at 40-L.
  (should (equal (qrencode--find-version 2953 'byte) '(40 . L)))
  (should-error (qrencode--find-version 2954 'byte) :type 'user-error)
  ;; An explicit level takes the other branch of the function.
  (should (equal (qrencode--find-version 100 'byte 'H) '(10 . H)))
  (should (equal (qrencode--find-version 100 'byte 'L) '(5 . L)))
  (should (equal (qrencode--find-version 1273 'byte 'H) '(40 . H)))
  (should-error (qrencode--find-version 1274 'byte 'H) :type 'user-error))

;; Analyse data

;;
(ert-deftest qrencode--get-subseq-test ()
  (should (equal (qrencode--get-subseq 4 12) '((0 . 12)
                                               (12 . 24)
                                               (24 . 36)
                                               (36 . 48))))
  (should (equal (qrencode--get-subseq 2 2 5) '((5 . 7)
                                                (7 . 9)))))

(ert-deftest qrencode--get-blocks-test ()
  (should (equal (qrencode--get-blocks 5 'H) '((0 . 11)
                                               (11 . 22)
                                               (22 . 34)
                                               (34 . 46)))))

(ert-deftest qrencode--blocks-test ()
  (should (equal (qrencode--blocks (number-sequence 0 46) 5 'H)
                 [(0 1 2 3 4 5 6 7 8 9 10)
                  (11 12 13 14 15 16 17 18 19 20 21)
                  (22 23 24 25 26 27 28 29 30 31 32 33)
                  (34 35 36 37 38 39 40 41 42 43 44 45)])))

(ert-deftest qrencode--repeat-string-test ()
  (should (string= (qrencode--repeat-string "12" 3) "121212"))
  (should (string= (qrencode--repeat-string "ABC" 5 " ") "ABC ABC ABC ABC ABC")))

(ert-deftest qrencode-format-as-netpbm-test ()
  (should (string= (qrencode-format-as-netpbm [[1 0] [0 1]])
                   "P1
30 30
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 1 1 1 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0 0
")))

(ert-deftest qrencode-zbarimg-test ()
  "Test decoding generated QRCodes using the zbarimg program."
  (let ((zbarimg (executable-find "zbarimg")))
    (skip-unless zbarimg)
    (let ((tmpfile (make-temp-file "qr" nil ".pbm")))
      (cl-loop for input across
               ["hello"
                "https://github.com/ruediger/qrencode-el"
                "hellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohellohello"
                "qrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqrqr"

                ;; escaped Unicode characters
                "\U0001f600\U0001f680\u3042"
                ;; raw UTF-8 characters
                "😸🚗愛"
                ]
               do (with-temp-file tmpfile
                    (insert (qrencode-format-as-netpbm (qrencode input nil nil 'return-raw))))
               do (should (string= (shell-command-to-string (format "%s -q '%s'" zbarimg tmpfile))
                                   (format "QR-Code:%s\n" input)))))))

(provide 'qrencode-tests)
;;; qrencode-tests.el ends here
