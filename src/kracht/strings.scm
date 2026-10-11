;; kracht

;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; kracht is free software: you can redistribute it and/or modify
;; it under the terms of the Lesser GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; kracht is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; Lesser GNU General Public License for more details.

;; You should have received a copy of the Lesser GNU General Public License
;; along with kracht.  If not, see <https://www.gnu.org/licenses/>.

(define-module (kracht strings)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 string-fun)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:re-export (string->utf8 utf8->string string-replace-substring)
  #:export (non-empty-string? empty-string?
                              string-drop-first-last
                              string-drop-first-last-n
                              string-drop-first-last-line
                              base64-encode
                              base64-decode))

;;;
;;; string checks
;;;

(define (non-empty-string? s)
  "Return #t if S is a string with at least one character, otherwise #f.
Note that any non-string value (including #t and numbers) returns #f."
  (and (string? s)
       (> (string-length s) 0)))

(define (empty-string? s)
  "Return #t if S is exactly the empty string \"\"."
  (equal? "" s))

;;;
;;; string manipulation
;;;

(define (string-drop-first-last s)
  "Return a substring of S with its first and last characters removed.
If the string length is 2 or less, returns S unaltered."
  (if (> (string-length s) 2)
      (string-take (string-drop s 1)
                   (- (string-length s) 2)) s))

(define (string-drop-first-last-n s n)
  "Return a substring of S with the first N characters and the final 1
character removed.  If the string length is 2 or less, returns S unaltered.
N is clamped to the valid range, so N is never allowed to drop the whole
string or to raise an error."
  (let ((len (string-length s)))
    (if (<= len 2) s
        (let ((start (max 0
                          (min n
                               (- len 1)))))
          (string-take (string-drop s start)
                       (- len start 1))))))

(define (string-drop-first-last-line s)
  "Split string S by newlines, remove its first and last lines, and rejoin
the remaining lines with newlines."
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2)
                     (drop-right (cdr lines) 1)
                     '())))
    (string-join middle "\n")))

;;;
;;; base64 (pure Scheme, no external binary required)
;;;

(define base64-alphabet
  "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789+/")

;; Maps an ASCII character code to its 6-bit base64 value, or #f.
(define base64-values
  (let ((v (make-vector 128 #f)))
    (let loop
      ((i 0))
      (when (< i 64)
        (vector-set! v
                     (char->integer (string-ref base64-alphabet i)) i)
        (loop (+ i 1)))) v))

(define (base64-decode-char c)
  "Return the 6-bit value of base64 character C, or #f if C is not valid."
  (let ((i (char->integer c)))
    (and (< i 128)
         (vector-ref base64-values i))))

(define (base64-encode s)
  "Encode string S into a base64 string without line wrapping.
This is implemented in pure Scheme, so it does not require the `base64`
binary to be in PATH and it cannot be broken by S containing a line that
happens to look like a shell heredoc terminator."
  (let* ((bv (string->utf8 s))
         (len (bytevector-length bv)))
    (call-with-output-string (lambda (port)
                               (let loop
                                 ((i 0))
                                 (when (< i len)
                                   (let* ((b0 (bytevector-u8-ref bv i))
                                          (b1 (if (< (+ i 1) len)
                                                  (bytevector-u8-ref bv
                                                                     (+ i 1))
                                                  0))
                                          (b2 (if (< (+ i 2) len)
                                                  (bytevector-u8-ref bv
                                                                     (+ i 2))
                                                  0))
                                          (n (+ (* b0 65536)
                                                (* b1 256) b2)))
                                     (display (string-ref base64-alphabet
                                                          (quotient n 262144))
                                              port)
                                     (display (string-ref base64-alphabet
                                                          (modulo (quotient n
                                                                            4096)
                                                                  64)) port)
                                     (if (< (+ i 1) len)
                                         (display (string-ref base64-alphabet
                                                              (modulo (quotient
                                                                       n 64)
                                                                      64))
                                                  port)
                                         (display #\= port))
                                     (if (< (+ i 2) len)
                                         (display (string-ref base64-alphabet
                                                              (modulo n 64))
                                                  port)
                                         (display #\= port))
                                     (loop (+ i 3)))))))))

(define (base64-decode s)
  "Decode a base64 encoded string S back into its raw UTF-8 string format.
Whitespace and newlines in S are ignored.  For backwards compatibility with
the previous shell-based implementation, trailing whitespace is trimmed from
the decoded output."
  (let ((clean (string-filter (lambda (c)
                                (not (char-whitespace? c))) s)))
    (let loop
      ((i 0)
       (acc 0)
       (nbits 0)
       (out '()))
      (if (>= i
              (string-length clean))
          (string-trim-right (bytevector->string (u8-list->bytevector (reverse
                                                                       out))
                                                 "utf-8"))
          (let ((c (string-ref clean i)))
            (if (char=? c #\=)
                (string-trim-right (bytevector->string (u8-list->bytevector (reverse
                                                                             out))
                                                       "utf-8"))
                (let ((v (base64-decode-char c)))
                  (if v
                      (let ((acc (+ (* acc 64) v))
                            (nbits (+ nbits 6)))
                        (if (>= nbits 8)
                            (let ((shift (expt 2
                                               (- nbits 8))))
                              (loop (+ i 1)
                                    (modulo acc shift)
                                    (- nbits 8)
                                    (cons (quotient acc shift) out)))
                            (loop (+ i 1) acc nbits out)))
                      (loop (+ i 1) acc nbits out)))))))))
