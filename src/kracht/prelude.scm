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

(define-module (kracht prelude)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-1)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 rdelim)
  #:use-module (srfi srfi-64))

;;
;; logging helpers
;;

;; (begin
;;   (define (log-message msg)
;;     (display (format #f "\n~a\n" msg)))
;;   (export log-message))

;; (begin
;;   (define (log-info msg . args)
;;     ;;    (log-message (format #f "[INFO] ~a" (format #f msg args)))
;;     (log-message (format #f msg args))
;;     )
;;   (export))

;;
;; string checks
;;

(define-public (non-empty-string? s)
  (and s
       (not (equal? "" s))))

;;
;; string manipulation
;;

(define-public (string-drop-first-last s)
  (if (> (string-length s) 2)

      (string-take (string-drop s 1)
                   (- (string-length s) 2)) s))

(define-public (string-drop-first-last-n s n)
  (if (> (string-length s) 2)
      (string-take (string-drop s n)
                   (- (string-length s)
                      (+ 1 n))) s))

(define-public (string-drop-first-last-line s)
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2)
                     (drop-right (cdr lines) 1)
                     '())))
    (string-join middle "\n")))

;;
;; module tests
;;

(test-begin "prelude")

;; non-empty-string? tests
(test-equal "non-empty-string? with non-empty string" #t
            (non-empty-string? "hello"))
(test-equal "non-empty-string? with empty string" #f
            (non-empty-string? ""))
(test-equal "non-empty-string? with whitespace" #t
            (non-empty-string? " "))
(test-equal "non-empty-string? with #f" #f
            (non-empty-string? #f))
(test-equal "non-empty-string? with #t" #t
            (non-empty-string? #t))

;; string-drop tests

(test-equal "string-drop-first-last normal case" "ell"
            (string-drop-first-last "hello"))
(test-equal "string-drop-first-last single char" "a"
            (string-drop-first-last "a"))
(test-equal "string-drop-first-last two chars" "ab"
            (string-drop-first-last "ab"))
(test-equal "string-drop-first-last empty string" ""
            (string-drop-first-last ""))

(test-equal "string-drop-first-last-line normal case" "middle line"
            (string-drop-first-last-line "first line\nmiddle line\nlast line"))
(test-equal "string-drop-first-last-line single line" ""
            (string-drop-first-last-line "single line"))
(test-equal "string-drop-first-last-line two lines" ""
            (string-drop-first-last-line "first line\nlast line"))
(test-equal "string-drop-first-last-line empty input" ""
            (string-drop-first-last-line ""))

;; log tests

;; (test-assert "log-info behaves as expected" (log-info "1 + 1 = ~a and 2 + 2 = ~a"
;;                                                       (+ 1 1) (+ 2 2)))

(test-end)
