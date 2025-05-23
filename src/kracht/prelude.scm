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
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 rdelim)
  #:export (log-message log
                        log-info
                        log-error
                        non-empty-string?
                        empty-string?
                        string-drop-first-last
                        string-drop-first-last-n
                        string-drop-first-last-line
                        assoc-ref-or-empty-str
                        build-asset-library
                        syscall
                        get-short-commit-sha))

;;
;; logging helpers
;;

(define (log-message msg)
  (display (format #f "\n~a\n" msg)))

(define (log msg . args)
  (log-message (apply format
                      (append (list #f msg) args))))

(define (log-info msg . args)
  (log-message (format #f "[INFO] ~a"
                       (apply format
                              (append (list #f msg) args)))))

(define (log-error msg . args)
  (log-message (format #f "[ERROR] ~a"
                       (apply format
                              (append (list #f msg) args)))))

;;
;; string checks
;;

(define (non-empty-string? s)
  (and s
       (not (equal? "" s))))

(define (empty-string? s)
  (equal? "" s))

;;
;; string manipulation
;;

(define (string-drop-first-last s)
  (if (> (string-length s) 2)

      (string-take (string-drop s 1)
                   (- (string-length s) 2)) s))

(define (string-drop-first-last-n s n)
  (if (> (string-length s) 2)
      (string-take (string-drop s n)
                   (- (string-length s)
                      (+ 1 n))) s))

(define (string-drop-first-last-line s)
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2)
                     (drop-right (cdr lines) 1)
                     '())))
    (string-join middle "\n")))

;;
;; alist helpers
;;

(define (assoc-ref-or-empty-str alist field)
  (let ((sut (assoc-ref alist field)))
    (if (equal? #f sut) "" sut)))

;;
;; Asset library is an in-memory cache, a lookup dictionary
;; that can be used for faster performance, memoization, etc
;; this version will read from a list of files specifies with
;; the FILES param
;;
;; to use it, you can simply call the function
;; with a list of file paths:
;;
;; (define my-asset-library
;;   (build-asset-library '("pub/css/fonts.css" "pub/js/tailwind.js")))
;;

(define* (build-asset-library files)
  (let* ((lib '()))
    (for-each (lambda (f)
                (let* ((file-contents (get-string-all (open-input-file f)))
                       (min-contents (cond
                                       ((string-suffix? ".css" f)
                                        (string-replace-substring
                                         file-contents "\n" ""))
                                       (else file-contents))))
                  (set! lib
                        (assoc-set! lib f min-contents)))) files) lib))

;; syscall

(define (syscall cmd)
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process) process-output))

(define (get-short-commit-sha)
  (syscall "git log --pretty=format:'%h' -n 1"))
