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
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 rdelim)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 time)
  #:use-module (rnrs bytevectors)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-9)
  #:use-module (web uri)
  #:re-export (string-replace-substring string->utf8
                                        utf8->string
                                        uri-decode
                                        uri-encode
                                        with-output-to-port
                                        with-output-to-file
                                        with-output-to-string
                                        get-string-all
                                        open-input-pipe
                                        open-input-output-pipe)
  #:export (log-message log
                        log-info
                        log-error
                        flatten-list
                        non-empty-string?
                        base64-encode
                        base64-decode
                        empty-string?
                        string-drop-first-last
                        string-drop-first-last-n
                        string-drop-first-last-line
                        assoc-ref-or-empty-str
                        build-asset-library
                        syscall
                        shuffle-fy
                        alist-from-url-encoded-body
                        html-escape
                        html-unescape
                        evaluate-thunks
                        getenv-or
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
;; list helpers
;;

(define (flatten-list xs)
  (cond
    ((null? xs)
     '())
    ((pair? xs)
     (append (flatten-list (car xs))
             (flatten-list (cdr xs))))
    (else (list xs))))

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

(define html-escape-alist
  '(("'" "&apos;")
    ("\"" "&quot;")
    ("á" "&aacute;")
    ("à" "&agrave;")
    ("Á" "&Aacute;")
    ("À" "&Agrave;")
    ("é" "&eacute;")
    ("è" "&egrave;")
    ("É" "&Eacute;")
    ("È" "&Egrave;")
    ("í" "&iacute;")
    ("ì" "&igrave;")
    ("Í" "&Iacute;")
    ("Ì" "&Igrave;")
    ("ó" "&oacute;")
    ("ò" "&ograve;")
    ("Ó" "&Oacute;")
    ("Ò" "&Ograve;")
    ("ú" "&uacute;")
    ("ù" "&ugrave;")
    ("Ú" "&Uacute;")
    ("Ù" "&Ugrave;")
    ("ç" "&ccedil;")
    ("Ç" "&Ccedil;")
    ("ñ" "&ntilde;")
    ("Ñ" "&Ntilde;")
    ("<" "&lt;")
    (">" "&gt;")
    ("/" "&sol;")
    ("*" "&ast;")
    ("å" "&aring;")
    ("æ" "&aelig;")
    ("ë" "&euml;")
    ("ï" "&iuml;")
    ("ô" "&ocirc;")
    ("ö" "&ouml;")
    ("ø" "&oslash;")
    ("û" "&ucirc;")
    ("ü" "&uuml;")
    ("ä" "&auml;")
    ("â" "&acirc;")
    ("ß" "&szlig;")
    ("Ü" "&Uuml;")
    ("Ö" "&Ouml;")
    ("Ë" "&Euml;")
    ("Ä" "&Auml;")
    ("Ï" "&Iuml;")
    ("$" "&dollar;")
    ("%" "&percnt;")
    ("ã" "&atilde;")
    ("Ã" "&Atilde;")
    ("õ" "&otilde;")
    ("Õ" "&Otilde;")))

(define (html-escape x)
  (fold (lambda (entity accum)
          (string-replace-substring accum
                                    (car entity)
                                    (cadr entity))) x html-escape-alist))

(define (html-unescape x)
  (fold (lambda (entity accum)
          (string-replace-substring accum
                                    (cadr entity)
                                    (car entity))) x html-escape-alist))

(define (alist-from-url-encoded-body body)
  (define (unsafe-mk-alist x)
    (cons (car x)
          (cdr x)))
  (let* ((str (bytevector->string body "utf-8"))
         (raw-kv-pairs (string-split str #\&))
         (xss (map unsafe-mk-alist
                   (map (lambda (x)
                          (string-split x #\=)) raw-kv-pairs)))
         (read-from-pairs (lambda (x)
                            (catch #t
                                   (lambda ()
                                     `(,(uri-decode (car x)) unquote
                                       (uri-decode (cadr x))))
                                   (lambda (key . args)
                                     (display (format #f
                                               "
warning! could not decode pair ~a: ~a: ~a
"
                                               x key args))
                                     `((unquote (uri-decode (car x))) . "")))))
         (unescaped-xss (map read-from-pairs xss))
         (result (append unescaped-xss)))
    (if (equal? result
                '(("" . "")))
        '() result)))

(define-syntax-rule (evaluate-thunks thunks)
  "Given a list of functions that don't take any inputs, run each one of
those functions and return a list of what each function returned."
  (map (lambda (t)
         (apply t
                '())) thunks))

(define* (getenv-or var
                    #:key (default #f))
  (catch #t
         (lambda ()
           (let* ((v (getenv (format #f "~a" var))))
             (if (not v) default v)))
         (lambda (key . args)
           (display (format #f
                            "
error: ~a: ~a
could not read environment variable ~a
return default: ~a
"
                            key
                            args
                            var
                            default)) default)))

;; List shuffle which implements the modern Fisher-Yates shuffle algorithm.
;; It converts the list to a vector for efficient O(1) random access
;; and mutation, shuffles the vector in-place, and then converts it back to a list.
(define (shuffle-fy xs)
  "Returns a randomly shuffled copy of XS."
  (set! *random-state*
        (random-state-from-platform))
  (if (or (null? xs)
          (null? (cdr xs))) xs
      (let* ((vec (list->vector xs))
             (len (vector-length vec)))
        ;; Loop from the last element down to the second element.
        (do ((i (- len 1)
                (- i 1)))
            ((< i 1))
            ;; Pick a random index j from 0 to i (inclusive).
            (let* ((j (random (1+ i)))
                   ;; Swap the elements at positions i and j.
                   (temp (vector-ref vec i)))
              (vector-set! vec i
                           (vector-ref vec j))
              (vector-set! vec j temp)))
        ;; Convert the shuffled vector back to a list.
        (vector->list vec))))

(define (base64-encode s)
  (syscall (format #f "cat << \\EOFEOF | base64 --wrap=0
~a
EOFEOF" s)))

(define (base64-decode s)
  (string-trim-right (syscall (format #f
                               "cat << \\EOFEOF | base64 --decode --wrap=0
~a
EOFEOF" s))))
