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
                                        fold
                                        fold-right
                                        open-input-pipe
                                        open-input-output-pipe)
  #:export (flatten-list non-empty-string?
                         base64-encode
                         base64-decode
                         empty-string?
                         string-drop-first-last
                         string-drop-first-last-n
                         string-drop-first-last-line
                         assoc-ref-or-empty-str
                         build-asset-library
                         assoc-lens
                         syscall
                         shuffle-fy
                         alist-from-url-encoded-body
                         html-escape
                         html-unescape
                         evaluate-thunks
                         getenv-or
                         get-short-commit-sha
                         symbol->sql-column
                         sql-column->symbol
                         symbols->sql-columns-list
                         alist?
                         sql-row->scheme-alist
                         pair->sql-assignment
                         value->sql-literal))

;;
;; string checks
;;

(define (non-empty-string? s)
  "Return #t if S is a valid string and not equal to \"\", otherwise return #f."
  (and s
       (not (equal? "" s))))

(define (empty-string? s)
  "Return #t if S is exactly the empty string \"\"."
  (equal? "" s))

;;
;; string manipulation
;;

(define (string-drop-first-last s)
  "Return a substring of S with its first and last characters removed.
If the string length is 2 or less, returns S unaltered."
  (if (> (string-length s) 2)
      (string-take (string-drop s 1)
                   (- (string-length s) 2)) s))

(define (string-drop-first-last-n s n)
  "Return a substring of S with the first N characters and the final 1 character removed.
If the string length is 2 or less, returns S unaltered."
  (if (> (string-length s) 2)
      (string-take (string-drop s n)
                   (- (string-length s)
                      (+ 1 n))) s))

(define (string-drop-first-last-line s)
  "Split string S by newlines, remove its first and last lines, and rejoin
the remaining lines with newlines."
  (let* ((lines (string-split s #\newline))
         (middle (if (> (length lines) 2)
                     (drop-right (cdr lines) 1)
                     '())))
    (string-join middle "\n")))

;;
;; list helpers
;;

(define (flatten-list xs)
  "Recursively flatten a nested list structure XS into a flat, one-dimensional list."
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

(define (assoc-lens xs keys)
  "Deeply look up a value within nested association lists XS by traversing
the sequence of KEYS sequentially."
  (fold (lambda (k xxs)
          (assoc-ref xxs k)) xs keys))

(define (assoc-ref-or-empty-str alist field)
  "Look up FIELD in ALIST. Return the associated value if found,
or an empty string \"\" if the field does not exist or matches #f."
  (let ((sut (assoc-ref alist field)))
    (if (equal? #f sut) "" sut)))

(define* (build-asset-library files)
  "Asset library is an in-memory cache, a lookup dictionary
that can be used for faster performance, memoization, etc.
This version will read from a list of files specified with
the FILES param.

To use it, you can simply call the function with a list of file paths."
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

;; (define my-asset-library
;;   (build-asset-library '("pub/css/fonts.css" "pub/js/tailwind.js")))

;; syscall

(define (syscall cmd)
  "Execute system shell command CMD via an input pipe and return
the captured standard output as a string."
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process) process-output))

(define (get-short-commit-sha)
  "Retrieve the short Git commit SHA (7 characters) of the current repository state."
  (syscall "git log --pretty=format:'%h' -n 1"))

;;
;;
;;

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
  "Replace special or non-ASCII characters in string X with their HTML entity equivalents."
  (fold (lambda (entity accum)
          (string-replace-substring accum
                                    (car entity)
                                    (cadr entity))) x html-escape-alist))

(define (html-unescape x)
  "Convert HTML entities inside string X back into their original character equivalents."
  (fold (lambda (entity accum)
          (string-replace-substring accum
                                    (cadr entity)
                                    (car entity))) x html-escape-alist))

(define (unsafe-mk-alist x)
  (cons (car x)
        (cdr x)))

(define (alist-from-url-encoded-body body)
  "Parse a URL-encoded bytevector HTTP BODY into an association list of key-value strings."

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
                    #:key default)
  "Get env VAR or return a DEFAULT value."
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

(define (shuffle-fy xs)
  "Returns a randomly shuffled copy of XS.
List shuffle which implements the modern Fisher-Yates shuffle algorithm.
It converts the list to a vector for efficient O(1) random access
and mutation, shuffles the vector in-place, and then converts it back to a list."
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
  "Encode string S into a base64 string without line wrapping by system execution."
  (syscall (format #f "cat << \\EOFEOF | base64 --wrap=0
~a
EOFEOF" s)))

(define (base64-decode s)
  "Decode base64 encoded string S back into its raw string format by system execution.
Trims trailing right whitespaces from output."
  (string-trim-right (syscall (format #f
                               "cat << \\EOFEOF | base64 --decode --wrap=0
~a
EOFEOF" s))))

;;
;; SQL
;;

(define (symbol->sql-column x)
  "Convert Scheme symbol X into a SQL column name string by replacing
hyphens with underscores."
  (string-replace-substring (symbol->string x) "-" "_"))

(define (sql-column->symbol x)
  "Convert SQL column name string X into a Scheme symbol by replacing
underscores with hyphens."
  (string->symbol (string-replace-substring x "_" "-")))

(define (symbols->sql-columns-list xs)
  "Convert a list of symbols XS into a single string of comma-separated
SQL column names."
  (string-join (map symbol->sql-column xs) ", "))

(define (alist? obj)
  "Return #t if OBJ is a valid association list (a list where every
element is a pair), otherwise return #f."
  (cond
    ((null? obj)
     #t)
    ((not (pair? obj))
     #f)
    ((not (pair? (car obj)))
     #f)
    (else (alist? (cdr obj)))))

(define (sql-row->scheme-alist xs)
  "Transform a SQL row representation XS into a Scheme-idiomatic association list.
If XS is an alist, it converts its column keys into hyphenated Scheme symbols.
If XS is not an alist, it returns XS unaltered."
  (cond
    ((alist? xs)
     (map (lambda (pair)
            (cons (sql-column->symbol (car pair))
                  (cdr pair))) xs))
    (else xs)))

(define (pair->sql-assignment a)
  "Convert a key-value pair A into a SQL assignment clause string
formatted as \"column_name = value\".

The key symbol is mapped via `symbol->sql-column`. Booleans are converted
to 1 or 0, numbers are unquoted, and all other types are single-quoted."
  (cond
    ((number? (cdr a))
     (format #f "~a = ~a"
             (symbol->sql-column (car a))
             (cdr a)))
    ((eq? #t
          (cdr a))
     (format #f "~a = 1"
             (symbol->sql-column (car a))))
    ((eq? #f
          (cdr a))
     (format #f "~a = 0"
             (symbol->sql-column (car a))))
    (else (format #f "~a = '~a'"
                  (symbol->sql-column (car a))
                  (cdr a)))))

(define (value->sql-literal a)
  "Convert a Scheme value A into its raw SQL literal string representation.
Numbers are kept raw, booleans are mapped to \"1\" or \"0\", and other
types are wrapped in single quotes. You should escape single quotes if needed yourself."
  (cond
    ((number? a)
     (format #f "~a" a))
    ((eq? #t a)
     "1")
    ((eq? #f a)
     "0")
    (else (format #f "'~a'" a))))
