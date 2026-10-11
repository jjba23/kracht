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

(define-module (kracht io)
  #:use-module (ice-9 format)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 ports)
  #:use-module (ice-9 string-fun)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-1)
  #:re-export (with-output-to-port with-output-to-file with-output-to-string
                                   get-string-all open-input-pipe
                                   open-input-output-pipe)
  #:export (read-file-string write-file-string build-asset-library syscall
                             get-short-commit-sha getenv-or))

;;;
;;; file helpers
;;;

(define (read-file-string path)
  "Read the entire contents of the file at PATH and return it as a string.
The input port is always closed, even if an error is raised while reading."
  (call-with-input-file path
    get-string-all))

(define (write-file-string path contents)
  "Write the string CONTENTS to the file at PATH, replacing any previous
contents.  Returns the number of characters written."
  (call-with-output-file path
    (lambda (port)
      (display contents port)))
  (string-length contents))

(define* (build-asset-library files)
  "Asset library is an in-memory cache, a lookup dictionary
that can be used for faster performance, memoization, etc.
This version will read from a list of files specified with
the FILES param.

To use it, you can simply call the function with a list of file paths."
  (fold (lambda (f lib)
          (let* ((file-contents (read-file-string f))
                 (min-contents (if (string-suffix? ".css" f)
                                   (string-replace-substring file-contents
                                                             "\n" "")
                                   file-contents)))
            (assoc-set! lib f min-contents)))
        '() files))

;; (define my-asset-library
;;   (build-asset-library '(\"pub/css/fonts.css\" \"pub/js/tailwind.js\")))

;;;
;;; syscall and environment
;;;

(define (syscall cmd)
  "Execute system shell command CMD via an input pipe and return
the captured standard output as a string."
  (let* ((process (open-input-pipe cmd))
         (process-output (get-string-all process)))
    (close-pipe process) process-output))

(define* (get-short-commit-sha #:key (default ""))
  "Retrieve the short Git commit SHA (7 characters) of the current repository
state, with surrounding whitespace trimmed.  If no SHA can be determined
(for example outside of a Git repository), return DEFAULT."
  (let ((sha (string-trim (syscall "git log --pretty=format:'%h' -n 1"))))
    (if (string-null? sha) default sha)))

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
