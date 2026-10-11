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

(define-module (kracht web)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:re-export (uri-decode uri-encode)
  #:export (html-escape html-unescape alist-from-url-encoded-body))

;;;
;;; HTML escaping
;;;

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

;; Character-keyed lookup tables derived from `html-escape-alist`.  Building
;; them once avoids re-scanning the whole alist for every character.
(define html-escape-chars
  (map (lambda (entity)
         (cons (string-ref (car entity) 0)
               (cadr entity))) html-escape-alist))

(define html-unescape-entities
  (map (lambda (entity)
         (cons (cadr entity)
               (string-ref (car entity) 0))) html-escape-alist))

(define (html-escape x)
  "Replace special or non-ASCII characters in string X with their HTML
entity equivalents.  The string is processed in a single pass, so an
already-escaped entity in X is not escaped a second time."
  (call-with-output-string (lambda (port)
                             (string-for-each (lambda (c)
                                                (let ((entity (assoc c
                                                               html-escape-chars)))
                                                  (display (if entity
                                                               (cdr entity) c)
                                                           port))) x))))

(define (html-unescape x)
  "Convert HTML entities inside string X back into their original character
equivalents.  The string is processed in a single pass; unknown or malformed
entities are left untouched."
  (let ((len (string-length x)))
    (call-with-output-string (lambda (port)
                               (let loop
                                 ((i 0))
                                 (when (< i len)
                                   (let ((c (string-ref x i)))
                                     (if (char=? c #\&)
                                         (let ((semi (string-index x #\; i)))
                                           (if semi
                                               (let* ((name (substring x i
                                                                       (+ semi
                                                                        1)))
                                                      (entity (assoc name
                                                               html-unescape-entities)))
                                                 (if entity
                                                     (begin
                                                       (display (cdr entity)
                                                                port)
                                                       (loop (+ semi 1)))
                                                     (begin
                                                       (display c port)
                                                       (loop (+ i 1)))))
                                               (begin
                                                 (display c port)
                                                 (loop (+ i 1)))))
                                         (begin
                                           (display c port)
                                           (loop (+ i 1)))))))))))

;;;
;;; URL-encoded bodies
;;;

(define (uri-decode-safe s)
  "Decode percent-encoded string S, returning S unaltered if decoding fails."
  (catch #t
         (lambda ()
           (uri-decode s))
         (lambda (key . args)
           (display (format #f "
warning! could not uri-decode ~a: ~a: ~a
" s key
                            args)
                    (current-error-port)) s)))

(define (alist-from-url-encoded-body body)
  "Parse a URL-encoded bytevector HTTP BODY into an association list of
decoded key/value string pairs.

Only the first `=` in each `&`-separated segment separates the key from the
value, so a value may itself contain `=` characters.  A segment without any
`=` gets an empty-string value, and empty segments are skipped.  A body
consisting solely of `=` is treated as empty and returns '()."
  (let ((str (bytevector->string body "utf-8")))
    (if (string-null? str)
        '()
        (let ((result (filter-map (lambda (segment)
                                    (if (string-null? segment) #f
                                        (let* ((eq (string-index segment #\=))
                                               (key (if eq
                                                        (substring segment 0
                                                                   eq) segment))
                                               (value (if eq
                                                          (substring segment
                                                                     (+ eq 1))
                                                          "")))
                                          (cons (uri-decode-safe key)
                                                (uri-decode-safe value)))))
                                  (string-split str #\&))))
          (if (equal? result
                      '(("" . "")))
              '() result)))))
