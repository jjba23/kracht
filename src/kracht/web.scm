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

;; Characters `html-escape' turns into named entities.  The first five are the
;; characters HTML requires escaping in text and attribute contexts; the rest
;; are the accented Latin letters kracht has always emitted as entities.
(define html-escape-alist
  '(("&" "&amp;")
    ("<" "&lt;")
    (">" "&gt;")
    ("'" "&apos;")
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

;; Entities `html-unescape' understands but `html-escape' no longer produces.
;; `&sol;', `&ast;', `&dollar;' and `&percnt;' are not HTML-significant and
;; escaping them corrupted plain text, so they are decode-only now; the
;; uppercase forms are legacy HTML spellings of the basic five.
(define html-unescape-only-alist
  '(("/" "&sol;")
    ("*" "&ast;")
    ("$" "&dollar;")
    ("%" "&percnt;")
    ("&" "&AMP;")
    ("<" "&LT;")
    (">" "&GT;")
    ("\"" "&QUOT;")))

(define html-unescape-entities
  (map (lambda (entity)
         (cons (cadr entity)
               (string-ref (car entity) 0)))
       (append html-escape-alist html-unescape-only-alist)))

(define (html-escape x)
  "Replace the characters of string X that are significant to HTML --
the ampersand, less-than, greater-than, double-quote and apostrophe
characters -- plus the accented Latin letters kracht renders as named
entities, with their entity equivalents.

X is processed one character at a time, so escaping is deliberately *not*
idempotent: `&amp;' becomes `&amp;amp;'.  Escape raw text exactly once and
never escape output that has already been escaped."
  (call-with-output-string (lambda (port)
                             (string-for-each (lambda (c)
                                                (let ((entity (assoc c
                                                               html-escape-chars)))
                                                  (display (if entity
                                                               (cdr entity) c)
                                                           port))) x))))

(define (numeric-character-reference name)
  "Decode the full entity string NAME (for example `&#39;' or `&#x27;') into
its character, or return #f when NAME is not a well-formed numeric character
reference (the digits must be non-empty, in range, and of the right radix)."
  (and (>= (string-length name) 4)
       (char=? (string-ref name 1) #\#)
       (let* ((body (substring name 2 (- (string-length name) 1)))
              (hex? (and (> (string-length body) 0)
                         (memv (string-ref body 0) '(#\x #\X))))
              (digits (if hex? (substring body 1) body))
              (radix (if hex? 16 10))
              (digit-chars (if hex? "0123456789abcdefABCDEF" "0123456789")))
         (and (> (string-length digits) 0)
              (string-every (lambda (d)
                              (string-index digit-chars d)) digits)
              (let ((n (string->number digits radix)))
                (and n (> n 0) (<= n #x10FFFF) (integer->char n)))))))

(define (html-unescape x)
  "Convert HTML entities inside string X back into their original characters.
Both named entities (`&lt;', `&aacute;') and numeric character references
(`&#39;', `&#x27;') are decoded.  The string is processed in a single pass;
unknown or malformed entities are copied through unchanged."
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
                                                      (entity (or (assoc-ref
                                                                   html-unescape-entities
                                                                   name)
                                                                  (numeric-character-reference
                                                                   name))))
                                                 (if entity
                                                     (begin
                                                       (display entity port)
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
