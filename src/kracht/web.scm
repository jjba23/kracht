;; kracht
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Escaping a value for the place it is written to.
;;
;; Escaping is a property of the position, not of the string: a value that ends
;; up in element text, in a quoted attribute, in an unquoted attribute, inside a
;; JavaScript string, in a CSS value or in a URL component each has its own way
;; of ending the position early.  `html-escape-context' names the position and
;; calls the encoder that matches it.
;;
;; ```mermaid
;; flowchart LR
;;   value["a value"] --> where{"where is it written?"}
;;   where -- text or quoted attribute --> escape["html-escape"]
;;   where -- unquoted attribute --> unquoted["html-escape-unquoted-attribute"]
;;   where -- js-string --> js["html-escape-js-string"]
;;   where -- css-value --> css["html-escape-css-value"]
;;   where -- url --> url["uri-encode"]
;;   escape --> document["the document"]
;;   unquoted --> document
;;   js --> document
;;   css --> document
;;   url --> document
;; ```
;;
;; An unknown context is an error rather than a fallback, because a value
;; escaped for the wrong position is a value that is not escaped at all.
;;
;;; Code:

(define-module (kracht web)
  #:use-module (ice-9 format)
  #:use-module (ice-9 iconv)
  #:use-module (srfi srfi-1)
  #:use-module (web uri)
  #:use-module (kracht html-entities)
  #:re-export (uri-decode uri-encode)
  #:export (html-escape html-unescape
                        html-escape-context
                        html-escape-unquoted-attribute
                        html-escape-js-string
                        html-escape-css-value
                        alist-from-url-encoded-body))

;;;
;;; HTML escaping
;;;

;; The entity tables live in (kracht html-entities): `html-escape-entities' is
;; the list of characters worth writing as an entity, while
;; `html-named-entities' is the full set of references the HTML standard
;; defines.  `html-char-ref' and `html-entity-ref' look them up.

(define (html-escape x)
  "Replace the characters of string X that are significant to HTML -- the
ampersand, less-than, greater-than, double-quote and apostrophe characters --
plus every character in the HTML4 entity set (the accented Latin letters, and
the common symbols from currency signs to Greek letters and arrows) with their
named-entity equivalents.

This covers two contexts: HTML element text, and a *quoted* attribute value.
For an unquoted attribute, a JavaScript string, a CSS value or a URL, use
`html-escape-context' instead; those contexts have their own ways to end a
value early that escaping the five HTML special characters does not address.

X is processed one character at a time, so escaping is deliberately *not*
idempotent: `&amp;' becomes `&amp;amp;'.  Escape raw text exactly once and
never escape output that has already been escaped."
  (call-with-output-string (lambda (port)
                             (string-for-each (lambda (c)
                                                (let ((entity (html-char-ref c)))
                                                  (display (if entity entity c)
                                                           port)))
                                              x))))

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

Every named character reference the HTML standard defines is recognised, as long
as it is written with its trailing semicolon -- `&lt;', `&aacute;', `&hellip;',
even `&NotEqualTilde;' -- together with numeric references such as `&#39;' and
`&#x27;'.  Some of those references stand for two code points, so the result can
be longer than the text that was decoded.

The string is processed in a single pass; an unknown or malformed entity is
copied through unchanged."
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
                                                      (entity (or (html-entity-ref
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
;;; Context-aware output encoding
;;;
;;; `html-escape' above is right for HTML text and for quoted attribute values,
;;; but the same input is not safe in every position: a string inside a
;;; <script> block, a CSS value, an unquoted attribute and a URL each have their
;;; own way to end a value early.  The encoders below cover those positions.
;;; Encode for the context the value is going into, and encode it last.

(define (ascii-alphanumeric? c)
  "Return #t when C is an ASCII letter or digit."
  (let ((n (char->integer c)))
    (or (and (>= n 48) (<= n 57))
        (and (>= n 65) (<= n 90))
        (and (>= n 97) (<= n 122)))))

(define (hex-digits n width)
  "Render the non-negative integer N as upper-case hexadecimal, left-padded
with zeroes to at least WIDTH digits."
  (let ((s (string-upcase (number->string n 16))))
    (string-append (make-string (max 0 (- width (string-length s))) #\0) s)))

(define (encode-each x encode-char)
  "Return X with each character replaced by the string (ENCODE-CHAR c)."
  (call-with-output-string (lambda (port)
                             (string-for-each (lambda (c)
                                                (display (encode-char c) port))
                                              x))))

(define (html-escape-unquoted-attribute x)
  "Encode string X for an unquoted HTML attribute value.

Every character that is not an ASCII letter, digit, `-', `_' or `.' becomes a
numeric character reference, so the value cannot be ended early by whitespace
or by a delimiter such as `>' or `/'.  Quoting the attribute and using
`html-escape' is usually the better fix; this is for the cases where you
cannot."
  (encode-each x (lambda (c)
                   (if (or (ascii-alphanumeric? c) (memv c '(#\- #\_ #\.)))
                       (string c)
                       (string-append "&#x"
                                      (hex-digits (char->integer c) 2)
                                      ";")))))

(define (html-escape-js-string x)
  "Encode string X for the inside of a JavaScript string literal.

Every character except an ASCII letter or digit is written as a `\\xHH',
`\\uHHHH' or surrogate-pair escape.  The result can therefore not close the
literal, cannot carry a line separator into the code, and cannot end the
enclosing <script> element with `</script' -- `<' and `/' both become escapes.
Use it for the contents of a quoted string; write the quotes yourself.

Because only ASCII letters, digits and backslash escapes survive, the result is
also safe to drop straight into an HTML attribute -- quoted or not -- with no
further escaping: no quote, angle bracket, ampersand or whitespace is left to
break out with."
  (encode-each x (lambda (c)
                   (let ((code-point (char->integer c)))
                     (cond
                       ((ascii-alphanumeric? c) (string c))
                       ((<= code-point #xFF)
                        (string-append "\\x" (hex-digits code-point 2)))
                       ((<= code-point #xFFFF)
                        (string-append "\\u" (hex-digits code-point 4)))
                       (else (let* ((v (- code-point #x10000))
                                    (high (+ #xD800 (quotient v 1024)))
                                    (low (+ #xDC00 (remainder v 1024))))
                               (string-append "\\u" (hex-digits high 4)
                                              "\\u" (hex-digits low 4)))))))))

(define (html-escape-css-value x)
  "Encode string X for use as a CSS value: an inline `style' declaration or a
rule body.

Every character except an ASCII letter or digit becomes a CSS hex escape, six
hex digits followed by a space (CSS consumes that one space as the escape's
terminator).  The value can therefore not close the declaration, the rule or
the enclosing <style> element.

Note that the space that terminates each escape survives in the result, so put
it in a *quoted* attribute when it goes into a `style' attribute rather than
leaning on the attribute's own quoting."
  (encode-each x (lambda (c)
                   (if (ascii-alphanumeric? c)
                       (string c)
                       (string-append "\\" (hex-digits (char->integer c) 6)
                                      " ")))))

(define* (html-escape-context x #:key (context 'text))
  "Encode string X for the given output CONTEXT, picking the encoder that
matches where the value is about to be written:

  text                 HTML element text
  attribute            a quoted HTML attribute value
  unquoted-attribute   an unquoted HTML attribute value
  js-string            the inside of a JavaScript string literal
  css-value            a CSS value or an inline style declaration
  url                  a URL component: a path segment, a query parameter
                       name or value, or a fragment

`url' delegates to `uri-encode', which percent-encodes every byte outside the
RFC 3986 unreserved set.  Use it for a component and not for a whole URL, whose
`://', `?' and `&' delimiters must survive."
  (case context
    ((text) (html-escape x))
    ((attribute) (html-escape x))
    ((unquoted-attribute) (html-escape-unquoted-attribute x))
    ((js-string) (html-escape-js-string x))
    ((css-value) (html-escape-css-value x))
    ((url) (uri-encode x))
    (else (error "kracht web: unknown escaping context" context))))

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
