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

(define-module (veritas unit modules-spec)
  #:use-module (veritas veritas)
  #:use-module (kracht strings)
  #:use-module (kracht lists)
  #:use-module (kracht sql)
  #:use-module (kracht web)
  #:use-module (kracht html-entities)
  #:use-module (kracht io)
  #:export (spec))

(define (module-exports name)
  (let ((iface (resolve-interface name)))
    (map car (module-map (lambda (k v) (cons k v)) iface))))

(define modules-suite
  (suite "Kracht sub-modules"
         (test "(kracht strings) works standalone"
               (assert-equal #:name "non-empty-string?"
                             #:expect #t
                             #:got (non-empty-string? "x"))
               (assert-equal #:name "string-drop-first-last"
                             #:expect "ell"
                             #:got (string-drop-first-last "hello"))
               (assert-equal #:name "base64-encode"
                             #:expect "aGVsbG8="
                             #:got (base64-encode "hello"))
               (assert-equal #:name "base64-decode"
                             #:expect "hello"
                             #:got (base64-decode "aGVsbG8=")))

         (test "(kracht lists) works standalone"
               (assert-equal #:name "flatten-list"
                             #:expect '(1 2 3)
                             #:got (flatten-list '(1 (2 (3)))))
               (assert-equal #:name "assoc-ref-or"
                             #:expect 1
                             #:got (assoc-ref-or '((a . 1)) 'a))
               (assert-equal #:name "alist?"
                             #:expect #t
                             #:got (alist? '((a . 1) (b . 2)))))

         (test "(kracht sql) works standalone"
               (assert-equal #:name "symbol->sql-column"
                             #:expect "first_name"
                             #:got (symbol->sql-column 'first-name))
               (assert-equal #:name "sql-column->symbol"
                             #:expect 'first-name
                             #:got (sql-column->symbol "first_name"))
               (assert-equal #:name "value->sql-literal escapes quotes"
                             #:expect "'O''Brien'"
                             #:got (value->sql-literal "O'Brien"))
               (assert-equal #:name "quote-sql-identifier"
                             #:expect "\"select\""
                             #:got (quote-sql-identifier 'select))
               (assert-equal #:name "sql-select binds its value"
                             #:expect
                             (list "SELECT * FROM \"t\" WHERE \"id\" = ?" '(1))
                             #:got (let ((q (sql-select 't #:where '((id . 1)))))
                                     (list (sql-query-sql q)
                                           (sql-query-params q))))
               (assert-equal #:name "sql-insert-many groups one VALUES per row"
                             #:expect
                             "INSERT INTO \"t\" (\"a\") VALUES (?), (?)"
                             #:got (sql-query-sql
                                    (sql-insert-many 't
                                                     (list '((a . 1)) '((a . 2))))))
               (assert-equal #:name "sql-upsert renders ON CONFLICT"
                             #:expect
                             "INSERT INTO \"t\" (\"id\", \"a\") VALUES (?, ?) ON CONFLICT (\"id\") DO UPDATE SET \"a\" = EXCLUDED.\"a\""
                             #:got (sql-query-sql
                                    (sql-upsert 't '((id . 1) (a . 2))
                                                #:conflict-target 'id))))

         (test "(kracht web) works standalone"
               (assert-equal #:name "html-escape"
                             #:expect "&apos;"
                             #:got (html-escape "'"))
               (assert-equal #:name "html-escape ampersand"
                             #:expect "a &amp; b"
                             #:got (html-escape "a & b"))
               (assert-equal #:name "html-unescape"
                             #:expect "<"
                             #:got (html-unescape "&lt;"))
               (assert-equal #:name "html-unescape numeric reference"
                             #:expect "'"
                             #:got (html-unescape "&#39;"))
               (assert-equal #:name "html-escape-context javascript"
                             #:expect "a\\x22b"
                             #:got (html-escape-context "a\"b"
                                                        #:context 'js-string))
               (assert-equal #:name "html-escape-css-value"
                             #:expect "100\\000025 "
                             #:got (html-escape-css-value "100%"))
               (assert-equal #:name "alist-from-url-encoded-body"
                             #:expect '(("a" . "1"))
                             #:got (alist-from-url-encoded-body
                                    (string->utf8 "a=1"))))

         (test "(kracht html-entities) works standalone"
               (assert-equal #:name "html-entity-ref reads a reference"
                             #:expect "&"
                             #:got (html-entity-ref "&amp;"))
               (assert-equal #:name "including one the HTML4 set has no name for"
                             #:expect (string (integer->char 8230))
                             #:got (html-entity-ref "&hellip;"))
               (assert-equal #:name "an unknown reference is #f"
                             #:expect #f
                             #:got (html-entity-ref "&nope;"))
               (assert-equal #:name "html-char-ref writes a reference"
                             #:expect "&lt;"
                             #:got (html-char-ref #\<))
               (assert-equal #:name "the escape table is the HTML4 set"
                             #:expect 251
                             #:got (length html-escape-entities))
               (assert-equal #:name "and the decode table is the HTML5 one"
                             #:expect 2125
                             #:got (length html-named-entities)))

         (test "(kracht io) works standalone"
               (assert-true (string? (syscall "echo hi"))
                            #:name "syscall returns a string")
               (assert-true (string? (get-short-commit-sha))
                            #:name "get-short-commit-sha returns a string"))

         (test "the split is clean: modules do not leak each other's names"
               (assert-false (memq 'html-escape
                                   (module-exports '(kracht strings)))
                             #:name "(kracht strings) does not export html-escape")
               (assert-false (memq 'flatten-list
                                   (module-exports '(kracht sql)))
                             #:name "(kracht sql) does not export flatten-list")
               (assert-false (memq 'base64-encode
                                   (module-exports '(kracht io)))
                             #:name "(kracht io) does not export base64-encode")
               (assert-true (and (memq 'base64-encode
                                       (module-exports '(kracht strings)))
                                 #t)
                            #:name "(kracht strings) exports base64-encode")
               (assert-true (and (memq 'value->sql-literal
                                       (module-exports '(kracht sql)))
                                 #t)
                            #:name "(kracht sql) exports value->sql-literal")
               (assert-true (and (memq 'quote-sql-identifier
                                       (module-exports '(kracht sql)))
                                 #t)
                            #:name "(kracht sql) exports quote-sql-identifier")
               (assert-true (and (memq 'sql-select
                                       (module-exports '(kracht sql)))
                                 #t)
                            #:name "(kracht sql) exports sql-select")
               (assert-true (and (memq 'sql-upsert-many
                                       (module-exports '(kracht sql)))
                                 #t)
                            #:name "(kracht sql) exports sql-upsert-many")
               (assert-true (and (memq 'sql-insert-many
                                       (module-exports '(kracht prelude)))
                                 #t)
                            #:name "(kracht prelude) re-exports sql-insert-many")
               (assert-true (and (memq 'sql-parameterize
                                       (module-exports '(kracht sql)))
                                 #t)
                            #:name "(kracht sql) exports sql-parameterize")
               (assert-true (and (memq 'sql-insert
                                       (module-exports '(kracht prelude)))
                                 #t)
                            #:name "(kracht prelude) re-exports sql-insert")
               (assert-true (and (memq 'html-escape-context
                                       (module-exports '(kracht web)))
                                 #t)
                            #:name "(kracht web) exports html-escape-context")
               (assert-false (memq 'html-escape-js-string
                                   (module-exports '(kracht sql)))
                             #:name "(kracht sql) does not export the web encoders")
               (assert-true (and (memq 'html-entity-ref
                                       (module-exports '(kracht prelude)))
                                 #t)
                            #:name "(kracht prelude) re-exports html-entity-ref")
               (assert-true (and (memq 'html-char-ref
                                       (module-exports '(kracht html-entities)))
                                 #t)
                            #:name "(kracht html-entities) exports html-char-ref")
               (assert-false (memq 'html-escape
                                   (module-exports '(kracht html-entities)))
                             #:name "(kracht html-entities) is tables only, not the escapers"))))

(define (spec)
  (veritas-run modules-suite))
