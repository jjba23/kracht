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

(define-module (kracht sql)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (kracht lists)
  #:export (symbol->sql-column sql-column->symbol
                               symbols->sql-columns-list
                               sql-row->scheme-alist
                               escape-sql-string
                               quote-sql-identifier
                               pair->sql-assignment
                               value->sql-literal))

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

(define (sql-text s)
  "Render S as the string a SQL literal would carry: strings pass through,
anything else is formatted with `format`.  Raises an error if S contains a NUL
character, which no SQL text type can store."
  (let ((str (if (string? s) s
                 (format #f "~a" s))))
    (when (string-index str #\nul)
      (error "kracht sql: a SQL text value cannot contain NUL characters" s))
    str))

(define* (escape-sql-string s #:key escape-backslash?)
  "Escape string S for inclusion inside a single-quoted SQL literal by
doubling each single quote (e.g. \"O'Brien\" becomes \"O''Brien\").  Non-string
values are formatted with `format` first.

Doubling quotes alone is correct for PostgreSQL, SQLite, and standard SQL,
where a backslash is an ordinary character.  Pass #:escape-backslash? #t for
MySQL/MariaDB, whose default parser also treats a backslash as an escape
character inside string literals.

A NUL character is rejected with an error rather than emitted, since no SQL
text type can store one."
  (let ((quoted (string-replace-substring (sql-text s) "'" "''")))
    (if escape-backslash?
        (string-replace-substring quoted "\\" "\\\\")
        quoted)))

(define (quote-sql-identifier name)
  "Render NAME (a symbol or string) as a safely delimited SQL identifier,
wrapped in ANSI double quotes with any embedded double quote doubled.

Use this for column and table names that come from data.  `symbol->sql-column'
only maps hyphens to underscores and does not defend against a name that
contains quotes or other punctuation.  A NUL character is rejected."
  (let ((str (if (symbol? name) (symbol->string name)
                 (format #f "~a" name))))
    (when (string-index str #\nul)
      (error "kracht sql: a SQL identifier cannot contain NUL characters"
             name))
    (string-append "\"" (string-replace-substring str "\"" "\"\"") "\"")))

(define (pair->sql-assignment a)
  "Convert a key-value pair A into a SQL assignment clause string
formatted as \"column_name = value\".

The key symbol is mapped via `symbol->sql-column`. Booleans are converted
to 1 or 0, numbers are unquoted, and all other types are single-quoted with
embedded single quotes escaped.  Use bound parameters where possible."
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
                  (escape-sql-string (cdr a))))))

(define (value->sql-literal a)
  "Convert a Scheme value A into its raw SQL literal string representation.
Numbers are kept raw, booleans are mapped to \"1\" or \"0\", and other
types are wrapped in single quotes with embedded single quotes escaped.
Prefer bound parameters over string interpolation where possible."
  (cond
    ((number? a)
     (format #f "~a" a))
    ((eq? #t a)
     "1")
    ((eq? #f a)
     "0")
    (else (format #f "'~a'"
                  (escape-sql-string a)))))
