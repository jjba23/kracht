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

;; You should have received a copy of the GNU Lesser General Public License
;; along with kracht.  If not, see <https://www.gnu.org/licenses/>.

(define-module (kracht sql)
  #:use-module (ice-9 format)
  #:use-module (ice-9 string-fun)
  #:use-module (srfi srfi-9)
  #:use-module (kracht lists)
  #:export (symbol->sql-column sql-column->symbol
                               symbols->sql-columns-list
                               sql-row->scheme-alist
                               escape-sql-string
                               quote-sql-identifier
                               pair->sql-assignment
                               value->sql-literal
                               make-sql-query
                               sql-query?
                               sql-query-sql
                               sql-query-params
                               sql-query->values
                               sql-query->alist
                               sql-placeholder
                               sql-where-fragment
                               sql-insert
                               sql-update
                               sql-delete
                               sql-select
                               sql-parameterize))

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
text type can store one.  Prefer the parameterized builders below
(`sql-insert', `sql-update', `sql-parameterize', ...) over interpolating
escaped values."
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
embedded single quotes escaped.  This interpolates values into the statement;
prefer `sql-update', which binds them instead."
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
This interpolates the value into the statement; prefer the parameterized
builders, which bind it instead."
  (cond
    ((number? a)
     (format #f "~a" a))
    ((eq? #t a)
     "1")
    ((eq? #f a)
     "0")
    (else (format #f "'~a'"
                  (escape-sql-string a)))))

;;;
;;; Parameterized queries
;;;
;;; The builders below never splice a value into the SQL text.  Each returns a
;;; <sql-query>: a statement whose values have been replaced by placeholders,
;;; plus the list of values to bind, in placeholder order.  Hand both to your
;;; driver as-is -- binding is what makes a value inert, so escaping is not
;;; needed and no value can change the shape of the statement.

(define-record-type <sql-query>
  (make-sql-query sql params)
  sql-query?
  (sql sql-query-sql)
  (params sql-query-params))

(define (sql-query->values q)
  "Return the SQL string of query Q and its list of parameters, as two values,
ready for `receive' or `call-with-values'."
  (values (sql-query-sql q) (sql-query-params q)))

(define (sql-query->alist q)
  "Return query Q as an association list with the keys `sql' and `params'."
  (list (cons 'sql (sql-query-sql q))
        (cons 'params (sql-query-params q))))

(define* (sql-placeholder index #:key (style 'question))
  "Render the placeholder standing for the INDEX-th (1-based) bound parameter
in STYLE.

STYLE is one of the symbols `question' (`?'; SQLite, MySQL, JDBC), `numbered'
(`$1'; PostgreSQL), `percent' (`%s'; psycopg and other pyformat drivers) or
`dbi' (`#?'; guile-dbi); or a procedure of one argument, which is called with
INDEX and must return the placeholder string."
  (if (procedure? style)
      (style index)
      (case style
        ((question) "?")
        ((numbered) (format #f "$~a" index))
        ((percent) "%s")
        ((dbi) "#?")
        (else (error "kracht sql: unknown parameter placeholder style" style)))))

(define (sql-identifier x quote?)
  "Render X (a symbol or string) as a SQL identifier.  A symbol is first mapped
through `symbol->sql-column'.  When QUOTE? is true the result is wrapped in
double quotes by `quote-sql-identifier'."
  (let ((name (if (symbol? x) (symbol->sql-column x)
                  (format #f "~a" x))))
    (if quote?
        (quote-sql-identifier name)
        name)))

(define (sql-identifier-list xs quote?)
  "Render the list XS as a comma-separated list of SQL identifiers, keeping the
star `*' unquoted."
  (string-join (map (lambda (x)
                      (if (eq? x '*) "*"
                          (sql-identifier x quote?))) xs)
               ", "))

;; Comparison operators a caller may name in a condition.  Keeping this an
;; allow-list is what stops a value from being read as an operator.
(define sql-comparison-operators
  '((= . "=")
    (<> . "<>")
    (!= . "<>")
    (< . "<")
    (<= . "<=")
    (> . ">")
    (>= . ">=")
    (like . "LIKE")
    (ilike . "ILIKE")
    (not-like . "NOT LIKE")
    (not-ilike . "NOT ILIKE")
    (is . "IS")
    (is-not . "IS NOT")))

(define (sql-in-fragment column items negate? style index)
  "Compile an `IN'/`NOT IN' fragment, returning three values: the SQL text, its
parameters and the next free parameter index.  An empty ITEMS list cannot be
written as `IN ()', so it collapses to an always-false -- or, for NOT IN,
always-true -- predicate instead."
  (if (null? items)
      (values (if negate? "1 = 1" "1 = 0") '() index)
      (let loop ((vs items) (i index) (placeholders '()) (params '()))
        (if (null? vs)
            (values (format #f "~a ~a (~a)" column
                            (if negate? "NOT IN" "IN")
                            (string-join (reverse placeholders) ", "))
                    (reverse params)
                    i)
            (loop (cdr vs) (+ i 1)
                  (cons (sql-placeholder i #:style style) placeholders)
                  (cons (car vs) params))))))

(define (sql-condition->fragment condition style quote? index)
  "Compile one WHERE CONDITION, returning three values: the SQL text, its
parameters and the next free parameter index.  See `sql-where-fragment' for the
shape of a condition."
  (cond
    ((not (pair? condition))
     (error "kracht sql: a condition must be a (column . value) pair or a list, not"
            condition))
    ;; A dotted pair is the shorthand for equality, matching the alists the
    ;; other helpers take: '(id . 7) is "id = 7".
    ((not (list? condition))
     (values (format #f "~a = ~a"
                     (sql-identifier (car condition) quote?)
                     (sql-placeholder index #:style style))
             (list (cdr condition))
             (+ index 1)))
    (else
     (let ((column (sql-identifier (car condition) quote?))
           (operands (cdr condition)))
       (case (length operands)
         ((0) (error "kracht sql: a condition needs a value" condition))
         ((1) (values (format #f "~a = ~a" column
                              (sql-placeholder index #:style style))
                      (list (car operands))
                      (+ index 1)))
         ((2) (let ((operator (car operands))
                    (value (cadr operands)))
                (cond
                  ((and (eq? operator 'is) (eq? value 'null))
                   (values (format #f "~a IS NULL" column) '() index))
                  ((and (eq? operator 'is-not) (eq? value 'null))
                   (values (format #f "~a IS NOT NULL" column) '() index))
                  ((eq? operator 'in)
                   (sql-in-fragment column value #f style index))
                  ((eq? operator 'not-in)
                   (sql-in-fragment column value #t style index))
                  ((assq operator sql-comparison-operators)
                   => (lambda (entry)
                        (values (format #f "~a ~a ~a" column (cdr entry)
                                        (sql-placeholder index #:style style))
                                (list value)
                                (+ index 1))))
                  (else (error "kracht sql: unknown comparison operator"
                               operator)))))
         (else (error "kracht sql: a condition must be (column value), (column operator value), (column 'in values), (column 'not-in values), (column 'is 'null) or (column 'is-not 'null)"
                      condition)))))))

(define (sql-conditions->fragment conditions style quote? index)
  "Compile CONDITIONS into an ` AND '-joined fragment, returning three values:
the text, its parameters and the next free parameter index."
  (let loop ((cs conditions) (i index) (parts '()) (params '()))
    (if (null? cs)
        (values (string-join (reverse parts) " AND ") (reverse params) i)
        (call-with-values (lambda ()
                            (sql-condition->fragment (car cs) style quote? i))
          (lambda (text ps next)
            (loop (cdr cs) next (cons text parts)
                  (append (reverse ps) params)))))))

(define* (sql-where-fragment conditions
                             #:key (style 'question) (quote-identifiers? #t) (start-index 1))
  "Build a ` WHERE ...' fragment from CONDITIONS, for statements you compose
yourself.  The result is a <sql-query> whose SQL is the empty string when
CONDITIONS is empty.

A condition is one of these shapes:

  (column . value)         -- column = value (the short form)
  (column value)           -- column = value
  (column operator value)  -- column OPERATOR value, for one of the comparison
                              operators: =, <>, !=, <, <=, >, >=, like, ilike,
                              not-like, not-ilike, is, is-not
  (column 'in values)      -- column IN (values ...)
  (column 'not-in values)  -- column NOT IN (values ...)
  (column 'is 'null)       -- column IS NULL
  (column 'is-not 'null)   -- column IS NOT NULL

Conditions are joined with ` AND '.  A column may be a symbol (hyphens become
underscores via `symbol->sql-column') or a string; either is quoted unless
#:quote-identifiers? is #f.

START-INDEX is the index of the first placeholder, for composing this fragment
after another one that has already bound values."
  (call-with-values (lambda ()
                      (sql-conditions->fragment conditions style
                                                quote-identifiers? start-index))
    (lambda (text params _next-index)
      (make-sql-query (if (string-null? text) ""
                          (string-append " WHERE " text))
                      params))))

(define* (sql-insert table row #:key (style 'question) (quote-identifiers? #t) returning)
  "Build an `INSERT INTO ... VALUES ...' query from ROW, a non-empty
association list of column/value pairs.  The values become bound parameters in
the order they appear in ROW.

RETURNING, when given, is a list of columns -- or `(*)' for all columns -- to
append as a `RETURNING' clause, as PostgreSQL and SQLite support."
  (when (null? row)
    (error "kracht sql: sql-insert needs at least one column" table))
  (let* ((columns (map car row))
         (row-values (map cdr row))
         (placeholders (map (lambda (i)
                              (sql-placeholder i #:style style))
                            (iota (length row-values) 1))))
    (make-sql-query
     (string-append "INSERT INTO "
                    (sql-identifier table quote-identifiers?)
                    " ("
                    (sql-identifier-list columns quote-identifiers?)
                    ") VALUES ("
                    (string-join placeholders ", ")
                    ")"                        (if returning
                        (string-append " RETURNING "
                                       (sql-identifier-list returning quote-identifiers?))
                        ""))
     row-values)))

(define* (sql-update table row #:key where (style 'question) (quote-identifiers? #t))
  "Build an `UPDATE ... SET ...' query from ROW, a non-empty association list of
column/value pairs; the values become bound parameters in ROW order.

WHERE is a list of conditions (see `sql-where-fragment') that becomes the
`WHERE' clause.  Passing none updates every row -- that is occasionally what
you want, but it is a good way to lose data, so check before you mean it."
  (when (null? row)
    (error "kracht sql: sql-update needs at least one column" table))
  (let loop ((pairs row) (i 1) (assignments '()) (params '()))
    (if (null? pairs)
        (call-with-values (lambda ()
                            (sql-conditions->fragment (or where '()) style
                                                      quote-identifiers? i))
          (lambda (text where-params _next-index)
            (make-sql-query
             (string-append "UPDATE "
                            (sql-identifier table quote-identifiers?)
                            " SET "
                            (string-join (reverse assignments) ", ")
                            (if (string-null? text) ""
                                (string-append " WHERE " text)))
             (append (reverse params) where-params))))
        (loop (cdr pairs) (+ i 1)
              (cons (format #f "~a = ~a"
                            (sql-identifier (caar pairs) quote-identifiers?)
                            (sql-placeholder i #:style style))
                    assignments)
              (cons (cdar pairs) params)))))

(define* (sql-delete table #:key where (style 'question) (quote-identifiers? #t))
  "Build a `DELETE FROM ...' query.  WHERE is a list of conditions (see
`sql-where-fragment') that becomes the `WHERE' clause; with no conditions the
query deletes every row."
  (call-with-values (lambda ()
                      (sql-conditions->fragment (or where '()) style
                                                quote-identifiers? 1))
    (lambda (text params _next-index)
      (make-sql-query
       (string-append "DELETE FROM " (sql-identifier table quote-identifiers?)
                      (if (string-null? text) ""
                          (string-append " WHERE " text)))
       params))))

(define (sql-order-by-clause order-by quote?)
  "Render ORDER-BY, a list of `(column [direction])' pairs, as an `ORDER BY'
clause.  DIRECTION is the symbol `asc' or `desc'."
  (if (null? order-by)
      ""
      (string-append
       " ORDER BY "
       (string-join
        (map (lambda (clause)
               (let ((column (sql-identifier (car clause) quote?))
                     (direction (and (pair? (cdr clause)) (cadr clause))))
                 (case direction
                   ((#f) column)
                   ((asc asc!) (string-append column " ASC"))
                   ((desc desc!) (string-append column " DESC"))
                   (else (error "kracht sql: unknown ORDER BY direction "
                                direction)))))
             order-by)
        ", "))))

(define* (sql-select table #:key (columns '(*)) where order-by limit offset
                     (style 'question) (quote-identifiers? #t))
  "Build a `SELECT ...' query.  COLUMNS is a list of columns, defaulting to
`(*)' for all of them.  WHERE is a list of conditions (see
`sql-where-fragment').  ORDER-BY is a list of `(column [direction])' pairs.

LIMIT and OFFSET, when given, are bound as parameters rather than interpolated,
so they cannot carry SQL into the statement."
  (call-with-values (lambda ()
                      (sql-conditions->fragment (or where '()) style
                                                quote-identifiers? 1))
    (lambda (text params index)
      (let* ((limit-index index)
             (offset-index (+ index (if limit 1 0)))
             (tail (string-append
                    (sql-order-by-clause (or order-by '()) quote-identifiers?)
                    (if limit
                        (string-append " LIMIT "
                                       (sql-placeholder limit-index #:style style))
                        "")
                    (if offset
                        (string-append " OFFSET "
                                       (sql-placeholder offset-index #:style style))
                        ""))))
        (make-sql-query
         (string-append "SELECT "
                        (sql-identifier-list columns quote-identifiers?)
                        " FROM "
                        (sql-identifier table quote-identifiers?)
                        (if (string-null? text) ""
                            (string-append " WHERE " text))
                        tail)
         (append params
                 (if limit (list limit) '())
                 (if offset (list offset) '())))))))

(define* (sql-parameterize template bindings #:key (style 'question))
  "Return a <sql-query> that binds BINDINGS into TEMPLATE, replacing each `?'
with the placeholder for STYLE.

Use this for statements the builders above do not cover.  TEMPLATE must contain
exactly as many `?' characters as there are bindings; the count is checked, so
a mismatch is reported instead of producing a subtly wrong query.  There is no
escape for a literal `?', so keep those out of the template."
  (let ((expected (length bindings)))
    (let loop ((chars (string->list template)) (index 1) (out '()) (rest bindings))
      (cond
        ((null? chars)
         (when (pair? rest)
           (error "kracht sql: sql-parameterize received more bindings than `?' markers"
                  expected))
         (make-sql-query (list->string (reverse out)) bindings))
        ((char=? (car chars) #\?)
         (when (null? rest)
           (error "kracht sql: sql-parameterize received more `?' markers than bindings"
                  expected))
         (loop (cdr chars) (+ index 1)
               (append (reverse (string->list (sql-placeholder index #:style style)))
                       out)
               (cdr rest)))
        (else (loop (cdr chars) index (cons (car chars) out) rest))))))
