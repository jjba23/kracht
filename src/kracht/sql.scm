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
                               sql-insert-many
                               sql-upsert
                               sql-upsert-many
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

;; Dialects disagree about how an identifier is delimited.  ANSI SQL -- and
;; therefore PostgreSQL and SQLite -- uses double quotes, while MySQL uses a
;; backtick and reads a double-quoted token as a string unless ANSI_QUOTES is
;; enabled.  Getting this wrong is not a style question: it silently changes
;; what the statement means.
(define (mysql-dialect? dialect)
  "Return #t when DIALECT is `mysql' or `mariadb'."
  (memq dialect '(mysql mariadb)))

(define (known-dialect? dialect)
  "Return #t when DIALECT is a dialect kracht knows how to render."
  (memq dialect '(ansi postgres postgresql sqlite mysql mariadb)))

(define (sql-identifier-style quote-identifiers? dialect)
  "Return how identifiers should be rendered: #f to leave them bare, or the
symbol `ansi' or `mysql' to delimit them the way DIALECT requires."
  (unless (known-dialect? dialect)
    (error "kracht sql: unknown dialect" dialect))
  (cond
    ((not quote-identifiers?) #f)
    ((mysql-dialect? dialect) 'mysql)
    (else 'ansi)))

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

(define* (quote-sql-identifier name #:key (dialect 'ansi))
  "Render NAME (a symbol or string) as a safely delimited SQL identifier.

With the default `#:dialect 'ansi' the name is wrapped in double quotes with any
embedded double quote doubled.  With `#:dialect 'mysql' (or `'mariadb') it is
wrapped in backticks with embedded backticks doubled instead, because MySQL
reads a double-quoted token as a string unless ANSI_QUOTES is enabled.

Use this for column and table names that come from data.  `symbol->sql-column'
only maps hyphens to underscores and does not defend against a name that
contains quotes or other punctuation.  A NUL character is rejected."
  (let ((str (if (symbol? name) (symbol->string name)
                 (format #f "~a" name))))
    (when (string-index str #\nul)
      (error "kracht sql: a SQL identifier cannot contain NUL characters"
             name))
    (if (mysql-dialect? dialect)
        (string-append "`" (string-replace-substring str "`" "``") "`")
        (string-append "\"" (string-replace-substring str "\"" "\"\"") "\""))))

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

(define (sql-identifier x identifier-style)
  "Render X (a symbol or string) as a SQL identifier.  A symbol is first mapped
through `symbol->sql-column'.  IDENTIFIER-STYLE is #f to leave the name bare,
or `ansi'/`mysql' to delimit it the way that dialect requires."
  (let ((name (if (symbol? x) (symbol->sql-column x)
                  (format #f "~a" x))))
    (if identifier-style
        (quote-sql-identifier name #:dialect identifier-style)
        name)))

(define (sql-identifier-list xs identifier-style)
  "Render the list XS as a comma-separated list of SQL identifiers, keeping the
star `*' unquoted."
  (string-join (map (lambda (x)
                      (if (eq? x '*) "*"
                          (sql-identifier x identifier-style))) xs)
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

(define (sql-condition->fragment condition style identifier-style index)
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
                     (sql-identifier (car condition) identifier-style)
                     (sql-placeholder index #:style style))
             (list (cdr condition))
             (+ index 1)))
    (else
     (let ((column (sql-identifier (car condition) identifier-style))
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

(define (sql-conditions->fragment conditions style identifier-style index)
  "Compile CONDITIONS into an ` AND '-joined fragment, returning three values:
the text, its parameters and the next free parameter index."
  (let loop ((cs conditions) (i index) (parts '()) (params '()))
    (if (null? cs)
        (values (string-join (reverse parts) " AND ") (reverse params) i)
        (call-with-values (lambda ()
                            (sql-condition->fragment (car cs) style
                                                     identifier-style i))
          (lambda (text ps next)
            (loop (cdr cs) next (cons text parts)
                  (append (reverse ps) params)))))))

(define* (sql-where-fragment conditions
                             #:key (style 'question) (quote-identifiers? #t)
                                   (dialect 'ansi) (start-index 1))
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
#:quote-identifiers? is #f, and quoted the way DIALECT requires.

START-INDEX is the index of the first placeholder, for composing this fragment
after another one that has already bound values."
  (call-with-values (lambda ()
                      (sql-conditions->fragment
                       conditions style
                       (sql-identifier-style quote-identifiers? dialect)
                       start-index))
    (lambda (text params _next-index)
      (make-sql-query (if (string-null? text) ""
                          (string-append " WHERE " text))
                      params))))

(define (rows->columns-and-values rows)
  "Validate ROWS, a non-empty list of association lists, and return two values:
the column list taken from the first row, and a list of value lists, each in
that column order.

Every row must carry exactly the same columns.  Rows are put into the first
row's column order, so a caller need not keep them sorted; a row that is missing
a column, or that carries an extra one, is an error rather than a silently
shifted value."
  (when (null? rows)
    (error "kracht sql: at least one row is required"))
  (let ((columns (map car (car rows))))
    (when (null? columns)
      (error "kracht sql: a row needs at least one column" (car rows)))
    (values columns
            (map (lambda (row)
                   (when (not (= (length row) (length columns)))
                     (error "kracht sql: every row must have the same columns"
                            row columns))
                   (map (lambda (column)
                          (let ((pair (assoc column row)))
                            (if pair
                                (cdr pair)
                                (error "kracht sql: a row is missing a column"
                                       column row))))
                        columns))
                 rows))))

(define (sql-values-clause value-rows column-count style start-index)
  "Render the `(...), (...)' VALUES list for VALUE-ROWS, each as long as
COLUMN-COUNT, returning two values: the text and the next free parameter index."
  (let loop ((rs value-rows) (i start-index) (groups '()))
    (if (null? rs)
        (values (string-join (reverse groups) ", ") i)
        (let inner ((n column-count) (j i) (placeholders '()))
          (if (= n 0)
              (loop (cdr rs) j
                    (cons (string-append "("
                                         (string-join (reverse placeholders) ", ")
                                         ")")
                          groups))
              (inner (- n 1) (+ j 1)
                     (cons (sql-placeholder j #:style style) placeholders)))))))

(define* (sql-insert-many table rows
                          #:key (style 'question) (quote-identifiers? #t)
                                (dialect 'ansi) returning)
  "Build an `INSERT INTO ... VALUES (...), (...)' query from ROWS, a non-empty
list of association lists.  The column list comes from the first row; every
other row must carry the same columns, and all the values are bound in that
column order, row by row.

Passing a single row is fine, and is exactly what `sql-insert' does.

RETURNING, when given, is a list of columns -- or `(*)' for all columns -- to
append as a `RETURNING' clause, as PostgreSQL and SQLite support."
  (call-with-values (lambda () (rows->columns-and-values rows))
    (lambda (columns value-rows)
      (let ((identifier-style (sql-identifier-style quote-identifiers? dialect)))
        (call-with-values
            (lambda ()
              (sql-values-clause value-rows (length columns) style 1))
          (lambda (values-text _next-index)
            (make-sql-query
             (string-append "INSERT INTO "
                            (sql-identifier table identifier-style)
                            " ("
                            (sql-identifier-list columns identifier-style)
                            ") VALUES "
                            values-text
                            (if returning
                                (string-append " RETURNING "
                                               (sql-identifier-list returning
                                                                    identifier-style))
                                ""))
             (apply append value-rows))))))))

(define* (sql-insert table row
                     #:key (style 'question) (quote-identifiers? #t)
                           (dialect 'ansi) returning)
  "Build an `INSERT INTO ... VALUES ...' query from ROW, a non-empty
association list of column/value pairs.  The values become bound parameters in
the order they appear in ROW.

Use `sql-insert-many' to insert several rows with a single statement.

RETURNING, when given, is a list of columns -- or `(*)' for all columns -- to
append as a `RETURNING' clause, as PostgreSQL and SQLite support."
  (when (null? row)
    (error "kracht sql: sql-insert needs at least one column" table))
  (sql-insert-many table (list row)
                   #:style style
                   #:quote-identifiers? quote-identifiers?
                   #:dialect dialect
                   #:returning returning))

(define (columns-without columns removed)
  "Return COLUMNS with every column in REMOVED dropped, comparing with `equal?'."
  (filter (lambda (column)
            (not (member column removed)))
          columns))

(define (columns-known? subset columns)
  "Return #t when every column in SUBSET appears in COLUMNS."
  (or (null? subset)
      (and (member (car subset) columns)
           (columns-known? (cdr subset) columns))))

(define (sql-upsert-set-clause dialect update-columns identifier-style)
  "Render the assignments of a conflict action, sourcing every value from the
row being inserted: EXCLUDED for PostgreSQL and SQLite, VALUES() for MySQL.  No
new parameters are introduced, so the caller's numbering is unaffected."
  (let ((mysql? (mysql-dialect? dialect)))
    (string-join
     (map (lambda (column)
            (let ((name (sql-identifier column identifier-style)))
              (format #f "~a = ~a"
                      name
                      (if mysql?
                          (format #f "VALUES(~a)" name)
                          (format #f "EXCLUDED.~a" name)))))
          update-columns)
     ", ")))

(define* (sql-upsert-many table rows
                          #:key (style 'question) (quote-identifiers? #t)
                                (dialect 'postgres) conflict-target
                                (update 'auto) returning)
  "Build an `INSERT ... ON CONFLICT ...' (PostgreSQL and SQLite) or
`INSERT ... ON DUPLICATE KEY UPDATE ...' (MySQL and MariaDB) query from ROWS, a
non-empty list of association lists -- see `sql-insert-many' for how ROWS is
read.

DIALECT is `postgres' (or `postgresql'), `sqlite', `mysql' (or `mariadb'), or
`ansi', which is treated as the ON CONFLICT form.

CONFLICT-TARGET is the column, or list of columns, the conflict is detected on.
It is required for the ON CONFLICT form unless UPDATE is `nothing', because
PostgreSQL and SQLite allow a bare `ON CONFLICT DO NOTHING'; it must be left out
for MySQL, which takes the conflict key from a unique index instead.

UPDATE says what happens on a conflict:

  'auto      the default: overwrite every inserted column that is not part of
             CONFLICT-TARGET.  MySQL has no conflict target, so there 'auto
             covers the key columns too; name them in `(columns)' to leave the
             key alone
  'nothing   `ON CONFLICT DO NOTHING'; PostgreSQL and SQLite only, since MySQL
             has no equivalent
  (columns)  overwrite just these columns

Every value comes from the row being inserted, through EXCLUDED or VALUES(), so
the statement binds no parameters beyond the ones its INSERT already does.  A
new value computed from the old one -- a counter, say -- is outside what this
builder expresses; write that statement with `sql-parameterize'.

RETURNING, when given, is a list of columns -- or `(*)' -- for PostgreSQL and
SQLite.  MySQL has no `RETURNING' clause."
  (unless (known-dialect? dialect)
    (error "kracht sql: unknown upsert dialect" dialect))
  (let ((mysql? (mysql-dialect? dialect)))
    (when (and mysql? returning)
      (error "kracht sql: MySQL has no RETURNING clause" dialect))
    (when (and mysql? conflict-target)
      (error "kracht sql: MySQL infers the conflict key, so #:conflict-target does not apply"
             conflict-target))
    (when (and mysql? (eq? update 'nothing))
      (error "kracht sql: MySQL has no DO NOTHING; use sql-insert-many or name columns in #:update"
             dialect))
    (when (and (not mysql?) (not (eq? update 'nothing)) (not conflict-target))
      (error "kracht sql: #:conflict-target is required for DO UPDATE" dialect))
    (call-with-values (lambda () (rows->columns-and-values rows))
      (lambda (columns value-rows)
        (let* ((target (if conflict-target
                           (if (list? conflict-target)
                               conflict-target
                               (list conflict-target))
                           '()))
               (update-columns (cond
                                 ((eq? update 'nothing) '())
                                 ((eq? update 'auto)
                                  (columns-without columns target))
                                 ((list? update) update)
                                 (else (error "kracht sql: #:update must be 'auto, 'nothing or a list of columns"
                                              update))))
               (identifier-style (sql-identifier-style quote-identifiers? dialect)))
          (when (not (columns-known? target columns))
            (error "kracht sql: every #:conflict-target column must appear in the rows"
                   target columns))
          (when (not (columns-known? update-columns columns))
            (error "kracht sql: every #:update column must appear in the rows"
                   update-columns columns))
          (when (and (not (eq? update 'nothing)) (null? update-columns))
            (error "kracht sql: nothing is left to update; pass #:update 'nothing"
                   update))
          (call-with-values
              (lambda ()
                (sql-values-clause value-rows (length columns) style 1))
            (lambda (values-text _next-index)
              (make-sql-query
               (string-append
                "INSERT INTO " (sql-identifier table identifier-style)
                " (" (sql-identifier-list columns identifier-style) ") VALUES "
                values-text
                (if mysql?
                    (string-append " ON DUPLICATE KEY UPDATE "
                                   (sql-upsert-set-clause dialect update-columns
                                                          identifier-style))
                    (string-append
                     " ON CONFLICT "
                     (if (null? target) ""
                         (string-append "("
                                        (sql-identifier-list target identifier-style)
                                        ") "))
                     (if (eq? update 'nothing)
                         "DO NOTHING"
                         (string-append "DO UPDATE SET "
                                        (sql-upsert-set-clause dialect update-columns
                                                               identifier-style)))))
                (if returning
                    (string-append " RETURNING "
                                   (sql-identifier-list returning identifier-style))
                    ""))
               (apply append value-rows)))))))))

(define* (sql-upsert table row
                     #:key (style 'question) (quote-identifiers? #t)
                           (dialect 'postgres) conflict-target
                           (update 'auto) returning)
  "Build an upsert for a single ROW.  See `sql-upsert-many', which takes the
same arguments but a list of rows."
  (sql-upsert-many table (list row)
                   #:style style
                   #:quote-identifiers? quote-identifiers?
                   #:dialect dialect
                   #:conflict-target conflict-target
                   #:update update
                   #:returning returning))

(define* (sql-update table row
                     #:key where (style 'question) (quote-identifiers? #t)
                           (dialect 'ansi))
  "Build an `UPDATE ... SET ...' query from ROW, a non-empty association list of
column/value pairs; the values become bound parameters in ROW order.

WHERE is a list of conditions (see `sql-where-fragment') that becomes the
`WHERE' clause.  Passing none updates every row -- that is occasionally what
you want, but it is a good way to lose data, so check before you mean it."
  (when (null? row)
    (error "kracht sql: sql-update needs at least one column" table))
  (let ((identifier-style (sql-identifier-style quote-identifiers? dialect)))
    (let loop ((pairs row) (i 1) (assignments '()) (params '()))
      (if (null? pairs)
          (call-with-values (lambda ()
                              (sql-conditions->fragment (or where '()) style
                                                        identifier-style i))
            (lambda (text where-params _next-index)
              (make-sql-query
               (string-append "UPDATE "
                              (sql-identifier table identifier-style)
                              " SET "
                              (string-join (reverse assignments) ", ")
                              (if (string-null? text) ""
                                  (string-append " WHERE " text)))
               (append (reverse params) where-params))))
          (loop (cdr pairs) (+ i 1)
                (cons (format #f "~a = ~a"
                              (sql-identifier (caar pairs) identifier-style)
                              (sql-placeholder i #:style style))
                      assignments)
                (cons (cdar pairs) params))))))

(define* (sql-delete table
                     #:key where (style 'question) (quote-identifiers? #t)
                           (dialect 'ansi))
  "Build a `DELETE FROM ...' query.  WHERE is a list of conditions (see
`sql-where-fragment') that becomes the `WHERE' clause; with no conditions the
query deletes every row."
  (let ((identifier-style (sql-identifier-style quote-identifiers? dialect)))
    (call-with-values (lambda ()
                        (sql-conditions->fragment (or where '()) style
                                                  identifier-style 1))
      (lambda (text params _next-index)
        (make-sql-query
         (string-append "DELETE FROM " (sql-identifier table identifier-style)
                        (if (string-null? text) ""
                            (string-append " WHERE " text)))
         params)))))

(define (sql-order-by-clause order-by identifier-style)
  "Render ORDER-BY, a list of `(column [direction])' pairs, as an `ORDER BY'
clause.  DIRECTION is the symbol `asc' or `desc'."
  (if (null? order-by)
      ""
      (string-append
       " ORDER BY "
       (string-join
        (map (lambda (clause)
               (let ((column (sql-identifier (car clause) identifier-style))
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
                     (style 'question) (quote-identifiers? #t) (dialect 'ansi))
  "Build a `SELECT ...' query.  COLUMNS is a list of columns, defaulting to
`(*)' for all of them.  WHERE is a list of conditions (see
`sql-where-fragment').  ORDER-BY is a list of `(column [direction])' pairs.

LIMIT and OFFSET, when given, are bound as parameters rather than interpolated,
so they cannot carry SQL into the statement."
  (let ((identifier-style (sql-identifier-style quote-identifiers? dialect)))
    (call-with-values (lambda ()
                        (sql-conditions->fragment (or where '()) style
                                                  identifier-style 1))
      (lambda (text params index)
        (let* ((limit-index index)
               (offset-index (+ index (if limit 1 0)))
               (tail (string-append
                      (sql-order-by-clause (or order-by '()) identifier-style)
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
                          (sql-identifier-list columns identifier-style)
                          " FROM "
                          (sql-identifier table identifier-style)
                          (if (string-null? text) ""
                              (string-append " WHERE " text))
                          tail)
           (append params
                   (if limit (list limit) '())
                   (if offset (list offset) '()))))))))

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
