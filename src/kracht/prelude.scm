;; kracht
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; (kracht prelude) is the umbrella module.  It gathers every focused kracht
;; module (strings, lists, sql, web, io) and re-exports a broad set of common
;; SRFI procedures, so a single
;;
;;   (use-modules (kracht prelude))
;;
;; is usually all you need.  If you prefer to import only what you use, each
;; sub-module can be required on its own, e.g. (kracht strings).

(define-module (kracht prelude)
  ;; Kracht's focused sub-modules.
  #:use-module (kracht strings)
  #:use-module (kracht lists)
  #:use-module (kracht sql)
  #:use-module (kracht web)
  #:use-module (kracht html-entities)
  #:use-module (kracht io)
  ;; Standard-library re-exports that Guile's core does not already provide.
  #:use-module (ice-9 match)
  #:use-module (ice-9 pretty-print)
  #:use-module (srfi srfi-1)
  #:use-module (srfi srfi-2)
  #:use-module (srfi srfi-8)
  #:use-module (srfi srfi-11)
  #:use-module (srfi srfi-26)
  #:use-module (srfi srfi-111)
  #:use-module ((srfi srfi-19)
                #:select (current-date current-julian-day date->string
                                      date->time-utc date-day date-hour
                                      date-minute date-month date-nanosecond
                                      date-second date-week-day date-year
                                      date-zone-offset date? make-date
                                      make-time string->date subtract-duration
                                      time<=? time<? time=? time>=?
                                      time>? time? time-difference
                                      time-nanosecond time-second time-tai
                                      time-type time-utc time-utc->date
                                      time-monotonic add-duration))
  #:use-module ((srfi srfi-43)
                #:select (reverse-list->vector reverse-vector->list
                                               vector-any vector-append
                                               vector-binary-search
                                               vector-concatenate vector-count
                                               vector-empty? vector-every
                                               vector-fold vector-fold-right
                                               vector-for-each vector-index
                                               vector-index-right vector-map
                                               vector-map!
                                               vector-reverse-copy
                                               vector-reverse-copy!
                                               vector-reverse! vector-skip
                                               vector-skip-right vector-swap!
                                               vector-unfold vector-unfold-right
                                               vector=))
  ;; Re-export everything provided by the kracht sub-modules...
  #:re-export (string-replace-substring
                                        string->utf8
                                        utf8->string
                                        uri-decode
                                        uri-encode
                                        with-output-to-port
                                        with-output-to-file
                                        with-output-to-string
                                        get-string-all
                                        open-input-pipe
                                        open-input-output-pipe
                                        non-empty-string?
                                        empty-string?
                                        string-drop-first-last
                                        string-drop-first-last-n
                                        string-drop-first-last-line
                                        base64-encode
                                        base64-decode
                                        flatten-list
                                        alist?
                                        assoc-lens
                                        assoc-ref-or
                                        assoc-ref-or-empty-str
                                        alist-set
                                        evaluate-thunks
                                        shuffle-fy
                                        symbol->sql-column
                                        sql-column->symbol
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
                                        sql-parameterize
                                        html-escape
                                        html-unescape
                                        html-entity-ref
                                        html-char-ref
                                        html-named-entities
                                        html-escape-entities
                                        html-escape-context
                                        html-escape-unquoted-attribute
                                        html-escape-js-string
                                        html-escape-css-value
                                        alist-from-url-encoded-body
                                        read-file-string
                                        write-file-string
                                        build-asset-library
                                        syscall
                                        get-short-commit-sha
                                        getenv-or)
  ;; ...and the common SRFI / ice-9 procedures.
  #:re-export (pretty-print
                                        truncated-print
                                        match
                                        match-lambda
                                        match-lambda*
                                        match-let
                                        match-let*
                                        match-letrec
                                        and-let*
                                        receive
                                        let-values
                                        let*-values
                                        cut
                                        cute
                                        box
                                        box?
                                        unbox
                                        set-box!
                                        fold
                                        fold-right
                                        alist-cons
                                        alist-copy
                                        alist-delete
                                        alist-delete!
                                        any
                                        append-map
                                        append-map!
                                        append-reverse
                                        append-reverse!
                                        break
                                        break!
                                        car+cdr
                                        circular-list
                                        circular-list?
                                        concatenate
                                        concatenate!
                                        count
                                        delete-duplicates
                                        delete-duplicates!
                                        dotted-list?
                                        drop
                                        drop-right
                                        drop-right!
                                        drop-while
                                        eighth
                                        every
                                        fifth
                                        filter-map
                                        find
                                        find-tail
                                        first
                                        fourth
                                        last
                                        length+
                                        ;; SRFI-1's `list-index' is deliberately
                                        ;; not re-exported: Guile's core exports an
                                        ;; incompatible `list-index', so re-exporting
                                        ;; it would print an "overrides core binding"
                                        ;; warning on every import.
                                        list-tabulate
                                        list=
                                        lset-adjoin
                                        lset-diff+intersection
                                        lset-diff+intersection!
                                        lset-difference
                                        lset-difference!
                                        lset-intersection
                                        lset-intersection!
                                        lset-union
                                        lset-union!
                                        lset-xor
                                        lset-xor!
                                        lset<=
                                        lset=
                                        map!
                                        ninth
                                        not-pair?
                                        null-list?
                                        pair-fold
                                        pair-fold-right
                                        pair-for-each
                                        partition
                                        partition!
                                        proper-list?
                                        reduce
                                        reduce-right
                                        remove
                                        remove!
                                        second
                                        seventh
                                        sixth
                                        span
                                        span!
                                        split-at
                                        split-at!
                                        take
                                        take!
                                        take-right
                                        take-while
                                        take-while!
                                        tenth
                                        third
                                        unfold
                                        unfold-right
                                        unzip1
                                        unzip2
                                        unzip3
                                        unzip4
                                        unzip5
                                        xcons
                                        zip
                                        reverse-list->vector
                                        reverse-vector->list
                                        vector-any
                                        vector-append
                                        vector-binary-search
                                        vector-concatenate
                                        vector-count
                                        vector-empty?
                                        vector-every
                                        vector-fold
                                        vector-fold-right
                                        vector-for-each
                                        vector-index
                                        vector-index-right
                                        vector-map
                                        vector-map!
                                        vector-reverse-copy
                                        vector-reverse-copy!
                                        vector-reverse!
                                        vector-skip
                                        vector-skip-right
                                        vector-swap!
                                        vector-unfold
                                        vector-unfold-right
                                        vector=
                                        current-date
                                        current-julian-day
                                        date->string
                                        date->time-utc
                                        date-day
                                        date-hour
                                        date-minute
                                        date-month
                                        date-nanosecond
                                        date-second
                                        date-week-day
                                        date-year
                                        date-zone-offset
                                        date?
                                        make-date
                                        make-time
                                        string->date
                                        subtract-duration
                                        time<=?
                                        time<?
                                        time=?
                                        time>=?
                                        time>?
                                        time?
                                        time-difference
                                        time-nanosecond
                                        time-second
                                        time-tai
                                        time-type
                                        time-utc
                                        time-utc->date
                                        time-monotonic
                                        add-duration))
