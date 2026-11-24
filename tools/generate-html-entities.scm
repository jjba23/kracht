;;; generate-html-entities.scm --- generate the kracht entity tables

;; kracht
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;;; Commentary:
;;
;; Write src/kracht/html-entities.scm out of the standard's own tables, rather
;; than transcribing a couple of thousand named character references by hand.
;; It needs nothing but Guile and guile-json, both of which the test suite
;; already requires.  Run it from the project root:
;;
;;     guile tools/generate-html-entities.scm
;;
;; There are two inputs, and the split between them is deliberate:
;;
;;   * tools/html5-entities.json is the standard's data: an unmodified copy of
;;     the WHATWG named character reference list published as
;;     https://html.spec.whatwg.org/entities.json.  Refresh it with
;;
;;         curl -o tools/html5-entities.json \
;;              https://html.spec.whatwg.org/entities.json
;;
;;   * the emit set below is kracht's policy: which of those references are
;;     worth *writing* when escaping.  That is a choice rather than data, so
;;     it is written out here instead of being read from somewhere.  It is the
;;     HTML 4.01 entity set, plus `apos', which HTML4 declares through the DTD
;;     rather than in a document.
;;
;; A name is written for the character the standard's own table says it stands
;; for, so the two tables below cannot disagree with each other: an entry is
;; either right or it is not written.

;;; Code:

(use-modules (ice-9 format)
             (srfi srfi-1)
             (json))

;; The standard's list, and the module generated from it.
(define entities-json-file "tools/html5-entities.json")
(define module-file "src/kracht/html-entities.scm")

;; The two characters a Scheme string literal has to escape.  They are made
;; from their code points so that this file needs no backslashes of its own,
;; which is one less way to get the escaping wrong in generated text.
(define backslash-char (integer->char 92))
(define quote-mark (integer->char 34))
(define quote-char (string quote-mark))

;; Writing a Scheme string literal: only the backslash and the double quote
;; need care, and every other character -- accented letters, symbols, the
;; non-printing ones -- is written through as itself.
(define (scheme-string s)
  (string-append quote-char
                 (call-with-output-string
                   (lambda (port)
                     (string-for-each (lambda (c)
                                        (when (memv c (list backslash-char
                                                            quote-mark))
                                          (display backslash-char port))
                                        (display c port))
                                      s)))
                 quote-char))

;; The references kracht is willing to write as `html-escape', given as the
;; name between `&' and `;' and ordered by the code point it stands for.  This
;; is the HTML4 set: Latin-1, the common symbols, the Greek letters and the
;; mathematical and card-suit characters, together with the five characters
;; HTML is sensitive to and `apos'.
;;
;; It is deliberately *not* the standard's complete list: decoding is broader
;; than encoding, and writing `&sol;' for a slash (or `&dollar;' for a dollar
;; sign) would produce documents no one can read.
(define emit-set
  '("quot" "amp" "apos" "lt" "gt" "nbsp" "iexcl" "cent" "pound" "curren"
    "yen" "brvbar" "sect" "uml" "copy" "ordf" "laquo" "not" "shy" "reg"
    "macr" "deg" "plusmn" "sup2" "sup3" "acute" "micro" "para" "middot"
    "cedil" "sup1" "ordm" "raquo" "frac14" "frac12" "frac34" "iquest"
    "Agrave" "Aacute" "Acirc" "Atilde" "Auml" "Aring" "AElig" "Ccedil"
    "Egrave" "Eacute" "Ecirc" "Euml" "Igrave" "Iacute" "Icirc" "Iuml"
    "ETH" "Ntilde" "Ograve" "Oacute" "Ocirc" "Otilde" "Ouml" "times"
    "Oslash" "Ugrave" "Uacute" "Ucirc" "Uuml" "Yacute" "THORN" "szlig"
    "agrave" "aacute" "acirc" "atilde" "auml" "aring" "aelig" "ccedil"
    "egrave" "eacute" "ecirc" "euml" "igrave" "iacute" "icirc" "iuml"
    "eth" "ntilde" "ograve" "oacute" "ocirc" "otilde" "ouml" "divide"
    "oslash" "ugrave" "uacute" "ucirc" "uuml" "yacute" "thorn" "yuml"
    "OElig" "oelig" "Scaron" "scaron" "Yuml" "fnof" "circ" "tilde"
    "Alpha" "Beta" "Gamma" "Delta" "Epsilon" "Zeta" "Eta" "Theta" "Iota"
    "Kappa" "Lambda" "Mu" "Nu" "Xi" "Omicron" "Pi" "Rho" "Sigma" "Tau"
    "Upsilon" "Phi" "Chi" "Psi" "Omega" "alpha" "beta" "gamma" "delta"
    "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi"
    "omicron" "pi" "rho" "sigmaf" "sigma" "tau" "upsilon" "phi" "chi"
    "psi" "omega" "thetasym" "upsih" "piv" "ensp" "emsp" "thinsp" "zwnj"
    "zwj" "lrm" "rlm" "ndash" "mdash" "lsquo" "rsquo" "sbquo" "ldquo"
    "rdquo" "bdquo" "dagger" "Dagger" "bull" "hellip" "permil" "prime"
    "Prime" "lsaquo" "rsaquo" "oline" "frasl" "euro" "image" "weierp"
    "real" "trade" "alefsym" "larr" "uarr" "rarr" "darr" "harr" "crarr"
    "lArr" "uArr" "rArr" "dArr" "hArr" "forall" "part" "exist" "empty"
    "nabla" "isin" "notin" "ni" "prod" "sum" "minus" "lowast" "radic"
    "prop" "infin" "ang" "and" "or" "cap" "cup" "int" "there4" "sim"
    "cong" "asymp" "ne" "equiv" "le" "ge" "sub" "sup" "nsub" "sube"
    "supe" "oplus" "otimes" "perp" "sdot" "lceil" "rceil" "lfloor"
    "rfloor" "lang" "rang" "loz" "spades" "clubs" "hearts" "diams"))

;; Everything of the module that is not a table.  It lives here so that the
;; generated file is the whole artifact, comments included.
(define module-head
  ";; kracht
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

;; This module is data, and it is generated rather than written by hand.  The
;; named character references below are the ones the HTML standard defines --
;; the WHATWG list published as
;;   https://html.spec.whatwg.org/entities.json
;; and copied unmodified into tools/html5-entities.json.  Regenerate the file
;; with tools/generate-html-entities.scm rather than editing it.
;;
;; Only references that end in a semicolon are kept.  The HTML parser also
;; accepts a legacy set of unterminated forms (`&amp' with no `;'), but only in
;; positions where no other character could continue the name; kracht's decoder
;; requires the terminator instead, which is unambiguous everywhere.

(define-module (kracht html-entities)
  #:export (html-named-entities
            html-escape-entities
            html-entity-ref
            html-char-ref))

;; Every HTML5 named character reference that ends in `;', as an association
;; list from the reference as it appears in a document to the characters it
;; stands for.  The value is a string rather than a character because 93 of
;; these references -- `&NotEqualTilde;', say -- stand for two code points.
(define html-named-entities
  '(
")

;; The commentary in front of the second table, and the opening of its
;; definition.  It follows the table the generator writes first.
(define escape-table-head
  "    ))

;; The characters `html-escape' writes as named entities: the HTML4 set,
;; which is Latin-1 plus the common symbols, together with the five
;; characters HTML is sensitive to and `&apos;'.  Decoding is deliberately
;; broader than encoding -- this is the list worth writing, while
;; `html-named-entities' is the list worth understanding.  Which names are
;; worth writing is a choice rather than data, so the set lives in
;; tools/generate-html-entities.scm.
;;
;; Each entry stands for the character the standard says its reference stands
;; for, which is not always the character HTML4 named it for: `&lang;' and
;; `&rang;' mean the mathematical angle brackets U+27E8 and U+27E9 here,
;; because that is how HTML5 reads them.  The CJK angle brackets U+2329 and
;; U+232A have no reference worth writing, and pass through as themselves.
(define html-escape-entities
  '(
")

;; The lookups, which are hand-written code rather than data.  The two
;; docstrings are written out through `scheme-string' like everything else, so
;; the quotes inside them are the generator's business and not the author's.
(define entity-ref-docstring
  (string-append
   "Return the characters the named character reference NAME stands for, or
#f when NAME is not one kracht knows.

NAME is the reference as it appears in the document, including the
leading ampersand and the trailing semicolon -- `"
   quote-char "&aacute;" quote-char "', not
`" quote-char "aacute" quote-char "' or `" quote-char "&aacute"
   quote-char "'."))

(define char-ref-docstring
  "Return the named character reference for the character C, or #f when
kracht has no entity it is worth writing for C.")

(define module-tail
  (string-append
   "    ))

;; Both tables are looked up by key, so build the hash tables once here
;; rather than scanning an alist per character.
(define html-entity-lookup
  (let ((table (make-hash-table)))
    (for-each (lambda (entry)
                (hash-set! table (car entry) (cdr entry)))
              html-named-entities)
    table))

(define html-char-lookup
  (let ((table (make-hash-table)))
    (for-each (lambda (entry)
                (hash-set! table (string-ref (car entry) 0) (cdr entry)))
              html-escape-entities)
    table))

(define (html-entity-ref name)
  "
   (scheme-string entity-ref-docstring)
   "
  (hash-ref html-entity-lookup name #f))

(define (html-char-ref c)
  "
   (scheme-string char-ref-docstring)
   "
  (hash-ref html-char-lookup c #f))
"))

(define (read-entities)
  "Read the standard's list and return it as an association list from the
reference as it appears in a document to the characters it stands for, keeping
only the references that end in a semicolon."
  (call-with-input-file entities-json-file
    (lambda (port)
      (sort (filter-map (lambda (entry)
                          (let ((reference (car entry)))
                            (and (string-suffix? ";" reference)
                                 (cons reference
                                       (assoc-ref (cdr entry) "characters")))))
                        (json->scm port))
            (lambda (a b)
              (string<? (car a) (car b)))))))

(define (reference-table entities)
  "Index ENTITIES by the reference as it appears in a document."
  (let ((table (make-hash-table)))
    (for-each (lambda (entry)
                (hash-set! table (car entry) (cdr entry)))
              entities)
    table))

(define (emittable-entries table)
  "Return the (character . reference) pairs to write out, ordered by code
point.  A character is kept as a one-character string, the way the tables
index them.

Each reference is written for the character the standard's own table gives it,
which is what keeps the two tables consistent.  A name the standard does not
know, or one that stands for more than a single code point -- which the
character-keyed table cannot express -- is an error rather than a silently
skipped line."
  (sort (map (lambda (name)
               (let* ((reference (string-append "&" name ";"))
                      (characters (hash-ref table reference #f)))
                 (cond
                   ((not characters)
                    (error "no such reference in the standard's list:"
                           reference))
                   ((= 1 (string-length characters))
                    (cons characters reference))
                   (else
                    (error "reference stands for more than one code point, so no character can stand for it:"
                           reference)))))
             emit-set)
        (lambda (a b)
          (string<? (car a) (car b)))))

(define (write-entries port entries)
  "Write ENTRIES as an association list, one pair per line, with the four
leading spaces the generated module uses for its own table bodies."
  (for-each (lambda (entry)
              (format port "    (~a . ~a)~%"
                      (scheme-string (car entry))
                      (scheme-string (cdr entry))))
            entries))

(define (write-module entities emittable)
  "Write the module out: the head, the decoded table, the second table's
commentary and opening, the emittable table, and the lookups."
  (call-with-output-file module-file
    (lambda (port)
      (display module-head port)
      (write-entries port entities)
      (display escape-table-head port)
      (write-entries port emittable)
      (display module-tail port))))

(define (main)
  (let* ((entities (read-entities))
         (table (reference-table entities))
         (emittable (emittable-entries table)))
    (write-module entities emittable)
    (format #t "wrote ~a~%" module-file)
    (format #t "decode entries: ~a~%" (length entities))
    (format #t "encode entries: ~a~%" (length emittable))
    (format #t "multi-codepoint references: ~a~%"
            (count (lambda (entry)
                     (> (string-length (cdr entry)) 1))
                   entities))
    (exit 0)))

(main)
