;; kracht
;;
;; SPDX-License-Identifier: MIT
;; Copyright © Josep Bigorra <jjbigorra@gmail.com>

(define-module (veritas unit entities-spec)
  #:use-module (veritas veritas)
  #:use-module (kracht prelude)
  #:export (spec))

;; Non-ASCII expectations are written as code points rather than as literal
;; characters, so this file does not depend on how it is encoded on disk.
(define (chars . code-points)
  "Build the string whose characters are CODE-POINTS."
  (list->string (map integer->char code-points)))

(define nbsp (chars 160))
(define copy (chars 169))
(define mdash (chars 8212))
(define hellip (chars 8230))
(define euro (chars 8364))
(define trade (chars 8482))
(define alpha (chars 945))
(define Alpha (chars 913))
(define rarr (chars 8594))
(define le (chars 8804))
(define aacute (chars 225))
(define atilde (chars 227))

;; A reference that stands for two code points rather than one.
(define not-equal-tilde (chars 8770 824))

(define entities-suite
  (suite "HTML entity tables"
         (test "the tables are the standard's, not a hand-written sample"
               (assert-equal #:name "every HTML5 reference ending in a semicolon"
                             #:expect 2125
                             #:got (length html-named-entities))
               (assert-equal #:name "the escape set is the HTML4 one"
                             #:expect 253
                             #:got (length html-escape-entities)))

         (test "a reference stands for the character the standard gives it"
               ;; `&lang;' and `&rang;' name the CJK angle brackets U+2329 and
               ;; U+232A in HTML4, but the mathematical ones U+27E8 and U+27E9
               ;; in HTML5.  The table follows the standard, so the entry is
               ;; the mathematical bracket and the CJK one has no reference.
               (assert-equal #:name "the mathematical brackets are what is written"
                             #:expect (list "&lang;" "&rang;")
                             #:got (list (html-char-ref (integer->char #x27E8))
                                         (html-char-ref (integer->char #x27E9))))
               (assert-equal #:name "and they survive a round trip"
                             #:expect (chars #x27E8 #x27E9)
                             #:got (html-unescape (html-escape (chars #x27E8
                                                                       #x27E9))))
               (assert-equal #:name "the CJK brackets have no reference to write"
                             #:expect (list #f #f)
                             #:got (list (html-char-ref (integer->char #x2329))
                                         (html-char-ref (integer->char #x232A))))
               (assert-equal #:name "so those characters pass through unchanged"
                             #:expect (chars #x2329 #x232A)
                             #:got (html-escape (chars #x2329 #x232A)))
               (assert-equal #:name "while the references still decode as HTML5 reads them"
                             #:expect (list (chars #x27E8) (chars #x27E9))
                             #:got (list (html-unescape "&lang;")
                                         (html-unescape "&rang;"))))

         (test "every entry is shaped the way a lookup expects"
               (assert-true (every (lambda (entry)
                                     (and (string-prefix? "&" (car entry))
                                          (string-suffix? ";" (car entry))))
                                   html-named-entities)
                            #:name "decode names run from & to ;")
               (assert-true (every (lambda (entry)
                                     (= 1 (string-length (car entry))))
                                   html-escape-entities)
                            #:name "escape keys are single characters")
               (assert-true (every (lambda (entry)
                                     (and (string-prefix? "&" (cdr entry))
                                          (string-suffix? ";" (cdr entry))))
                                   html-escape-entities)
                            #:name "escape values are named references"))

         (test "the two tables agree with each other"
               ;; Every reference the encoder is willing to write must be one
               ;; the decoder understands, and must mean the same character.
               (assert-equal #:name "nothing is emitted that cannot be read back"
                             #:expect '()
                             #:got (filter-map (lambda (entry)
                                                 (if (html-entity-ref (cdr entry))
                                                     #f
                                                     (cdr entry)))
                                               html-escape-entities))
               (assert-equal #:name "and reading it back gives the same character"
                             #:expect '()
                             #:got (filter-map (lambda (entry)
                                                 (if (equal? (html-entity-ref
                                                              (cdr entry))
                                                             (car entry))
                                                     #f
                                                     (list (car entry)
                                                           (cdr entry)
                                                           (html-entity-ref
                                                            (cdr entry)))))
                                               html-escape-entities))
               (assert-equal #:name "the encoder's own lookup is consistent"
                             #:expect '()
                             #:got (filter-map (lambda (entry)
                                                 (if (equal? (html-char-ref
                                                              (string-ref
                                                               (car entry) 0))
                                                             (cdr entry))
                                                     #f
                                                     (car entry)))
                                               html-escape-entities)))

         (test "html-entity-ref reads a reference"
               (assert-equal #:name "the ampersand entity" #:expect "&"
                             #:got (html-entity-ref "&amp;"))
               (assert-equal #:name "the legacy upper-case spelling too"
                             #:expect "&"
                             #:got (html-entity-ref "&AMP;"))
               (assert-equal #:name "a non-breaking space" #:expect nbsp
                             #:got (html-entity-ref "&nbsp;"))
               (assert-equal #:name "punctuation the old table did not have"
                             #:expect hellip
                             #:got (html-entity-ref "&hellip;"))
               (assert-equal #:name "and another" #:expect mdash
                             #:got (html-entity-ref "&mdash;"))
               (assert-equal #:name "a currency sign" #:expect euro
                             #:got (html-entity-ref "&euro;"))
               (assert-equal #:name "a currency sign's HTML5-only spelling"
                             #:expect "$"
                             #:got (html-entity-ref "&dollar;"))
               (assert-equal #:name "a mathematical symbol" #:expect le
                             #:got (html-entity-ref "&le;"))
               (assert-equal #:name "an arrow" #:expect rarr
                             #:got (html-entity-ref "&rarr;"))
               (assert-equal #:name "a Greek letter" #:expect alpha
                             #:got (html-entity-ref "&alpha;"))
               (assert-equal #:name "case matters: this is the capital" #:expect Alpha
                             #:got (html-entity-ref "&Alpha;"))
               (assert-equal #:name "an entity standing for two code points"
                             #:expect not-equal-tilde
                             #:got (html-entity-ref "&NotEqualTilde;")))

         (test "html-entity-ref refuses what is not a reference"
               (assert-equal #:name "an unknown name" #:expect #f
                             #:got (html-entity-ref "&nope;"))
               (assert-equal #:name "a reference with no semicolon" #:expect #f
                             #:got (html-entity-ref "&aacute"))
               (assert-equal #:name "a name with no ampersand" #:expect #f
                             #:got (html-entity-ref "aacute;"))
               (assert-equal #:name "an empty reference" #:expect #f
                             #:got (html-entity-ref "&;"))
               (assert-equal #:name "the empty string" #:expect #f
                             #:got (html-entity-ref "")))

         (test "html-char-ref writes a reference"
               (assert-equal #:name "a significant character" #:expect "&lt;"
                             #:got (html-char-ref #\<))
               (assert-equal #:name "another" #:expect "&gt;"
                             #:got (html-char-ref #\>))
               (assert-equal #:name "another" #:expect "&amp;"
                             #:got (html-char-ref #\&))
               (assert-equal #:name "another" #:expect "&quot;"
                             #:got (html-char-ref #\"))
               (assert-equal #:name "and the apostrophe HTML5 gave us" #:expect "&apos;"
                             #:got (html-char-ref #\'))
               (assert-equal #:name "a non-breaking space" #:expect "&nbsp;"
                             #:got (html-char-ref (string-ref nbsp 0)))
               (assert-equal #:name "a symbol" #:expect "&copy;"
                             #:got (html-char-ref (string-ref copy 0)))
               (assert-equal #:name "an accent, as before" #:expect "&aacute;"
                             #:got (html-char-ref (string-ref aacute 0)))
               (assert-equal #:name "and another" #:expect "&atilde;"
                             #:got (html-char-ref (string-ref atilde 0)))
               (assert-equal #:name "a character with no entity" #:expect #f
                             #:got (html-char-ref #\a))
               (assert-equal #:name "a digit has none either" #:expect #f
                             #:got (html-char-ref #\7))
               (assert-equal #:name "nor does a slash" #:expect #f
                             #:got (html-char-ref #\/))
               (assert-equal #:name "nor an astral-plane character" #:expect #f
                             #:got (html-char-ref (integer->char #x1F600))))

         (test "html-escape uses the wider table"
               (assert-equal #:name "symbols and accents all become entities"
                             #:expect
                             "Price&hellip; &euro;5 &mdash; &copy; &alpha;&rarr; caf&eacute;"
                             #:got (html-escape
                                    (string-append "Price" hellip " " euro "5 "
                                                   mdash " " copy " " alpha rarr
                                                   " caf" (chars 233))))
               (assert-equal #:name "a non-breaking space is written as one"
                             #:expect "a&nbsp;b"
                             #:got (html-escape (string-append "a" nbsp "b")))
               (assert-equal #:name "the HTML5-only names are still not emitted"
                             #:expect "a/b*c$%"
                             #:got (html-escape "a/b*c$%"))
               (assert-equal #:name "a character with no entity passes through"
                             #:expect (string-append "a" (string (integer->char #x1F600)))
                             #:got (html-escape
                                    (string-append "a" (string (integer->char #x1F600)))))
               (assert-equal #:name "and the five significant ones are unaffected"
                             #:expect "&lt;&gt;&amp;&quot;&apos;"
                             #:got (html-escape "<>&\"'")))

         (test "html-unescape uses the wider table"
               (assert-equal #:name "a string of symbols comes back"
                             #:expect
                             (string-append "Price" hellip " " euro "5 " mdash
                                            " " copy " " alpha rarr)
                             #:got (html-unescape
                                    "Price&hellip; &euro;5 &mdash; &copy; &alpha;&rarr;"))
               (assert-equal #:name "the legacy upper-case spellings still work"
                             #:expect "&<>\""
                             #:got (html-unescape "&AMP;&LT;&GT;&QUOT;"))
               (assert-equal #:name "the HTML5-only names decode too"
                             #:expect "a/b*c$%"
                             #:got (html-unescape "a&sol;b&ast;c&dollar;&percnt;"))
               (assert-equal #:name "a two-code-point reference decodes to two"
                             #:expect (list 2 not-equal-tilde)
                             #:got (list (string-length (html-unescape
                                                         "&NotEqualTilde;"))
                                         (html-unescape "&NotEqualTilde;")))
               (assert-equal #:name "a reference at the very end of the string"
                             #:expect (string-append "5 " le)
                             #:got (html-unescape "5 &le;"))
               (assert-equal #:name "two references with nothing between them"
                             #:expect (string-append alpha Alpha)
                             #:got (html-unescape "&alpha;&Alpha;"))
               (assert-equal #:name "numeric references still work alongside them"
                             #:expect (string-append alpha "'")
                             #:got (html-unescape "&alpha;&#39;"))
               (assert-equal #:name "an unknown name is still left alone"
                             #:expect "&nope;" #:got (html-unescape "&nope;")))

         (test "escaping and unescaping are inverses over the whole table"
               (assert-equal #:name "every escapable character survives a round trip"
                             #:expect '()
                             #:got (filter-map (lambda (entry)
                                                 (let ((char (car entry)))
                                                   (if (equal? (html-unescape
                                                                (html-escape char))
                                                               char)
                                                       #f
                                                       char)))
                                               html-escape-entities))
               (assert-equal #:name "and so does a sentence mixing them"
                             #:expect
                             (string-append "caf" aacute " " euro " " le " 3 "
                                            mdash " " trade)
                             #:got (html-unescape
                                    (html-escape
                                     (string-append "caf" aacute " " euro " "
                                                    le " 3 " mdash " " trade)))))))

(define (spec)
  (veritas-run entities-suite))
