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

(define-module (veritas unit prelude-spec)
  #:use-module (veritas veritas)
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (kracht prelude)
  #:export (spec))

(define prelude-suite
  (suite "Prelude test suite"
         (test "alist-from-url-encoded-body"
               (assert-equal #:name "empty body"
                             #:expect '()
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "")))
               (assert-equal #:name "single key value pair"
                             #:expect '(("name" . "John Doe"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "name=John%20Doe")))
               (assert-equal #:name "multiple key-value pairs"
                             #:expect '(("name" . "Jane Doe") ("age" . "30"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "name=Jane%20Doe&age=30")))
               (assert-equal #:name "key with no value"
                             #:expect '(("key" . ""))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "key=")))

               (assert-equal #:name
                "value with no key (unlikely but possible from split)"
                #:expect '(("" . "value"))
                #:got (alist-from-url-encoded-body (string->utf8 "=value")))

               (assert-equal #:name
                "keys and values with special characters (URL-encoded)"
                #:expect '(("param_1" . "value with spaces")
                           ("param/2" . "another_value!"))
                #:got (alist-from-url-encoded-body (string->utf8
                                                    "param_1=value%20with%20spaces&param%2F2=another_value%21")))
               (assert-equal #:name "duplicate keys"
                             #:expect '(("item" . "apple") ("item" . "orange"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "item=apple&item=orange")))
               (assert-equal #:name "empty key and empty value"
                             #:expect '()
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "=")))
               (assert-equal #:name "single key, no equals sign"
                             #:expect '(("key-only" . ""))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "key-only")))

               (assert-equal #:name "multiple keys, no equals sign"
                             #:expect '(("key1" . "") ("key2" . ""))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "key1&key2")))

               (assert-equal #:name "mixed valid and malformed pairs"
                             #:expect '(("a" . "1") ("b" . "")
                                        ("c" . "3"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "a=1&b&c=3")))
               (assert-equal #:name "space character (%20)"
                             #:expect '(("message" . "hello world"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "message=hello%20world")))

               (assert-equal #:name
                "plus sign (%2B) - often used for space, but distinct from actual '+'"
                #:expect '(("code" . "a+b"))
                #:got (alist-from-url-encoded-body (string->utf8 "code=a%2Bb")))

               (assert-equal #:name "ampersand character (%26)"
                             #:expect '(("data" . "one&two"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "data=one%26two")))

               (assert-equal #:name "equals sign character (%3D)"
                             #:expect '(("equation" . "x=y"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "equation=x%3Dy")))

               (assert-equal #:name "question mark character (%3F)"
                             #:expect '(("query" . "what?"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "query=what%3F")))

               (assert-equal #:name "hash/pound sign character (%23)"
                             #:expect '(("tag" . "#important"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "tag=%23important")))

               (assert-equal #:name "slash character (%2F)"
                             #:expect '(("path" . "/usr/local"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "path=%2Fusr%2Flocal")))

               (assert-equal #:name "backslash character (%5C)"
                             #:expect '(("winpath" . "C:\\Users\\"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "winpath=C%3A%5CUsers%5C")))

               (assert-equal #:name "colon character (%3A)"
                             #:expect '(("protocol" . "http:"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "protocol=http%3A")))

               (assert-equal #:name "semicolon character (%3B)"
                             #:expect '(("cookie" . "id;user"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "cookie=id%3Buser")))

               (assert-equal #:name "comma character (%2C)"
                             #:expect '(("list" . "a,b,c"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "list=a%2Cb%2Cc")))

               (assert-equal #:name "at symbol character (%40)"
                             #:expect '(("email" . "test@example.com"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "email=test%40example.com")))

               (assert-equal #:name "dollar sign character (%24)"
                             #:expect '(("amount" . "$100"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "amount=%24100")))

               (assert-equal #:name "exclamation mark character (%21)"
                             #:expect '(("warning" . "danger!"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "warning=danger%21")))

               (assert-equal #:name "parentheses characters (%28, %29)"
                             #:expect '(("func" . "f(x)"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "func=f%28x%29")))

               (assert-equal #:name "square brackets characters (%5B, %5D)"
                             #:expect '(("array" . "[1,2]"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "array=%5B1%2C2%5D")))

               (assert-equal #:name "curly braces characters (%7B, %7D)"
                             #:expect '(("json" . "{data}"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "json=%7Bdata%7D")))

               (assert-equal #:name
                "unicode characters (e.g., 'é' - %C3%A9 in UTF-8)"
                #:expect '(("name" . "fiancé"))
                #:got (alist-from-url-encoded-body (string->utf8
                                                    "name=fianc%C3%A9")))

               (assert-equal #:name "multiple encoded characters in one value"
                             #:expect '(("complex" . "a&b=c?d e"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "complex=a%26b%3Dc%3Fd%20e"))))

         (test "non-empty-string? tests"
               (assert-true (non-empty-string? "hello")
                            #:name "non-empty-string? with non-empty string")
               (assert-false (non-empty-string? "")
                             #:name "non-empty-string? with empty string")
               (assert-true (non-empty-string? " ")
                            #:name "non-empty-string? with whitespace")
               (assert-false (non-empty-string? #f)
                             #:name "non-empty-string? with #f")
               (assert-true (non-empty-string? #t)
                            #:name "non-empty-string? with #t"))

         (test "empty-string? tests"
               (assert-false (empty-string? "hello")
                             #:name "empty-string? with non-empty string")
               (assert-true (empty-string? "")
                            #:name "empty-string? with empty string")
               (assert-false (empty-string? " ")
                             #:name "empty-string? with whitespace"))

         (test "string-drop tests"
               (assert-equal #:name "string-drop-first-last normal case"
                             #:expect "ell"
                             #:got (string-drop-first-last "hello"))
               (assert-equal #:name "string-drop-first-last single char"
                             #:expect "a"
                             #:got (string-drop-first-last "a"))
               (assert-equal #:name "string-drop-first-last two chars"
                             #:expect "ab"
                             #:got (string-drop-first-last "ab"))
               (assert-equal #:name "string-drop-first-last empty string"
                             #:expect ""
                             #:got (string-drop-first-last ""))

               (assert-equal #:name "string-drop-first-last-line normal case"
                             #:expect "middle line"
                             #:got (string-drop-first-last-line
                                                                "first line\nmiddle line\nlast line"))
               (assert-equal #:name "string-drop-first-last-line single line"
                             #:expect ""
                             #:got (string-drop-first-last-line "single line"))
               (assert-equal #:name "string-drop-first-last-line two lines"
                             #:expect ""
                             #:got (string-drop-first-last-line
                                                                "first line\nlast line"))
               (assert-equal #:name "string-drop-first-last-line empty input"
                             #:expect ""
                             #:got (string-drop-first-last-line "")))))

;;              (test "assoc-ref-or-empty-str tests"
;;                          (assert-equal "empty alist"
;;                                      (assoc-ref-or-empty-str '()
;;                                                              'key) "")

;;                          (assert-equal "key not found"
;;                                      (assoc-ref-or-empty-str '((a . 1) (b . 2))
;;                                                              'c) "")

;;                          (assert-equal "key found - string value"
;;                                      (assoc-ref-or-empty-str '((a . 1)
;;                                                                (b . "hello"))
;;                                                              'b) "hello")

;;                          (assert-equal "key found - non-string value"
;;                                      (assoc-ref-or-empty-str '((a . 1) (b . 2))
;;                                                              'b) 2)

;;                          (assert-equal "key is first element"
;;                                      (assoc-ref-or-empty-str '((key . "value")
;;                                                                (b . 2))
;;                                                              'key) "value")

;;                          (assert-equal "key is last element"
;;                                      (assoc-ref-or-empty-str '((a . 1) (b . 2)
;;                                                                (key . "value"))
;;                                                              'key) "value"))

;;              (test "log tests"
;;                          (test-assert "log-message behaves as expected"
;;                                       (log-message (format #f
;;                                                     "1 + 1 = ~a and 2 + 2 = ~a"
;;                                                     (+ 1 1)
;;                                                     (+ 2 2))))

;;                          (test-assert "log behaves as expected"
;;                                       (log-info "3 + 3 = ~a and 4 + 4 = ~a"
;;                                                 (+ 3 3)
;;                                                 (+ 4 4)))

;;                          (test-assert "log-info behaves as expected"
;;                                       (log-info "5 + 5 = ~a and 6 + 6 = ~a"
;;                                                 (+ 5 5)
;;                                                 (+ 6 6)))

;;                          (test-assert "log-error behaves as expected"
;;                                       (log-error "fake message: ~a"
;;                                                  "something totally expected")))

;;              (test "sha"
;;                          (test-assert "get-short-commit-sha works as expected"
;;                           (write (get-short-commit-sha))))

;;              (test "html-escape tests"
;;                          (assert-equal "Empty string" ""
;;                                      (html-escape ""))
;;                          (assert-equal "String with no special characters"
;;                                      "Hello, World!"
;;                                      (html-escape "Hello, World!"))
;;                          (assert-equal "Single quote" "&apos;"
;;                                      (html-escape "'"))
;;                          (assert-equal "Double quote" "&quot;"
;;                                      (html-escape "\""))
;;                          (assert-equal "Mix of quotes"
;;                                      "He&apos;s a &quot;guy&quot;."
;;                                      (html-escape "He's a \"guy\"."))
;;                          (assert-equal "Accented a" "&aacute;"
;;                                      (html-escape "á"))
;;                          (assert-equal "Accented A" "&Aacute;"
;;                                      (html-escape "Á"))
;;                          (assert-equal "Accented e" "&eacute;"
;;                                      (html-escape "é"))
;;                          (assert-equal "Accented E" "&Eacute;"
;;                                      (html-escape "É"))
;;                          (assert-equal "Accented i" "&iacute;"
;;                                      (html-escape "í"))
;;                          (assert-equal "Accented I" "&Iacute;"
;;                                      (html-escape "Í"))
;;                          (assert-equal "Accented o" "&oacute;"
;;                                      (html-escape "ó"))
;;                          (assert-equal "Accented O" "&Oacute;"
;;                                      (html-escape "Ó"))
;;                          (assert-equal "Accented u" "&uacute;"
;;                                      (html-escape "ú"))
;;                          (assert-equal "Accented U" "&Uacute;"
;;                                      (html-escape "Ú"))
;;                          (assert-equal "C-cedilla" "&ccedil;"
;;                                      (html-escape "ç"))
;;                          (assert-equal "N-tilde" "&ntilde;"
;;                                      (html-escape "ñ"))
;;                          (assert-equal "Multiple accents"
;;                                      "&Aacute;rvore &eacute; bonita"
;;                                      (html-escape "Árvore é bonita"))
;;                          (assert-equal "String with multiple occurrences"
;;                           "He said, &quot;It&apos;s a beautiful day!&quot;"
;;                           (html-escape "He said, \"It's a beautiful day!\""))
;;                          (assert-equal "Example sentence escaped"
;;                           "&Oacute;l&aacute;, m&uacute;ndo! Isto &eacute; um teste."
;;                           (html-escape "Ólá, múndo! Isto é um teste.")))

;;              (test "html-unescape tests"
;;                          (assert-equal "Empty string" ""
;;                                      (html-unescape ""))
;;                          (assert-equal "String with no entities" "Hello, World!"
;;                                      (html-unescape "Hello, World!"))
;;                          (assert-equal "Single apos entity" "'"
;;                                      (html-unescape "&apos;"))
;;                          (assert-equal "Single quot entity" "\""
;;                                      (html-unescape "&quot;"))
;;                          (assert-equal "Mix of entities" "He's a \"guy\"."
;;                                      (html-unescape
;;                                       "He&apos;s a &quot;guy&quot;."))
;;                          (assert-equal "Accented a entity" "á"
;;                                      (html-unescape "&aacute;"))
;;                          (assert-equal "Accented A entity" "Á"
;;                                      (html-unescape "&Aacute;"))
;;                          (assert-equal "C-cedilla entity" "ç"
;;                                      (html-unescape "&ccedil;"))
;;                          (assert-equal "Multiple entity occurrences"
;;                                      "It's a \"quote\"."
;;                                      (html-unescape
;;                                       "It&apos;s a &quot;quote&quot;."))
;;                          (assert-equal "Example sentence unescaped"
;;                                      "Ólá, múndo! Isto é um teste."
;;                                      (html-unescape
;;                                       "&Oacute;l&aacute;, m&uacute;ndo! Isto &eacute; um teste.")))

;;              (test "Round-trip tests"
;;                          (assert-equal "Escape then unescape"
;;                                      "Hello, it's a \"test\" with áéíóú."
;;                                      (html-unescape (html-escape
;;                                                      "Hello, it's a \"test\" with áéíóú.")))
;;                          (assert-equal "Example sentence"
;;                                      "Olá, múndo! Isto é um teste."
;;                                      (html-unescape (html-escape
;;                                                      "Olá, múndo! Isto é um teste.")))
;;                          (assert-equal
;;                           "Unescape then escape (assuming valid entities)"
;;                           "&lt;div&gt;&apos;test&apos;&lt;&sol;div&gt;"
;;                           (html-escape (html-unescape
;;                                         "&lt;div&gt;&apos;test&apos;&lt;&sol;div&gt;")))
;;                          (assert-equal
;;                           "Unescape then escape (for chars in alist)"
;;                           "&apos;&aacute;&eacute;&iacute;&oacute;&uacute;&quot;"
;;                           (html-escape (html-unescape
;;                                         "&apos;&aacute;&eacute;&iacute;&oacute;&uacute;&quot;")))))

(define (spec)
  (veritas-run prelude-suite))
