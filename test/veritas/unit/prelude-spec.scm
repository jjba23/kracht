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

(define another-suite
  (suite "Another test suite"
         (test "sha tests"
               (assert-true (string? (get-short-commit-sha))
                            #:name "get-short-commit-sha works as expected"))

         (test "html-escape tests"
               (assert-equal #:got (html-escape "")
                             #:expect ""
                             #:name "Empty string")
               (assert-equal #:got (html-escape "Hello, World!")
                             #:expect "Hello, World!"
                             #:name "String with no special characters")
               (assert-equal #:got (html-escape "'")
                             #:expect "&apos;"
                             #:name "Single quote")
               (assert-equal #:got (html-escape "\"")
                             #:expect "&quot;"
                             #:name "Double quote")
               (assert-equal #:got (html-escape "He's a \"guy\".")
                             #:expect "He&apos;s a &quot;guy&quot;."
                             #:name "Mix of quotes")
               (assert-equal #:got (html-escape "É")
                             #:expect "&Eacute;"
                             #:name "Accented E")
               (assert-equal #:got (html-escape "í")
                             #:expect "&iacute;"
                             #:name "Accented i")
               (assert-equal #:got (html-escape "Í")
                             #:expect "&Iacute;"
                             #:name "Accented I")
               (assert-equal #:got (html-escape "ó")
                             #:expect "&oacute;"
                             #:name "Accented o")
               (assert-equal #:got (html-escape "Ó")
                             #:expect "&Oacute;"
                             #:name "Accented O")
               (assert-equal #:got (html-escape "ú")
                             #:expect "&uacute;"
                             #:name "Accented u")
               (assert-equal #:got (html-escape "Ú")
                             #:expect "&Uacute;"
                             #:name "Accented U")
               (assert-equal #:got (html-escape "ç")
                             #:expect "&ccedil;"
                             #:name "C-cedilla")
               (assert-equal #:got (html-escape "ñ")
                             #:expect "&ntilde;"
                             #:name "N-tilde")
               (assert-equal #:got (html-escape "Árvore é bonita")
                             #:expect "&Aacute;rvore &eacute; bonita"
                             #:name "Multiple accents")
               (assert-equal #:got (html-escape
                                    "He said, \"It's a beautiful day!\"")
                #:expect "He said, &quot;It&apos;s a beautiful day!&quot;"
                #:name "String with multiple occurrences")
               (assert-equal #:got (html-escape "Ólá, múndo! Isto é um teste.")
                #:expect
                "&Oacute;l&aacute;, m&uacute;ndo! Isto &eacute; um teste."
                #:name "Example sentence escaped"))

         (test "html-unescape tests"
               (assert-equal #:got (html-unescape "")
                             #:expect ""
                             #:name "Empty string")
               (assert-equal #:got (html-unescape "Hello, World!")
                             #:expect "Hello, World!"
                             #:name "String with no entities")
               (assert-equal #:got (html-unescape "&apos;")
                             #:expect "'"
                             #:name "Single apos entity")
               (assert-equal #:got (html-unescape "&quot;")
                             #:expect "\""
                             #:name "Single quot entity")
               (assert-equal #:got (html-unescape
                                    "He&apos;s a &quot;guy&quot;.")
                             #:expect "He's a \"guy\"."
                             #:name "Mix of entities")
               (assert-equal #:got (html-unescape "&aacute;")
                             #:expect "á"
                             #:name "Accented a entity")
               (assert-equal #:got (html-unescape "&Aacute;")
                             #:expect "Á"
                             #:name "Accented A entity")
               (assert-equal #:got (html-unescape "&ccedil;")
                             #:expect "ç"
                             #:name "C-cedilla entity")
               (assert-equal #:got (html-unescape
                                    "It&apos;s a &quot;quote&quot;.")
                             #:expect "It's a \"quote\"."
                             #:name "Multiple entity occurrences")
               (assert-equal #:got (html-unescape
                                    "&Oacute;l&aacute;, m&uacute;ndo! Isto &eacute; um teste.")
                             #:expect "Ólá, múndo! Isto é um teste."
                             #:name "Example sentence unescaped"))

         (test "Round-trip tests"
               (assert-equal #:got (html-unescape (html-escape
                                                   "Hello, it's a \"test\" with áéíóú."))
                             #:expect "Hello, it's a \"test\" with áéíóú."
                             #:name "Escape then unescape")
               (assert-equal #:got (html-unescape (html-escape
                                                   "Olá, múndo! Isto é um teste."))
                             #:expect "Olá, múndo! Isto é um teste."
                             #:name "Example sentence")
               (assert-equal #:got (html-escape (html-unescape
                                                 "&lt;div&gt;&apos;test&apos;&lt;&sol;div&gt;"))
                             #:expect
                             "&lt;div&gt;&apos;test&apos;&lt;&sol;div&gt;"
                             #:name
                             "Unescape then escape (assuming valid entities)")
               (assert-equal #:got (html-escape (html-unescape
                                                 "&apos;&aacute;&eacute;&iacute;&oacute;&uacute;&quot;"))
                #:expect
                "&apos;&aacute;&eacute;&iacute;&oacute;&uacute;&quot;"
                #:name "Unescape then escape (for chars in alist)"))))

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
                             #:got (string-drop-first-last-line "")))
         (test "assoc-ref-or-empty-str tests"
               (assert-equal #:name "empty alist"
                             #:got (assoc-ref-or-empty-str '()
                                                           'key)
                             #:expect "")

               (assert-equal #:name "key not found"
                             #:got (assoc-ref-or-empty-str '((a . 1) (b . 2))
                                                           'c)
                             #:expect "")

               (assert-equal #:name "key found - string value"
                             #:got (assoc-ref-or-empty-str '((a . 1)
                                                             (b . "hello"))
                                                           'b)
                             #:expect "hello")

               (assert-equal #:name "key found - non-string value"
                             #:got (assoc-ref-or-empty-str '((a . 1) (b . 2))
                                                           'b)
                             #:expect 2)

               (assert-equal #:name "key is first element"
                             #:got (assoc-ref-or-empty-str '((key . "value")
                                                             (b . 2))
                                                           'key)
                             #:expect "value")

               (assert-equal #:name "key is last element"
                             #:got (assoc-ref-or-empty-str '((a . 1) (b . 2)
                                                             (key . "value"))
                                                           'key)
                             #:expect "value"))))

(define (spec)
  (veritas-run prelude-suite another-suite))
