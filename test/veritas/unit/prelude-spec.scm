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
               ;;
               )))

;;                          (test-equal "single key, no equals sign"
;;                                      '(("key-only" . "")) ;This is how string-split on '=' will behave
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "key-only")))

;;                          (test-equal "multiple keys, no equals sign"
;;                                      '(("key1" . "") ("key2" . ""))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "key1&key2")))

;;                          (test-equal "mixed valid and malformed pairs"
;;                                      '(("a" . "1") ("b" . "")
;;                                        ("c" . "3"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "a=1&b&c=3")))
;;                          (test-equal "space character (%20)"
;;                                      '(("message" . "hello world"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "message=hello%20world")))

;;                          (test-equal
;;                           "plus sign (%2B) - often used for space, but distinct from actual '+'"
;;                           '(("code" . "a+b"))
;;                           (alist-from-url-encoded-body (string->utf8
;;                                                         "code=a%2Bb")))

;;                          (test-equal "ampersand character (%26)"
;;                                      '(("data" . "one&two"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "data=one%26two")))

;;                          (test-equal "equals sign character (%3D)"
;;                                      '(("equation" . "x=y"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "equation=x%3Dy")))

;;                          (test-equal "question mark character (%3F)"
;;                                      '(("query" . "what?"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "query=what%3F")))

;;                          (test-equal "hash/pound sign character (%23)"
;;                                      '(("tag" . "#important"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "tag=%23important")))

;;                          (test-equal "slash character (%2F)"
;;                                      '(("path" . "/usr/local"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "path=%2Fusr%2Flocal")))

;;                          (test-equal "backslash character (%5C)"
;;                                      '(("winpath" . "C:\\Users\\"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "winpath=C%3A%5CUsers%5C")))

;;                          (test-equal "colon character (%3A)"
;;                                      '(("protocol" . "http:"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "protocol=http%3A")))

;;                          (test-equal "semicolon character (%3B)"
;;                                      '(("cookie" . "id;user"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "cookie=id%3Buser")))

;;                          (test-equal "comma character (%2C)"
;;                                      '(("list" . "a,b,c"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "list=a%2Cb%2Cc")))

;;                          (test-equal "at symbol character (%40)"
;;                                      '(("email" . "test@example.com"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "email=test%40example.com")))

;;                          (test-equal "dollar sign character (%24)"
;;                                      '(("amount" . "$100"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "amount=%24100")))

;;                          (test-equal "exclamation mark character (%21)"
;;                                      '(("warning" . "danger!"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "warning=danger%21")))

;;                          (test-equal "parentheses characters (%28, %29)"
;;                                      '(("func" . "f(x)"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "func=f%28x%29")))

;;                          (test-equal "square brackets characters (%5B, %5D)"
;;                                      '(("array" . "[1,2]"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "array=%5B1%2C2%5D")))

;;                          (test-equal "curly braces characters (%7B, %7D)"
;;                                      '(("json" . "{data}"))
;;                                      (alist-from-url-encoded-body (string->utf8
;;                                                                    "json=%7Bdata%7D")))

;;                          (test-equal
;;                           "unicode characters (e.g., 'é' - %C3%A9 in UTF-8)"
;;                           '(("name" . "fiancé"))
;;                           (alist-from-url-encoded-body (string->utf8
;;                                                         "name=fianc%C3%A9")))

;;                          (test-equal
;;                           "multiple encoded characters in one value"
;;                           '(("complex" . "a&b=c?d e"))
;;                           (alist-from-url-encoded-body (string->utf8
;;                                                         "complex=a%26b%3Dc%3Fd%20e"))))

;;              (test-group "non-empty-string? tests"
;;                          (test-equal "non-empty-string? with non-empty string"
;;                           #t
;;                           (non-empty-string? "hello"))
;;                          (test-equal "non-empty-string? with empty string" #f
;;                                      (non-empty-string? ""))
;;                          (test-equal "non-empty-string? with whitespace" #t
;;                                      (non-empty-string? " "))
;;                          (test-equal "non-empty-string? with #f" #f
;;                                      (non-empty-string? #f))
;;                          (test-equal "non-empty-string? with #t" #t
;;                                      (non-empty-string? #t)))

;;              (test-group "empty-string? tests"
;;                          (test-equal "empty-string? with non-empty string" #f
;;                                      (empty-string? "hello"))
;;                          (test-equal "empty-string? with empty string" #t
;;                                      (empty-string? ""))
;;                          (test-equal "empty-string? with whitespace" #f
;;                                      (empty-string? " ")))

;;              (test-group "string-drop tests"
;;                          (test-equal "string-drop-first-last normal case"
;;                                      "ell"
;;                                      (string-drop-first-last "hello"))
;;                          (test-equal "string-drop-first-last single char" "a"
;;                                      (string-drop-first-last "a"))
;;                          (test-equal "string-drop-first-last two chars" "ab"
;;                                      (string-drop-first-last "ab"))
;;                          (test-equal "string-drop-first-last empty string" ""
;;                                      (string-drop-first-last ""))

;;                          (test-equal "string-drop-first-last-line normal case"
;;                           "middle line"
;;                           (string-drop-first-last-line
;;                                                        "first line\nmiddle line\nlast line"))
;;                          (test-equal "string-drop-first-last-line single line"
;;                           ""
;;                           (string-drop-first-last-line "single line"))
;;                          (test-equal "string-drop-first-last-line two lines"
;;                                      ""
;;                                      (string-drop-first-last-line
;;                                       "first line\nlast line"))
;;                          (test-equal "string-drop-first-last-line empty input"
;;                           ""
;;                           (string-drop-first-last-line "")))

;;              (test-group "assoc-ref-or-empty-str tests"
;;                          (test-equal "empty alist"
;;                                      (assoc-ref-or-empty-str '()
;;                                                              'key) "")

;;                          (test-equal "key not found"
;;                                      (assoc-ref-or-empty-str '((a . 1) (b . 2))
;;                                                              'c) "")

;;                          (test-equal "key found - string value"
;;                                      (assoc-ref-or-empty-str '((a . 1)
;;                                                                (b . "hello"))
;;                                                              'b) "hello")

;;                          (test-equal "key found - non-string value"
;;                                      (assoc-ref-or-empty-str '((a . 1) (b . 2))
;;                                                              'b) 2)

;;                          (test-equal "key is first element"
;;                                      (assoc-ref-or-empty-str '((key . "value")
;;                                                                (b . 2))
;;                                                              'key) "value")

;;                          (test-equal "key is last element"
;;                                      (assoc-ref-or-empty-str '((a . 1) (b . 2)
;;                                                                (key . "value"))
;;                                                              'key) "value"))

;;              (test-group "log tests"
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

;;              (test-group "sha"
;;                          (test-assert "get-short-commit-sha works as expected"
;;                           (write (get-short-commit-sha))))

;;              (test-group "html-escape tests"
;;                          (test-equal "Empty string" ""
;;                                      (html-escape ""))
;;                          (test-equal "String with no special characters"
;;                                      "Hello, World!"
;;                                      (html-escape "Hello, World!"))
;;                          (test-equal "Single quote" "&apos;"
;;                                      (html-escape "'"))
;;                          (test-equal "Double quote" "&quot;"
;;                                      (html-escape "\""))
;;                          (test-equal "Mix of quotes"
;;                                      "He&apos;s a &quot;guy&quot;."
;;                                      (html-escape "He's a \"guy\"."))
;;                          (test-equal "Accented a" "&aacute;"
;;                                      (html-escape "á"))
;;                          (test-equal "Accented A" "&Aacute;"
;;                                      (html-escape "Á"))
;;                          (test-equal "Accented e" "&eacute;"
;;                                      (html-escape "é"))
;;                          (test-equal "Accented E" "&Eacute;"
;;                                      (html-escape "É"))
;;                          (test-equal "Accented i" "&iacute;"
;;                                      (html-escape "í"))
;;                          (test-equal "Accented I" "&Iacute;"
;;                                      (html-escape "Í"))
;;                          (test-equal "Accented o" "&oacute;"
;;                                      (html-escape "ó"))
;;                          (test-equal "Accented O" "&Oacute;"
;;                                      (html-escape "Ó"))
;;                          (test-equal "Accented u" "&uacute;"
;;                                      (html-escape "ú"))
;;                          (test-equal "Accented U" "&Uacute;"
;;                                      (html-escape "Ú"))
;;                          (test-equal "C-cedilla" "&ccedil;"
;;                                      (html-escape "ç"))
;;                          (test-equal "N-tilde" "&ntilde;"
;;                                      (html-escape "ñ"))
;;                          (test-equal "Multiple accents"
;;                                      "&Aacute;rvore &eacute; bonita"
;;                                      (html-escape "Árvore é bonita"))
;;                          (test-equal "String with multiple occurrences"
;;                           "He said, &quot;It&apos;s a beautiful day!&quot;"
;;                           (html-escape "He said, \"It's a beautiful day!\""))
;;                          (test-equal "Example sentence escaped"
;;                           "&Oacute;l&aacute;, m&uacute;ndo! Isto &eacute; um teste."
;;                           (html-escape "Ólá, múndo! Isto é um teste.")))

;;              (test-group "html-unescape tests"
;;                          (test-equal "Empty string" ""
;;                                      (html-unescape ""))
;;                          (test-equal "String with no entities" "Hello, World!"
;;                                      (html-unescape "Hello, World!"))
;;                          (test-equal "Single apos entity" "'"
;;                                      (html-unescape "&apos;"))
;;                          (test-equal "Single quot entity" "\""
;;                                      (html-unescape "&quot;"))
;;                          (test-equal "Mix of entities" "He's a \"guy\"."
;;                                      (html-unescape
;;                                       "He&apos;s a &quot;guy&quot;."))
;;                          (test-equal "Accented a entity" "á"
;;                                      (html-unescape "&aacute;"))
;;                          (test-equal "Accented A entity" "Á"
;;                                      (html-unescape "&Aacute;"))
;;                          (test-equal "C-cedilla entity" "ç"
;;                                      (html-unescape "&ccedil;"))
;;                          (test-equal "Multiple entity occurrences"
;;                                      "It's a \"quote\"."
;;                                      (html-unescape
;;                                       "It&apos;s a &quot;quote&quot;."))
;;                          (test-equal "Example sentence unescaped"
;;                                      "Ólá, múndo! Isto é um teste."
;;                                      (html-unescape
;;                                       "&Oacute;l&aacute;, m&uacute;ndo! Isto &eacute; um teste.")))

;;              (test-group "Round-trip tests"
;;                          (test-equal "Escape then unescape"
;;                                      "Hello, it's a \"test\" with áéíóú."
;;                                      (html-unescape (html-escape
;;                                                      "Hello, it's a \"test\" with áéíóú.")))
;;                          (test-equal "Example sentence"
;;                                      "Olá, múndo! Isto é um teste."
;;                                      (html-unescape (html-escape
;;                                                      "Olá, múndo! Isto é um teste.")))
;;                          (test-equal
;;                           "Unescape then escape (assuming valid entities)"
;;                           "&lt;div&gt;&apos;test&apos;&lt;&sol;div&gt;"
;;                           (html-escape (html-unescape
;;                                         "&lt;div&gt;&apos;test&apos;&lt;&sol;div&gt;")))
;;                          (test-equal
;;                           "Unescape then escape (for chars in alist)"
;;                           "&apos;&aacute;&eacute;&iacute;&oacute;&uacute;&quot;"
;;                           (html-escape (html-unescape
;;                                         "&apos;&aacute;&eacute;&iacute;&oacute;&uacute;&quot;")))))

(define (spec)
  (veritas-run prelude-suite))
