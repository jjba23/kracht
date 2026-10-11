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

(define test-file-path
  (string-append (or (getenv "TMPDIR") "/tmp")
                 "/kracht-write-read-test.txt"))

;; Assertion arguments are evaluated when the suite is built, so the fixture
;; has to exist by then; the assertions themselves run later.
(define file-write-char-count
  (write-file-string test-file-path "line 1\nline 2\n"))

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
                             "&lt;div&gt;&apos;test&apos;&lt;/div&gt;"
                             #:name
                             "Unescape then escape round-trips the significant characters")
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
                                                                 "complex=a%26b%3Dc%3Fd%20e")))

               (assert-equal #:name "literal equals sign stays part of the value"
                             #:expect '(("q" . "a=b"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "q=a=b")))

               (assert-equal #:name "empty &-separated segments are skipped"
                             #:expect '(("a" . "1") ("b" . "2"))
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "a=1&&b=2")))

               (assert-equal #:name "only an ampersand"
                             #:expect '()
                             #:got (alist-from-url-encoded-body (string->utf8
                                                                 "&"))))

         (test "non-empty-string? tests"
               (assert-true (non-empty-string? "hello")
                            #:name "non-empty-string? with non-empty string")
               (assert-false (non-empty-string? "")
                             #:name "non-empty-string? with empty string")
               (assert-true (non-empty-string? " ")
                            #:name "non-empty-string? with whitespace")
               (assert-false (non-empty-string? #f)
                             #:name "non-empty-string? with #f")
               (assert-false (non-empty-string? #t)
                             #:name "non-empty-string? with #t (not a string)")
               (assert-false (non-empty-string? 42)
                             #:name "non-empty-string? with a number"))

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

         (test "string-drop-first-last-n tests"
               (assert-equal #:name "drop one leading and the last char"
                             #:expect "ell"
                             #:got (string-drop-first-last-n "hello" 1))
               (assert-equal #:name "drop two leading and the last char"
                             #:expect "ll"
                             #:got (string-drop-first-last-n "hello" 2))
               (assert-equal #:name "n = 0 drops only the last char"
                             #:expect "hell"
                             #:got (string-drop-first-last-n "hello" 0))
               (assert-equal #:name "n at the boundary returns empty string"
                             #:expect ""
                             #:got (string-drop-first-last-n "hello" 4))
               (assert-equal #:name "n larger than length is clamped"
                             #:expect ""
                             #:got (string-drop-first-last-n "hello" 99))
               (assert-equal #:name "negative n is clamped to zero"
                             #:expect "hell"
                             #:got (string-drop-first-last-n "hello" -5))
               (assert-equal #:name "short strings are returned unaltered"
                             #:expect "ab"
                             #:got (string-drop-first-last-n "ab" 1)))

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
                             #:expect "value"))

         (test "assoc-ref-or tests"
               (assert-equal #:name "found key"
                             #:got (assoc-ref-or '((a . 1)) 'a)
                             #:expect 1)
               (assert-equal #:name "missing key defaults to #f"
                             #:got (assoc-ref-or '((a . 1)) 'z)
                             #:expect #f)
               (assert-equal #:name "missing key uses the given default"
                             #:got (assoc-ref-or '((a . 1)) 'z #:default 'missing)
                             #:expect 'missing))

         (test "alist-set tests"
               (assert-equal #:name "replace an existing key in place"
                             #:got (alist-set '((a . 1) (b . 2)) 'a 9)
                             #:expect '((a . 9) (b . 2)))
               (assert-equal #:name "add a new key at the front"
                             #:got (alist-set '((a . 1)) 'c 3)
                             #:expect '((c . 3) (a . 1)))
               (assert-equal #:name "the original alist is never mutated"
                             #:got (let ((orig '((a . 1))))
                                     (alist-set orig 'b 2)
                                     orig)
                             #:expect '((a . 1))))

         (test "assoc-lens tests"
               (assert-equal #:name "nested lookup"
                             #:got (assoc-lens '((a (b . 1))) '(a b))
                             #:expect 1)
               (assert-equal #:name "missing nested key"
                             #:got (assoc-lens '((a (b . 1))) '(z b))
                             #:expect #f)
               (assert-equal #:name "intermediate value is not an alist"
                             #:got (assoc-lens '((a . 1)) '(a b))
                             #:expect #f))))

(define robustness-suite
  (suite "Robustness test suite"
         (test "base64-encode known vectors"
               (assert-equal #:name "empty string"
                             #:expect ""
                             #:got (base64-encode ""))
               (assert-equal #:name "one byte needs two padding chars"
                             #:expect "Zg=="
                             #:got (base64-encode "f"))
               (assert-equal #:name "two bytes need one padding char"
                             #:expect "Zm8="
                             #:got (base64-encode "fo"))
               (assert-equal #:name "three bytes need no padding"
                             #:expect "Zm9v"
                             #:got (base64-encode "foo"))
               (assert-equal #:name "hello"
                             #:expect "aGVsbG8="
                             #:got (base64-encode "hello"))
               (assert-equal #:name "hello world"
                             #:expect "aGVsbG8gd29ybGQ="
                             #:got (base64-encode "hello world")))

         (test "base64-decode known vectors"
               (assert-equal #:name "empty string"
                             #:expect ""
                             #:got (base64-decode ""))
               (assert-equal #:name "padded one byte"
                             #:expect "f"
                             #:got (base64-decode "Zg=="))
               (assert-equal #:name "padded two bytes"
                             #:expect "fo"
                             #:got (base64-decode "Zm8="))
               (assert-equal #:name "unpadded three bytes"
                             #:expect "foo"
                             #:got (base64-decode "Zm9v"))
               (assert-equal #:name "hello world"
                             #:expect "hello world"
                             #:got (base64-decode "aGVsbG8gd29ybGQ=")))

         (test "base64 round trips"
               (assert-equal #:name "ascii"
                             #:expect "hello world"
                             #:got (base64-decode (base64-encode "hello world")))
               (assert-equal #:name "utf-8"
                             #:expect "héllo wörld áéíóú"
                             #:got (base64-decode (base64-encode "héllo wörld áéíóú")))
               (assert-equal #:name "newlines inside the payload"
                             #:expect "line one\nline two"
                             #:got (base64-decode (base64-encode "line one\nline two")))
               (assert-equal #:name "embedded heredoc-looking terminator is safe"
                             #:expect "x\nEOFEOF\ny"
                             #:got (base64-decode (base64-encode "x\nEOFEOF\ny")))
               (assert-equal #:name "whitespace in the encoded input is ignored"
                             #:expect "hello"
                             #:got (base64-decode "aGVs\nbG8=")))

         (test "html escaping covers the HTML-significant characters"
               (assert-equal #:name "ampersand is escaped"
                             #:expect "a &amp; b"
                             #:got (html-escape "a & b"))
               (assert-equal #:name "angle brackets are escaped"
                             #:expect "&lt;script&gt;"
                             #:got (html-escape "<script>"))
               (assert-equal #:name "both quote styles are escaped"
                             #:expect "&quot;x&quot; &apos;y&apos;"
                             #:got (html-escape "\"x\" 'y'"))
               (assert-equal #:name
                             "slashes, stars, dollars and percent are left alone"
                             #:expect "a/b*c$%"
                             #:got (html-escape "a/b*c$%"))
               (assert-equal #:name "escaping is not idempotent"
                             #:expect "&amp;amp;"
                             #:got (html-escape "&amp;"))
               (assert-equal #:name "legacy entities are still decoded"
                             #:expect "a/b*c$%"
                             #:got (html-unescape "a&sol;b&ast;c&dollar;&percnt;"))
               (assert-equal #:name "numeric character references are decoded"
                             #:expect "'&"
                             #:got (html-unescape "&#39;&#x26;"))
               (assert-equal #:name "uppercase legacy entities are decoded"
                             #:expect "&<>\""
                             #:got (html-unescape "&AMP;&LT;&GT;&QUOT;"))
               (assert-equal #:name "malformed numeric references are untouched"
                             #:expect "&#; &#x; &#0; &#xg;"
                             #:got (html-unescape "&#; &#x; &#0; &#xg;"))
               (assert-equal #:name "unknown entity left untouched"
                             #:expect "&unknown;"
                             #:got (html-unescape "&unknown;")))

         (test "SQL literal escaping"
               (assert-equal #:name "escape-sql-string doubles quotes"
                             #:expect "O''Brien"
                             #:got (escape-sql-string "O'Brien"))
               (assert-equal #:name "value->sql-literal number"
                             #:expect "42"
                             #:got (value->sql-literal 42))
               (assert-equal #:name "value->sql-literal boolean true"
                             #:expect "1"
                             #:got (value->sql-literal #t))
               (assert-equal #:name "value->sql-literal boolean false"
                             #:expect "0"
                             #:got (value->sql-literal #f))
               (assert-equal #:name "value->sql-literal plain string"
                             #:expect "'hello'"
                             #:got (value->sql-literal "hello"))
               (assert-equal #:name "value->sql-literal escapes quotes"
                             #:expect "'O''Brien'"
                             #:got (value->sql-literal "O'Brien"))
               (assert-equal #:name "pair->sql-assignment escapes quotes"
                             #:expect "name = 'O''Brien'"
                             #:got (pair->sql-assignment '(name . "O'Brien")))
               (assert-equal #:name "pair->sql-assignment keeps numbers raw"
                             #:expect "age = 30"
                             #:got (pair->sql-assignment '(age . 30)))
               (assert-equal #:name "pair->sql-assignment hyphenated column"
                             #:expect "first_name = 'Josep'"
                             #:got (pair->sql-assignment
                                    '(first-name . "Josep")))
               (assert-equal #:name "escape-sql-string rejects NUL"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (escape-sql-string (string #\nul)))
                                          (lambda args 'rejected)))
               (assert-equal #:name
                             "escape-sql-string doubles backslashes on request"
                             #:expect "a\\\\b''c"
                             #:got (escape-sql-string "a\\b'c"
                                                      #:escape-backslash? #t))
               (assert-equal #:name "quote-sql-identifier wraps a symbol"
                             #:expect "\"select\""
                             #:got (quote-sql-identifier 'select))
               (assert-equal #:name
                             "quote-sql-identifier doubles embedded quotes"
                             #:expect "\"we\"\"ird\""
                             #:got (quote-sql-identifier "we\"ird")))

         (test "file read/write helpers"
               (assert-equal #:name "write-file-string returns the character count"
                             #:expect 14
                             #:got file-write-char-count)
               (assert-equal #:name "read-file-string returns exact contents"
                             #:expect "line 1\nline 2\n"
                             #:got (read-file-string test-file-path))
               (assert-no-error (lambda ()
                                  (delete-file test-file-path))
                                #:name "cleanup the temporary file"))

         (test "shuffle-fy preserves the elements"
               (assert-equal #:name "a shuffled range is still the range"
                             #:expect (iota 50)
                             #:got (sort (shuffle-fy (iota 50)
                                                     #:state (seed->random-state
                                                              7)) <))
               (assert-equal #:name "empty list"
                             #:expect '()
                             #:got (shuffle-fy '()))
               (assert-equal #:name "single element list"
                             #:expect '(1)
                             #:got (shuffle-fy '(1)))
               (assert-equal #:name "the same seed yields the same order"
                             #:expect (shuffle-fy (iota 20)
                                                  #:state (seed->random-state 99))
                             #:got (shuffle-fy (iota 20)
                                               #:state (seed->random-state 99))))

         (test "shuffle-fy does not mutate its input"
               (let ((xs (list 1 2 3 4 5)))
                 (shuffle-fy xs #:state (seed->random-state 1))
                 (assert-equal #:name "the original list is unchanged"
                               #:expect '(1 2 3 4 5)
                               #:got xs)))))

(define reexports-suite
  (suite "SRFI re-exports"
         (test "srfi-1 list procedures"
               (assert-equal #:name "fold" #:expect 6 #:got (fold + 0 '(1 2 3)))
               (assert-equal #:name "fold-right"
                             #:expect '(1 2 3)
                             #:got (fold-right cons '() '(1 2 3)))
               (assert-equal #:name "filter-map"
                             #:expect '(1 9)
                             #:got (filter-map (lambda (x)
                                                 (and (odd? x) (* x x)))
                                               '(1 2 3 4)))
               (assert-equal #:name "count"
                             #:expect 2
                             #:got (count even? '(1 2 3 4)))
               (assert-equal #:name "take and drop"
                             #:expect '((1 2) (3 4))
                             #:got (list (take '(1 2 3 4) 2)
                                         (drop '(1 2 3 4) 2)))
               (assert-equal #:name "list-tabulate"
                             #:expect '(0 1 4)
                             #:got (list-tabulate 3 (lambda (i) (* i i))))
               (assert-equal #:name "delete-duplicates"
                             #:expect '(1 2 3)
                             #:got (delete-duplicates '(1 2 1 3 2)))
               (assert-equal #:name "zip"
                             #:expect '((1 a) (2 b))
                             #:got (zip '(1 2) '(a b)))
               (assert-equal #:name "reduce"
                             #:expect 6
                             #:got (reduce + 0 '(1 2 3)))
               (assert-equal #:name "lset-union"
                             #:expect '(1 2 3)
                             #:got (sort (lset-union = '(1 2) '(2 3)) <)))

         (test "srfi-2, 8, 11 and 26 syntactic forms"
               (assert-equal #:name "and-let* success"
                             #:expect 'ok
                             #:got (and-let* ((x 5) ((> x 2))) 'ok))
               (assert-false (and-let* ((x 5) ((< x 2))) 'ok)
                             #:name "and-let* failure")
               (assert-equal #:name "receive"
                             #:expect '(1 2)
                             #:got (call-with-values (lambda () (values 1 2))
                                     (lambda (a b) (list a b))))
               (assert-equal #:name "let-values"
                             #:expect 7
                             #:got (let-values (((a b) (values 3 4)))
                                     (+ a b)))
               (assert-equal #:name "let*-values"
                             #:expect 2
                             #:got (let*-values (((a) (values 1))
                                                 ((b) (values (+ a 1))))
                                     b))
               (assert-equal #:name "cut"
                             #:expect 3
                             #:got ((cut + 1 2)))
               (assert-equal #:name "cute"
                             #:expect '(2 3)
                             #:got (let ((y 2))
                                     (map (cute + y <>) '(0 1)))))

         (test "srfi-43 vector procedures"
               (assert-equal #:name "vector-fold"
                             #:expect 6
                             #:got (vector-fold (lambda (i acc v)
                                                  (+ acc v))
                                                0 #(1 2 3)))
               (assert-equal #:name "vector-for-each"
                             #:expect 6
                             #:got (let ((s 0))
                                     (vector-for-each (lambda (i v)
                                                        (set! s (+ s v)))
                                                      #(1 2 3))
                                     s))
               (assert-equal #:name "vector-map receives the index"
                             #:expect #(0 2 6)
                             #:got (vector-map (lambda (i v) (* i v))
                                               #(1 2 3)))
               (assert-equal #:name "vector-index"
                             #:expect 2
                             #:got (vector-index even? #(1 3 4)))
               (assert-equal #:name "vector-append"
                             #:expect #(1 2 3)
                             #:got (vector-append #(1) #(2 3)))
               (assert-equal #:name "vector-count"
                             #:expect 2
                             #:got (vector-count (lambda (i v)
                                                  (even? v))
                                                #(1 2 3 4))))

         (test "srfi-111 boxes"
               (assert-equal #:name "box and unbox"
                             #:expect 7
                             #:got (unbox (box 7)))
               (assert-equal #:name "set-box!"
                             #:expect 8
                             #:got (let ((b (box 7)))
                                     (set-box! b 8)
                                     (unbox b)))
               (assert-true (box? (box 1))
                            #:name "box?"))

         (test "srfi-19 date and time"
               (assert-true (date? (current-date))
                            #:name "current-date returns a date")
               (assert-true (string? (date->string (current-date)))
                            #:name "date->string returns a string")
               (assert-true (date? (make-date 0 0 0 0 1 1 2020 0))
                            #:name "make-date returns a date"))

         (test "match and pretty-print re-exports"
               (assert-equal #:name "match"
                             #:expect 3
                             #:got (match '(1 2) ((a b) (+ a b))))
               (assert-true (string? (with-output-to-string
                                       (lambda ()
                                         (pretty-print '(1 2)))))
                            #:name "pretty-print writes to a string"))

         (test "native Guile hash tables are left untouched"
               (assert-equal #:name "core make-hash-table/hash-ref still work"
                             #:expect 'found
                             #:got (let ((h (make-hash-table)))
                                     (hash-set! h 'k 'found)
                                     (hash-ref h 'k))))))

(define sql-builder-suite
  (suite "SQL statement builder"
         (test "INSERT binds every value"
               (let ((q (sql-insert 'users '((name . "O'Brien")
                                             (age . 30)))))
                 (assert-equal #:name "values become placeholders"
                               #:expect
                               "INSERT INTO \"users\" (\"name\", \"age\") VALUES (?, ?)"
                               #:got (sql-query-sql q))
                 (assert-equal #:name "parameters follow row order"
                               #:expect '("O'Brien" 30)
                               #:got (sql-query-params q))))

         (test "INSERT ... RETURNING"
               (assert-equal #:name "a column list is quoted"
                             #:expect
                             "INSERT INTO \"users\" (\"name\") VALUES (?) RETURNING \"id\""
                             #:got (sql-query-sql (sql-insert 'users
                                                              '((name . "x"))
                                                              #:returning '(id))))
               (assert-equal #:name "the star stays unquoted"
                             #:expect
                             "INSERT INTO \"users\" (\"name\") VALUES (?) RETURNING *"
                             #:got (sql-query-sql (sql-insert 'users
                                                              '((name . "x"))
                                                              #:returning '(*)))))

         (test "multi-row INSERT"
               (let ((q (sql-insert-many 'users
                                         (list '((name . "A") (age . 1))
                                               '((name . "B") (age . 2))))))
                 (assert-equal #:name "one VALUES group per row"
                               #:expect
                               "INSERT INTO \"users\" (\"name\", \"age\") VALUES (?, ?), (?, ?)"
                               #:got (sql-query-sql q))
                 (assert-equal #:name "parameters run row by row"
                               #:expect '("A" 1 "B" 2)
                               #:got (sql-query-params q)))
               (assert-equal #:name "numbered placeholders keep counting across rows"
                             #:expect
                             "INSERT INTO \"users\" (\"name\", \"age\") VALUES ($1, $2), ($3, $4)"
                             #:got (sql-query-sql
                                    (sql-insert-many
                                     'users
                                     (list '((name . "A") (age . 1))
                                           '((name . "B") (age . 2)))
                                     #:style 'numbered)))
               (assert-equal #:name "rows are put into the first row's column order"
                             #:expect '("A" 1 "B" 2)
                             #:got (sql-query-params
                                    (sql-insert-many
                                     'users
                                     (list '((name . "A") (age . 1))
                                           '((age . 2) (name . "B"))))))
               (assert-equal #:name "one row is a legal batch"
                             #:expect (sql-query-sql (sql-insert 'users
                                                                '((name . "x"))))
                             #:got (sql-query-sql (sql-insert-many
                                                   'users
                                                   (list '((name . "x")))))))

         (test "multi-row INSERT refuses rows that do not line up"
               (assert-equal #:name "a row with different columns"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-insert-many 't
                                                             (list '((a . 1))
                                                                   '((b . 2)))))
                                          (lambda args 'rejected)))
               (assert-equal #:name "a row carrying an extra column"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-insert-many
                                             't
                                             (list '((a . 1))
                                                   '((a . 2) (b . 3)))))
                                          (lambda args 'rejected)))
               (assert-equal #:name "an empty batch"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda () (sql-insert-many 't '()))
                                          (lambda args 'rejected))))

         (test "upsert: ON CONFLICT for PostgreSQL and SQLite"
               (let ((q (sql-upsert 'users '((id . 1) (name . "A") (age . 2))
                                    #:conflict-target 'id)))
                 (assert-equal #:name "the key, then every other column"
                               #:expect
                               "INSERT INTO \"users\" (\"id\", \"name\", \"age\") VALUES (?, ?, ?) ON CONFLICT (\"id\") DO UPDATE SET \"name\" = EXCLUDED.\"name\", \"age\" = EXCLUDED.\"age\""
                               #:got (sql-query-sql q))
                 (assert-equal #:name "the upsert binds no parameter of its own"
                               #:expect '(1 "A" 2)
                               #:got (sql-query-params q)))
               (assert-equal #:name "an explicit column list narrows the update"
                             #:expect
                             "INSERT INTO \"users\" (\"id\", \"name\", \"age\") VALUES (?, ?, ?) ON CONFLICT (\"id\") DO UPDATE SET \"name\" = EXCLUDED.\"name\""
                             #:got (sql-query-sql
                                    (sql-upsert 'users
                                                '((id . 1) (name . "A") (age . 2))
                                                #:conflict-target '(id)
                                                #:update '(name))))
               (assert-equal #:name "DO NOTHING keeps the conflict target"
                             #:expect
                             "INSERT INTO \"users\" (\"id\", \"name\") VALUES (?, ?) ON CONFLICT (\"id\") DO NOTHING"
                             #:got (sql-query-sql
                                    (sql-upsert 'users '((id . 1) (name . "A"))
                                                #:conflict-target 'id
                                                #:update 'nothing)))
               (assert-equal #:name "and may omit it"
                             #:expect
                             "INSERT INTO \"users\" (\"id\", \"name\") VALUES (?, ?) ON CONFLICT DO NOTHING"
                             #:got (sql-query-sql
                                    (sql-upsert 'users '((id . 1) (name . "A"))
                                                #:update 'nothing)))
               (assert-equal #:name "sqlite renders the same ON CONFLICT form"
                             #:expect
                             (sql-query-sql (sql-upsert 'users '((id . 1) (name . "A"))
                                                        #:conflict-target 'id
                                                        #:dialect 'postgres))
                             #:got (sql-query-sql (sql-upsert 'users '((id . 1) (name . "A"))
                                                             #:conflict-target 'id
                                                             #:dialect 'sqlite))))

         (test "upsert: ON DUPLICATE KEY UPDATE for MySQL"
               (assert-equal #:name "MySQL quotes its identifiers and sources VALUES()"
                             #:expect
                             "INSERT INTO `users` (`id`, `name`) VALUES (?, ?) ON DUPLICATE KEY UPDATE `id` = VALUES(`id`), `name` = VALUES(`name`)"
                             #:got (sql-query-sql (sql-upsert 'users
                                                             '((id . 1) (name . "A"))
                                                             #:dialect 'mysql)))
               (assert-equal #:name "an explicit column list keeps the key out of it"
                             #:expect
                             "INSERT INTO `users` (`id`, `name`, `age`) VALUES (?, ?, ?) ON DUPLICATE KEY UPDATE `name` = VALUES(`name`)"
                             #:got (sql-query-sql
                                    (sql-upsert 'users
                                                '((id . 1) (name . "A") (age . 2))
                                                #:dialect 'mariadb
                                                #:update '(name)))))

         (test "upsert: multi-row batches"
               (let ((q (sql-upsert-many
                         'users
                         (list '((id . 1) (name . "A"))
                               '((id . 2) (name . "B")))
                         #:conflict-target 'id
                         #:style 'numbered)))
                 (assert-equal #:name "one statement, numbered placeholders"
                               #:expect
                               "INSERT INTO \"users\" (\"id\", \"name\") VALUES ($1, $2), ($3, $4) ON CONFLICT (\"id\") DO UPDATE SET \"name\" = EXCLUDED.\"name\""
                               #:got (sql-query-sql q))
                 (assert-equal #:name "parameters run row by row"
                               #:expect '(1 "A" 2 "B")
                               #:got (sql-query-params q))))

         (test "upsert: misuse is reported"
               (assert-equal #:name "ON CONFLICT DO UPDATE needs a target"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1) (name . "A"))))
                                          (lambda args 'rejected)))
               (assert-equal #:name "MySQL has no conflict target to give"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1))
                                                        #:dialect 'mysql
                                                        #:conflict-target 'id))
                                          (lambda args 'rejected)))
               (assert-equal #:name "MySQL has no DO NOTHING"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1))
                                                        #:dialect 'mysql
                                                        #:update 'nothing))
                                          (lambda args 'rejected)))
               (assert-equal #:name "MySQL has no RETURNING"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1))
                                                        #:dialect 'mysql
                                                        #:returning '(id)))
                                          (lambda args 'rejected)))
               (assert-equal #:name "an unknown dialect"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1)) #:dialect 'oracle))
                                          (lambda args 'rejected)))
               (assert-equal #:name "nothing left to update"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1))
                                                        #:conflict-target 'id))
                                          (lambda args 'rejected)))
               (assert-equal #:name "a column that is not in the rows"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1) (name . "A"))
                                                        #:conflict-target 'id
                                                        #:update '(nope)))
                                          (lambda args 'rejected)))
               (assert-equal #:name "a conflict target that is not in the rows"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1) (name . "A"))
                                                        #:conflict-target 'nope))
                                          (lambda args 'rejected)))
               (assert-equal #:name "an update that is neither 'auto, 'nothing nor a list"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-upsert 't '((id . 1) (name . "A"))
                                                        #:conflict-target 'id
                                                        #:update 'sausage))
                                          (lambda args 'rejected))))

         (test "#:dialect decides how identifiers are delimited"
               (assert-equal #:name "MySQL uses backticks, not double quotes"
                             #:expect
                             "SELECT `id`, `name` FROM `users` WHERE `id` = ? LIMIT ?"
                             #:got (sql-query-sql (sql-select 'users
                                                             #:columns '(id name)
                                                             #:where '((id . 1))
                                                             #:limit 5
                                                             #:dialect 'mysql)))
               (assert-equal #:name "and so do the other builders"
                             #:expect
                             '("UPDATE `users` SET `name` = ? WHERE `id` = ?"
                               "DELETE FROM `users` WHERE `id` = ?")
                             #:got (list (sql-query-sql
                                          (sql-update 'users '((name . "A"))
                                                      #:where '((id . 1))
                                                      #:dialect 'mysql))
                                         (sql-query-sql
                                          (sql-delete 'users #:where '((id . 1))
                                                      #:dialect 'mysql))))
               (assert-equal #:name "the default is still ANSI quoting"
                             #:expect
                             "SELECT \"id\" FROM \"users\" WHERE \"id\" = ?"
                             #:got (sql-query-sql (sql-select 'users
                                                             #:columns '(id)
                                                             #:where '((id . 1)))))
               (assert-equal #:name "quote-sql-identifier switches delimiter on request"
                             #:expect '("\"id\"" "`id`")
                             #:got (list (quote-sql-identifier 'id)
                                         (quote-sql-identifier 'id #:dialect 'mysql)))
               (assert-equal #:name "an unknown dialect is rejected"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda () (sql-select 't #:dialect 'oracle))
                                          (lambda args 'rejected))))

         (test "placeholder dialects"
               (let ((row '((name . "x"))))
                 (assert-equal #:name "numbered (PostgreSQL)"
                               #:expect
                               "INSERT INTO \"users\" (\"name\") VALUES ($1)"
                               #:got (sql-query-sql (sql-insert 'users row
                                                                #:style 'numbered)))
                 (assert-equal #:name "dbi (guile-dbi)"
                               #:expect
                               "INSERT INTO \"users\" (\"name\") VALUES (#?)"
                               #:got (sql-query-sql (sql-insert 'users row
                                                                #:style 'dbi)))
                 (assert-equal #:name "percent (pyformat)"
                               #:expect
                               "INSERT INTO \"users\" (\"name\") VALUES (%s)"
                               #:got (sql-query-sql (sql-insert 'users row
                                                                #:style 'percent))))
               (assert-equal #:name "a procedure supplies a custom style"
                             #:expect "SELECT :p1"
                             #:got (sql-query-sql
                                    (sql-parameterize "SELECT ?" '(1)
                                                      #:style (lambda (i)
                                                                (format #f ":p~a" i))))))

         (test "UPDATE binds the SET values before the WHERE ones"
               (let ((q (sql-update 'users '((name . "A") (age . 9))
                                    #:where '((id . 7)))))
                 (assert-equal #:name "sql"
                               #:expect
                               "UPDATE \"users\" SET \"name\" = ?, \"age\" = ? WHERE \"id\" = ?"
                               #:got (sql-query-sql q))
                 (assert-equal #:name "parameters"
                               #:expect '("A" 9 7)
                               #:got (sql-query-params q)))
               (assert-equal #:name "no conditions means no WHERE clause"
                             #:expect "UPDATE \"users\" SET \"active\" = ?"
                             #:got (sql-query-sql (sql-update 'users
                                                              '((active . #t))))))

         (test "DELETE conditions"
               (let ((q (sql-delete 'users #:where '((id . 7)
                                                     (deleted-at is null)))))
                 (assert-equal #:name "sql"
                               #:expect
                               "DELETE FROM \"users\" WHERE \"id\" = ? AND \"deleted_at\" IS NULL"
                               #:got (sql-query-sql q))
                 (assert-equal #:name "a null test binds nothing"
                               #:expect '(7)
                               #:got (sql-query-params q))))

         (test "SELECT with every clause"
               (let ((q (sql-select 'users #:columns '(id name)
                                    #:where '((age > 18)
                                              (name in ("a" "b"))
                                              (email like "%@x")
                                              (id . 3))
                                    #:order-by '((name desc) (id))
                                    #:limit 10
                                    #:offset 20)))
                 (assert-equal #:name "sql"
                               #:expect
                               "SELECT \"id\", \"name\" FROM \"users\" WHERE \"age\" > ? AND \"name\" IN (?, ?) AND \"email\" LIKE ? AND \"id\" = ? ORDER BY \"name\" DESC, \"id\" LIMIT ? OFFSET ?"
                               #:got (sql-query-sql q))
                 (assert-equal #:name "limit and offset are bound, not spliced"
                               #:expect '(18 "a" "b" "%@x" 3 10 20)
                               #:got (sql-query-params q))
                 (assert-equal #:name "a lone OFFSET numbers from the first free slot"
                               #:expect "SELECT * FROM \"users\" OFFSET $1"
                               #:got (sql-query-sql (sql-select 'users
                                                                #:offset 20
                                                                #:style 'numbered)))))

         (test "IN and NOT IN"
               (assert-equal #:name "an empty IN never matches"
                             #:expect "SELECT * FROM \"users\" WHERE 1 = 0"
                             #:got (sql-query-sql (sql-select 'users
                                                              #:where '((id in ())))))
               (assert-equal #:name "an empty NOT IN always matches"
                             #:expect "SELECT * FROM \"users\" WHERE 1 = 1"
                             #:got (sql-query-sql (sql-select 'users
                                                              #:where '((id not-in ())))))
               (assert-equal #:name "a populated NOT IN binds each element"
                             #:expect '("SELECT * FROM \"users\" WHERE \"id\" NOT IN (?, ?, ?)"
                                       (1 2 3))
                             #:got (let ((q (sql-select 'users
                                                       #:where '((id not-in (1 2 3))))))
                                     (list (sql-query-sql q)
                                           (sql-query-params q)))))

         (test "identifier quoting is what keeps a value out of the SQL"
               (let ((q (sql-select 'users #:where '((x . "1; DROP TABLE users")))))
                 (assert-equal #:name "an injected value stays in the parameters"
                               #:expect '("1; DROP TABLE users")
                               #:got (sql-query-params q))
                 (assert-false (string-contains (sql-query-sql q) "DROP")
                               #:name "and never appears in the statement"))
               (assert-equal #:name "a quote in a column name is doubled, not closed"
                             #:expect
                             "SELECT * FROM \"t\" WHERE \"x\"\"; DROP TABLE t\" = ?"
                             #:got (sql-query-sql
                                    (sql-select 't
                                                #:where (list (cons "x\"; DROP TABLE t"
                                                                   1)))))
               (assert-equal #:name "#:quote-identifiers? #f emits bare names"
                             #:expect "SELECT id FROM users WHERE first_name = ?"
                             #:got (sql-query-sql
                                    (sql-select 'users #:columns '(id)
                                                #:where '((first-name . "J"))
                                                #:quote-identifiers? #f))))

         (test "WHERE fragments compose"
               (let ((q (sql-where-fragment '((a . 1) (b . 2)))))
                 (assert-equal #:name "the fragment carries its own conditions"
                               #:expect " WHERE \"a\" = ? AND \"b\" = ?"
                               #:got (sql-query-sql q))
                 (assert-equal #:name "with its parameters"
                               #:expect '(1 2)
                               #:got (sql-query-params q)))
               (assert-equal #:name "an empty condition list yields no clause"
                             #:expect ""
                             #:got (sql-query-sql (sql-where-fragment '())))
               (assert-equal #:name "#:start-index continues another fragment"
                             #:expect " WHERE \"a\" = $4"
                             #:got (sql-query-sql (sql-where-fragment
                                                   '((a . 1))
                                                   #:start-index 4
                                                   #:style 'numbered))))

         (test "sql-parameterize fills a hand-written template"
               (assert-equal #:name "question marks in order"
                             #:expect '("SELECT * FROM t WHERE a = ? AND b = ?"
                                       (1 2))
                             #:got (let ((q (sql-parameterize
                                            "SELECT * FROM t WHERE a = ? AND b = ?"
                                            (list 1 2))))
                                     (list (sql-query-sql q)
                                           (sql-query-params q))))
               (assert-equal #:name "renumbered for PostgreSQL"
                             #:expect "UPDATE t SET a = $1 WHERE b = $2"
                             #:got (sql-query-sql (sql-parameterize
                                                   "UPDATE t SET a = ? WHERE b = ?"
                                                   (list 1 2)
                                                   #:style 'numbered)))
               (assert-equal #:name "a template with no values is left alone"
                             #:expect "SELECT 1"
                             #:got (sql-query-sql (sql-parameterize "SELECT 1" '()))))

         (test "misuse is reported, not silently mis-compiled"
               (assert-equal #:name "too few values"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-parameterize "a = ? b = ?" (list 1)))
                                          (lambda args 'rejected)))
               (assert-equal #:name "too many values"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-parameterize "a = ?" (list 1 2)))
                                          (lambda args 'rejected)))
               (assert-equal #:name "an operator outside the allow-list"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-select 't #:where '((a drop-table 1))))
                                          (lambda args 'rejected)))
               (assert-equal #:name "an unknown placeholder style"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-placeholder 1 #:style 'nope))
                                          (lambda args 'rejected)))
               (assert-equal #:name "an INSERT with no columns"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-insert 't '()))
                                          (lambda args 'rejected)))
               (assert-equal #:name "a condition that is not a pair or list"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-select 't #:where '(bogus)))
                                          (lambda args 'rejected)))
               (assert-equal #:name "an unknown ORDER BY direction"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (sql-select 't
                                                        #:order-by '((a sideways))))
                                          (lambda args 'rejected))))

         (test "a query hands itself to the driver"
               (let ((q (sql-select 't #:where '((id . 1)))))
                 (assert-true (sql-query? q)
                              #:name "sql-query?")
                 (assert-equal #:name "sql-query->values returns the two values"
                               #:expect
                               '("SELECT * FROM \"t\" WHERE \"id\" = ?" (1))
                               #:got (receive (sql params)
                                         (sql-query->values q)
                                       (list sql params)))
                 (assert-equal #:name "sql-query->alist exposes the sql and the params"
                               #:expect
                               (list "SELECT * FROM \"t\" WHERE \"id\" = ?" '(1))
                               #:got (let ((a (sql-query->alist q)))
                                     (list (assoc-ref a 'sql)
                                           (assoc-ref a 'params))))))))

(define escaping-context-suite
  (suite "context-aware escaping"
         (test "html-escape-context picks an encoder per position"
               (assert-equal #:name "text"
                             #:expect "&lt;b&gt;"
                             #:got (html-escape-context "<b>" #:context 'text))
               (assert-equal #:name "quoted attribute"
                             #:expect "a&quot;b"
                             #:got (html-escape-context "a\"b"
                                                        #:context 'attribute))
               (assert-equal #:name "unquoted attribute"
                             #:expect "a&#x20;b"
                             #:got (html-escape-context "a b"
                                                        #:context 'unquoted-attribute))
               (assert-equal #:name "javascript string"
                             #:expect "a\\x22b"
                             #:got (html-escape-context "a\"b"
                                                        #:context 'js-string))
               (assert-equal #:name "css value"
                             #:expect "a\\000022 b"
                             #:got (html-escape-context "a\"b"
                                                        #:context 'css-value))
               (assert-equal #:name "url component"
                             #:expect "a%2Fb%3Fc%3Dd%26e"
                             #:got (html-escape-context "a/b?c=d&e"
                                                        #:context 'url))
               (assert-equal #:name "text is the default"
                             #:expect (html-escape "<b>")
                             #:got (html-escape-context "<b>"))
               (assert-equal #:name "an unknown context is an error"
                             #:expect 'rejected
                             #:got (catch #t
                                          (lambda ()
                                            (html-escape-context "x"
                                                                 #:context 'nope))
                                          (lambda args 'rejected))))

         (test "unquoted attribute values"
               (assert-equal #:name "whitespace and delimiters cannot end the value"
                             #:expect "a&#x20;b&#x3D;c&#x2F;d&#x3E;e"
                             #:got (html-escape-unquoted-attribute "a b=c/d>e"))
               (assert-equal #:name "letters, digits, - _ and . pass through"
                             #:expect "abc-1_2.3"
                             #:got (html-escape-unquoted-attribute "abc-1_2.3"))
               (assert-equal #:name "quotes are encoded rather than ended"
                             #:expect "&#x22;&#x27;"
                             #:got (html-escape-unquoted-attribute "\"'"))
               (assert-equal #:name "a non-BMP character stays one reference"
                             #:expect "&#x1F600;"
                             #:got (html-escape-unquoted-attribute
                                    (string (integer->char #x1F600)))))

         (test "JavaScript string literals"
               (let ((encoded (html-escape-js-string
                               "</script><script>alert(1)</script>")))
                 (assert-false (string-contains encoded "<")
                               #:name "no angle bracket survives")
                 (assert-false (string-contains encoded "/")
                               #:name
                               "no slash survives, so </script is impossible"))
               (assert-equal #:name "quotes and spaces become escapes"
                             #:expect "O\\x27Reilly\\x20\\x22x\\x22"
                             #:got (html-escape-js-string "O'Reilly \"x\""))
               (assert-equal #:name "a backslash is escaped, not left to combine"
                             #:expect "a\\x5Cb"
                             #:got (html-escape-js-string "a\\b"))
               (assert-equal #:name "a newline cannot break the literal"
                             #:expect "a\\x0Ab"
                             #:got (html-escape-js-string "a
b"))
               (assert-equal #:name "BMP characters use \\xHH"
                             #:expect "caf\\xE9"
                             #:got (html-escape-js-string (string #\c #\a #\f
                                                                  (integer->char 233))))
               (assert-equal #:name "the line separator U+2028 is escaped"
                             #:expect "a\\u2028b"
                             #:got (html-escape-js-string
                                    (string-append "a"
                                                   (string (integer->char #x2028))
                                                   "b")))
               (assert-equal #:name "a non-BMP character becomes a surrogate pair"
                             #:expect "\\uD83D\\uDE00"
                             #:got (html-escape-js-string
                                    (string (integer->char #x1F600))))
               (assert-equal #:name "alphanumerics stay readable"
                             #:expect "abcXYZ019"
                             #:got (html-escape-js-string "abcXYZ019"))
               (assert-equal #:name
                             "an encoded value needs no further HTML escaping"
                             #:expect (html-escape-js-string "it's </script>")
                             #:got (html-escape
                                    (html-escape-js-string "it's </script>"))))

         (test "CSS values"
               (let ((encoded (html-escape-css-value
                               "red; } body { background: url(x)")))
                 (assert-false (string-contains encoded ";")
                               #:name "no semicolon survives")
                 (assert-false (string-contains encoded "}")
                               #:name "no closing brace survives")
                 (assert-false (string-contains encoded "{")
                               #:name "no opening brace survives")
                 (assert-false (string-contains encoded "(")
                               #:name "no function call survives"))
               (assert-equal #:name "a percent sign is escaped"
                             #:expect "100\\000025 "
                             #:got (html-escape-css-value "100%"))
               (assert-equal #:name "alphanumerics stay readable"
                             #:expect "TimesNewRoman10px"
                             #:got (html-escape-css-value "TimesNewRoman10px"))
               (assert-equal #:name "a non-BMP character uses a six-digit escape"
                             #:expect "\\01F600 "
                             #:got (html-escape-css-value
                                    (string (integer->char #x1F600)))))

         (test "every encoder leaves the empty string alone"
               (assert-equal #:name "empty input, empty output"
                             #:expect '("" "" "" "")
                             #:got (list (html-escape-context "" #:context 'text)
                                         (html-escape-unquoted-attribute "")
                                         (html-escape-js-string "")
                                         (html-escape-css-value ""))))))

(define (spec)
  (veritas-run prelude-suite another-suite robustness-suite reexports-suite
               sql-builder-suite escaping-context-suite))
