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

(define-module (kracht prelude-test)
  #:use-module ((srfi srfi-64)
                #:hide (define-test))
  #:use-module (rnrs bytevectors)
  #:use-module (ice-9 iconv)
  #:use-module (kracht test-utils)
  #:use-module (kracht prelude))

(define-test test-prelude
             (test-group "alist-from-url-encoded-body"
                         (test-equal "empty body"
                                     '()
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "")))

                         (test-equal "single key-value pair"
                                     '(("name" . "John Doe"))
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "name=John%20Doe")))

                         (test-equal "multiple key-value pairs"
                                     '(("name" . "Jane Doe") ("age" . "30"))
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "name=Jane%20Doe&age=30")))

                         (test-equal "key with no value"
                                     '(("key" . ""))
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "key=")))

                         (test-equal
                          "value with no key (unlikely but possible from split)"
                          '(("" . "value"))
                          (alist-from-url-encoded-body (string->utf8 "=value")))

                         (test-equal
                          "keys and values with special characters (URL-encoded)"
                          '(("param_1" . "value with spaces")
                            ("param/2" . "another_value!"))
                          (alist-from-url-encoded-body (string->utf8
                                                        "param_1=value%20with%20spaces&param%2F2=another_value%21")))

                         (test-equal "duplicate keys"
                                     '(("item" . "apple") ("item" . "orange"))
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "item=apple&item=orange")))

                         (test-equal "empty key and empty value"
                                     '()
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "=")))

                         (test-equal "single key, no equals sign"
                                     '(("key-only" . "")) ;This is how string-split on '=' will behave
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "key-only")))

                         (test-equal "multiple keys, no equals sign"
                                     '(("key1" . "")
                                       ("key2" . ""))
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "key1&key2")))

                         (test-equal "mixed valid and malformed pairs"
                                     '(("a" . "1") ("b" . "")
                                       ("c" . "3"))
                                     (alist-from-url-encoded-body (string->utf8
                                                                   "a=1&b&c=3"))))

             (test-group "non-empty-string? tests"
                         (test-equal "non-empty-string? with non-empty string"
                          #t
                          (non-empty-string? "hello"))
                         (test-equal "non-empty-string? with empty string" #f
                                     (non-empty-string? ""))
                         (test-equal "non-empty-string? with whitespace" #t
                                     (non-empty-string? " "))
                         (test-equal "non-empty-string? with #f" #f
                                     (non-empty-string? #f))
                         (test-equal "non-empty-string? with #t" #t
                                     (non-empty-string? #t)))

             (test-group "empty-string? tests"
                         (test-equal "empty-string? with non-empty string" #f
                                     (empty-string? "hello"))
                         (test-equal "empty-string? with empty string" #t
                                     (empty-string? ""))
                         (test-equal "empty-string? with whitespace" #f
                                     (empty-string? " ")))

             (test-group "string-drop tests"
                         (test-equal "string-drop-first-last normal case"
                                     "ell"
                                     (string-drop-first-last "hello"))
                         (test-equal "string-drop-first-last single char" "a"
                                     (string-drop-first-last "a"))
                         (test-equal "string-drop-first-last two chars" "ab"
                                     (string-drop-first-last "ab"))
                         (test-equal "string-drop-first-last empty string" ""
                                     (string-drop-first-last ""))

                         (test-equal "string-drop-first-last-line normal case"
                          "middle line"
                          (string-drop-first-last-line
                                                       "first line\nmiddle line\nlast line"))
                         (test-equal "string-drop-first-last-line single line"
                          ""
                          (string-drop-first-last-line "single line"))
                         (test-equal "string-drop-first-last-line two lines"
                                     ""
                                     (string-drop-first-last-line
                                      "first line\nlast line"))
                         (test-equal "string-drop-first-last-line empty input"
                          ""
                          (string-drop-first-last-line "")))

             (test-group "assoc-ref-or-empty-str tests"
                         (test-equal "empty alist"
                                     (assoc-ref-or-empty-str '()
                                                             'key) "")

                         (test-equal "key not found"
                                     (assoc-ref-or-empty-str '((a . 1) (b . 2))
                                                             'c) "")

                         (test-equal "key found - string value"
                                     (assoc-ref-or-empty-str '((a . 1)
                                                               (b . "hello"))
                                                             'b) "hello")

                         (test-equal "key found - non-string value"
                                     (assoc-ref-or-empty-str '((a . 1) (b . 2))
                                                             'b) 2)

                         (test-equal "key is first element"
                                     (assoc-ref-or-empty-str '((key . "value")
                                                               (b . 2))
                                                             'key) "value")

                         (test-equal "key is last element"
                                     (assoc-ref-or-empty-str '((a . 1) (b . 2)
                                                               (key . "value"))
                                                             'key) "value"))

             (test-group "log tests"
                         (test-assert "log-message behaves as expected"
                                      (log-message (format #f
                                                    "1 + 1 = ~a and 2 + 2 = ~a"
                                                    (+ 1 1)
                                                    (+ 2 2))))

                         (test-assert "log behaves as expected"
                                      (log-info "3 + 3 = ~a and 4 + 4 = ~a"
                                                (+ 3 3)
                                                (+ 4 4)))

                         (test-assert "log-info behaves as expected"
                                      (log-info "5 + 5 = ~a and 6 + 6 = ~a"
                                                (+ 5 5)
                                                (+ 6 6)))

                         (test-assert "log-error behaves as expected"
                                      (log-error "fake message: ~a"
                                                 "something totally expected")))

             (test-group "sha"
                         (test-assert "get-short-commit-sha works as expected"
                          (write (get-short-commit-sha))))

             (test-group "html-escape tests"
                         (test-equal "Empty string" ""
                                     (html-escape ""))
                         (test-equal "String with no special characters"
                                     "Hello, World!"
                                     (html-escape "Hello, World!"))
                         (test-equal "Single quote" "&apos;"
                                     (html-escape "'"))
                         (test-equal "Double quote" "&quot;"
                                     (html-escape "\""))
                         (test-equal "Mix of quotes"
                                     "He&apos;s a &quot;guy&quot;."
                                     (html-escape "He's a \"guy\"."))
                         (test-equal "Accented a" "&aacute;"
                                     (html-escape "á"))
                         (test-equal "Accented A" "&Aacute;"
                                     (html-escape "Á"))
                         (test-equal "Accented e" "&eacute;"
                                     (html-escape "é"))
                         (test-equal "Accented E" "&Eacute;"
                                     (html-escape "É"))
                         (test-equal "Accented i" "&iacute;"
                                     (html-escape "í"))
                         (test-equal "Accented I" "&Iacute;"
                                     (html-escape "Í"))
                         (test-equal "Accented o" "&oacute;"
                                     (html-escape "ó"))
                         (test-equal "Accented O" "&Oacute;"
                                     (html-escape "Ó"))
                         (test-equal "Accented u" "&uacute;"
                                     (html-escape "ú"))
                         (test-equal "Accented U" "&Uacute;"
                                     (html-escape "Ú"))
                         (test-equal "C-cedilla" "&ccedil;"
                                     (html-escape "ç"))
                         (test-equal "N-tilde" "&ntilde;"
                                     (html-escape "ñ"))
                         (test-equal "Multiple accents"
                                     "&Aacute;rvore &eacute; bonita"
                                     (html-escape "Árvore é bonita"))
                         (test-equal "String with multiple occurrences"
                          "He said, &quot;It&apos;s a beautiful day!&quot;"
                          (html-escape "He said, \"It's a beautiful day!\""))
                         (test-equal "Example sentence escaped"
                          "&Oacute;l&aacute;, m&uacute;ndo! Isto &eacute; um teste."
                          (html-escape "Ólá, múndo! Isto é um teste.")))

             (test-group "html-unescape tests"
                         (test-equal "Empty string" ""
                                     (html-unescape ""))
                         (test-equal "String with no entities" "Hello, World!"
                                     (html-unescape "Hello, World!"))
                         (test-equal "Single apos entity" "'"
                                     (html-unescape "&apos;"))
                         (test-equal "Single quot entity" "\""
                                     (html-unescape "&quot;"))
                         (test-equal "Mix of entities" "He's a \"guy\"."
                                     (html-unescape
                                      "He&apos;s a &quot;guy&quot;."))
                         (test-equal "Accented a entity" "á"
                                     (html-unescape "&aacute;"))
                         (test-equal "Accented A entity" "Á"
                                     (html-unescape "&Aacute;"))
                         (test-equal "C-cedilla entity" "ç"
                                     (html-unescape "&ccedil;"))
                         (test-equal "Multiple entity occurrences"
                                     "It's a \"quote\"."
                                     (html-unescape
                                      "It&apos;s a &quot;quote&quot;."))
                         (test-equal "Example sentence unescaped"
                                     "Ólá, múndo! Isto é um teste."
                                     (html-unescape
                                      "&Oacute;l&aacute;, m&uacute;ndo! Isto &eacute; um teste.")))

             (test-group "Round-trip tests"
                         (test-equal "Escape then unescape"
                                     "Hello, it's a \"test\" with áéíóú."
                                     (html-unescape (html-escape
                                                     "Hello, it's a \"test\" with áéíóú.")))
                         (test-equal "Example sentence"
                                     "Olá, múndo! Isto é um teste."
                                     (html-unescape (html-escape
                                                     "Olá, múndo! Isto é um teste.")))
                         (test-equal
                          "Unescape then escape (assuming valid entities)"
                          "&lt;div&gt;&apos;test&apos;&lt;&sol;div&gt;"
                          (html-escape (html-unescape
                                        "&lt;div&gt;&apos;test&apos;&lt;&sol;div&gt;")))
                         (test-equal
                          "Unescape then escape (for chars in alist)"
                          "&apos;&aacute;&eacute;&iacute;&oacute;&uacute;&quot;"
                          (html-escape (html-unescape
                                        "&apos;&aacute;&eacute;&iacute;&oacute;&uacute;&quot;")))))

