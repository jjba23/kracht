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
  #:use-module (kracht test-utils)
  #:use-module (kracht prelude))

(define-test test-prelude
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
                          (write (get-short-commit-sha)))))
