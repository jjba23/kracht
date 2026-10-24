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

(define-module (maak)
  #:declarative? #t
  #:use-module (maak maak)
  ;; The container helpers are imported under a prefix because the tasks below
  ;; are named the way maak exposes them on the command line -- `maak
  ;; container-build' -- and a task may not shadow the helper it calls.
  #:use-module ((maak container) #:prefix ct:))

;; ====== Maak file constants ======

;; The URL for the CSS style-sheet used to style API documentation.
(define api-docs-css-url
  "https://jointhefreeworld.org/static/css/jointhefreeworld.css")

;; The absolute path to the directory where the API documentation will be deployed.
(define deploy-api-docs-dir
  "/srv/http/jointhefreeworld.org")

;; The name of the project, represented as a symbol.
(define project-name
  'kracht)

(define hot-reload-file-types '(scm))

;; ====== Maak file tasks ======

(define (fmt)
  "Format Scheme source code files according to the Guix style guide."
  (run (list "find" "." "-maxdepth" "8" "-name" "*.scm" "-type" "f"
             "-exec" "guix" "style" "-f" "{}" ";")
       #:verbose? #t))

(define (repl)
  "Development REPL."
  (manifest-shell (list "guile" "-L" "./src"
                        "-c" "'((@ (ares server) run-nrepl-server))'")))

(define (docs)
  "Generate technical API documentation for Kracht."
  (delete-file-recursively "doc")
  ;; `documenta' writes the Texinfo tree and `texi2any' renders it: two calls,
  ;; so a failure names the step that failed instead of an `&&' chain.
  (manifest-shell (list "documenta" "api" (~ "./src/~a" project-name)))
  (manifest-shell (list "texi2any" "-v" "--html"
                        (~ "--css-ref=~a" api-docs-css-url)
                        "./doc/api/index.texi"))
  (mv "./index" "./doc/api-dist"))

(define (deploy)
  "Generate documentation for the project and publish to web directory."
  (docs)
  (remkdir-p (~ "~a/api-docs/~a" deploy-api-docs-dir project-name))
  (mv "doc/api-dist/*"
      (~ "~a/api-docs/~a" deploy-api-docs-dir project-name)
      #:expand? #t)
  (delete-file-recursively "doc"))

(define (test)
  "Run project's tests."
  (manifest-shell (list "guile" "-L" "./src" "-L" "./test"
                        "-c" "'((@ (veritas runner) run-tests))'")))

(define (hot-reload)
  "Hot-reloading developer's setup."
  (manifest-shell (list "watchexec" "'maak test'"
                        "--clear"
                        "--exts" (string-join
                                  (map symbol->string hot-reload-file-types)
                                  ",")
                        "--on-busy-update=restart"
                        "--shell" "bash"
                        "--watch" ".")))

(define (hr)
  "Alias to hot-reloading."
  (hot-reload))

(define (dev)
  "Developer's setup."
  (hot-reload))

(define (default)
  "Default task to be run, list available tasks."
  (list-tasks))

;; ====== Container tasks ======

(define (container-build)
  "Build the container image for kracht."
  (ct:container-build "." #:tag "kracht" #:verbose? #t))

(define (container-run)
  "Run the container image for kracht."
  (ct:container-run "kracht:latest"
                    #:interactive? #t
                    #:tty? #t))

(define (package-docker)
  "Build the container image for kracht."
  (container-build))
