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

(define-module (kracht lists)
  #:use-module (srfi srfi-1)
  #:export (flatten-list assoc-lens
                         assoc-ref-or
                         assoc-ref-or-empty-str
                         alist-set
                         alist?
                         evaluate-thunks
                         shuffle-fy))

;;;
;;; list helpers
;;;

(define (flatten-list xs)
  "Recursively flatten a nested list structure XS into a flat, one-dimensional list."
  (cond
    ((null? xs)
     '())
    ((pair? xs)
     (append (flatten-list (car xs))
             (flatten-list (cdr xs))))
    (else (list xs))))

(define (alist? obj)
  "Return #t if OBJ is a valid association list (a list where every
element is a pair), otherwise return #f."
  (cond
    ((null? obj)
     #t)
    ((not (pair? obj))
     #f)
    ((not (pair? (car obj)))
     #f)
    (else (alist? (cdr obj)))))

(define-syntax-rule (evaluate-thunks thunks)
  "Given a list of functions that don't take any inputs, run each one of
those functions and return a list of what each function returned."
  (map (lambda (t)
         (apply t
                '())) thunks))

(define* (shuffle-fy xs
                     #:key state)
  "Returns a randomly shuffled copy of XS.
List shuffle which implements the modern Fisher-Yates shuffle algorithm.
It converts the list to a vector for efficient O(1) random access
and mutation, shuffles the vector in-place, and then converts it back to
a list.  Pass STATE (a random state, as returned by
`random-state-from-platform`) to shuffle deterministically; otherwise the
current global random state is used and never reseeded as a side effect."
  (let ((xs (list-copy xs)))
    (if (or (null? xs)
            (null? (cdr xs))) xs
        (let* ((vec (list->vector xs))
               (len (vector-length vec)))
          ;; Loop from the last element down to the second element.
          (do ((i (- len 1)
                  (- i 1)))
              ((< i 1))
              ;; Pick a random index j from 0 to i (inclusive).
              (let* ((j (if state
                            (random (1+ i) state)
                            (random (1+ i))))
                     ;; Swap the elements at positions i and j.
                     (temp (vector-ref vec i)))
                (vector-set! vec i
                             (vector-ref vec j))
                (vector-set! vec j temp)))
          ;; Convert the shuffled vector back to a list.
          (vector->list vec)))))

;;;
;;; alist helpers
;;;

(define (assoc-lens xs keys)
  "Deeply look up a value within nested association lists XS by traversing
the sequence of KEYS sequentially.  Traversal stops and returns #f as soon
as an intermediate value is not an association list."
  (fold (lambda (k xxs)
          (if (pair? xxs)
              (assoc-ref xxs k) #f)) xs keys))

(define* (assoc-ref-or alist field
                       #:key (default #f))
  "Look up FIELD in ALIST.  Return the associated value if found, otherwise
return DEFAULT (which defaults to #f)."
  (let ((sut (assoc-ref alist field)))
    (if sut sut default)))

(define (assoc-ref-or-empty-str alist field)
  "Look up FIELD in ALIST. Return the associated value if found,
or an empty string \"\" if the field does not exist or matches #f."
  (assoc-ref-or alist field
                #:default ""))

(define (alist-set alist key value)
  "Return a new association list based on ALIST with KEY mapped to VALUE.
If KEY already exists its entry is replaced in place, otherwise a new entry
is consed onto the front.  ALIST itself is never mutated."
  (let ((pair (assoc key alist)))
    (if pair
        (map (lambda (p)
               (if (eq? p pair)
                   (cons key value) p)) alist)
        (cons (cons key value) alist))))
