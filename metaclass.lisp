;;;; The MIT License (MIT)

;;;; Copyright (c) 2015 Huang Xuxing

;;;; Permission is hereby granted, free of charge, to any person obtaining
;;;; a copy of this software and associated documentation files
;;;; (the "Software"), to deal in the Software without restriction,
;;;; including without limitation the rights to use, copy, modify, merge,
;;;; publish, distribute, sublicense, and/or sell copies of the Software,
;;;; and to permit persons to whom the Software is furnished to do so,
;;;; subject to the following conditions:

;;;; The above copyright notice and this permission notice shall be included
;;;; in all copies or substantial portions of the Software.

;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;;;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;;;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
;;;; THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;;;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE,
;;;; ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
;;;; OTHER DEALINGS IN THE SOFTWARE.

(in-package :cl-cat)

;;; Make different levels of categories canonical!
;;; Use DEFCATEGORY, DEFOBJECT and DEFARROW to handle all levels of them.
;;; So we only need one STANDARD-CATEGORY-CLASS as the metaclass of
;;; all classes we want to define with a categorical structure...
;;; NOTE: In SBCL metaclass accessors will return a list, use them carefully!

(deftype category-type ()
  '(member
    :none
    ;; PARTIAL-ORDER can have equivalent classes of objects,
    ;; e.g. there can be two groups of objects act like a bipartite graph.
    :partial-order
    ;; TOTAL-ORDER can have equivalent classes of objects.
    :total-order
    ;; WELL-ORDER won't have equivalent classes, since any subset of objects
    ;; will have a least candidate.
    :well-order
    ;; PRODUCT is the monoid in category \mathbf{Set}. Since all structures
    ;; we build can be regarded as sets, it's valid.
    :product
    ;; MONAD is a monoid of endofunctors in categories, it represent the
    ;; multiple functor application.
    :monad))

(defclass standard-category-class (standard-class)
  ((type :initarg :type :accessor category-type :type category-type)
   ;; Slots to be defined.
   )
  (:documentation
   "Category metaclass, defines the underlying inner structure of objects."))

(let ((cc (find-class 'standard-category-class)))
  (defun category-class-p (class)
    (subtypep (class-of class) cc)))

