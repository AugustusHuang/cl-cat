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

;;; NOTE: Once you've defined the class's underlying category, if you make
;;; another function focused on relationship between objects, it won't be
;;; made automatically global, so if then you define an object in the same
;;; category and class but don't have such kind of relationship with others,
;;; it's no fault. The only way to update the category meta-information is
;;; to re-define the category of this class and make the relationship exact.
;;; FIXME: Maybe we can do better?

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct (class-category
	       (:print-function
		(lambda (cc s k)
		  (declare (ignore k))
		  (format s "<Type ~A of category ~A>"
			  (class-category-class cc)
			  (class-category-category cc)))))
    (class t)
    ;; CATEGORY will be a subclass of NONE-CATEGORY.
    (category 'none-category)))

;;; DEFCATEGORY macro, will define the corresponding class of category and
;;; if given related function, initialize with given function, or just use
;;; slot-wise lexicographic default function.
(defmacro defcategory (class category &optional args)
  ()
  ;; In CLTL it's mentioned that the macro-expander should not perform
  ;; the side-effects, so definition of the category-object will be here.
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     ))

(defun make-none-category ()
  (make-instance 'none-category))

(defun make-=-category (=-func)
  (make-instance '=-category :=-function =-func))

(defun make->-category (>-func)
  (make-instance '>-category :>-function >-func))

(defun make-<-category (<-func)
  (make-instance '<-category :<-function <-func))

(defun make-+-category (+-func n-inf p-inf)
  (make-instance '+-category
		 :+-function +-func
		 :n-infinitum n-inf
		 :p-infinitum p-inf))

(defun make-succ-category (succ-func inf)
  (make-instance 'succ-category
		 :succ succ-func
		 :infinitum inf))

(defun make-pred-category (pred-func inf)
  (make-instance 'pred-category
		 :pred pred-func
		 :infinitum inf))

(defun make-functor-category (dom codom)
  (make-instance 'functor-category
		 :domain dom
		 :codomain codom))

(defun make-=-functor-category (=-func dom codom)
  (make-instance '=-functor-category
		 :=-function =-func
		 :domain dom
		 :codomain codom))

(defun make->-functor-category (>-func dom codom)
  (make-instance '>-functor-category
		 :>-function >-func
		 :domain dom
		 :codomain codom))

(defun make-<-functor-category (<-func dom codom)
  (make-instance '<-functor-category
		 :<-function <-func
		 :domain dom
		 :codomain codom))

(defun make-+-functor-category (+-func dom codom)
  (make-instance '+-functor-category
		 :+-function +-func
		 :domain dom
		 :codomain codom))

(defun make-succ-functor-category (succ-func dom codom)
  (make-instance 'succ-functor-category
		 :succ succ-func
		 :domain dom
		 :codomain codom))

(defun make-pred-functor-category (pred-func dom codom)
  (make-instance 'pred-functor-category
		 :pred pred-func
		 :domain dom
		 :codomain codom))

;;; Those pre-defined useful categories should be loaded to form the basis
;;; of user designs, of course user can define their own without the basis.
;;; Also, I will make a compactified version of standard type system.
