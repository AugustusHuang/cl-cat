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
		  (format s "<Class ~A of category ~A>"
			  (class-category-class cc)
			  (class-category-category cc)))))
    ;; CLASS will be a class-object.
    (class (find-class t))
    ;; CATEGORY will be an object of subclass of NONE-CATEGORY.
    (category (make-instance 'none-category))))

(declare (inline cc-class))
(defun cc-class (cc)
  (class-category-class cc))

(declare (inline cc-category))
(defun cc-category (cc)
  (class-category-category cc))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for i in names collect `(,i (gensym)))
     ,@body))

;;; DEFCATEGORY macro, will define the corresponding class of category and
;;; if given related function, initialize with given function, or just use
;;; slot-wise lexicographic default function.
(defmacro defcategory (class category &rest args)
  (with-gensyms (class-object category-instance)
    `(let ((,class-object (find-class ,class))
	   (,category-instance nil))
       (case ,category
	 ('none-category (setf ,category-instance
			       (make-none-category)))
	 ('=-category (setf ,category-instance
			    (make-=-category (first ,args))))
	 ('>-category (setf ,category-instance
			    (make->-category (first ,args))))
	 ('<-category (setf ,category-instance
			    (make-<-category (first ,args))))
	 ('order-category (setf ,category-instance
				(make-order-category (first ,args)
						     (second ,args)
						     (third ,args))))
	 ('+-category (setf ,category-instance
			    (make-+-category (first ,args)
					     (second ,args))))
	 ('finite-+-category (setf ,category-instance
				   (make-finite-+-category (first ,args)
							   (second ,args)
							   (third ,args)
							   (fourth ,args))))
	 ('mod-+-category (setf ,category-instance
				(make-mod-+-category (first ,args)
						     (second ,args)
						     (third ,args))))
	 ('succ-category (setf ,category-instance
			       (make-succ-category (first ,args)
						   (second ,args)
						   (third ,args)
						   (fourth ,args)
						   (fifth ,args))))
	 ('pred-category (setf ,category-instance
			       (make-pred-category (first ,args)
						   (second ,args)
						   (third ,args)
						   (fourth ,args)
						   (fifth ,args))))
	 ('functor-category (setf ,category-instance
				  (make-functor-category (first ,args)
							 (second ,args))))
	 ('=-functor-category (setf ,category-instance
				    (make-=-functor-category (first ,args)
							     (second ,args)
							     (third ,args))))
	 ('>-functor-category (setf ,category-instance
				    (make->-functor-category (first ,args)
							     (second ,args)
							     (third ,args))))
	 ('<-functor-category (setf ,category-instance
				    (make-<-functor-category (first ,args)
							     (second ,args)
							     (third ,args))))
	 ('order-functor-category (setf ,category-instance
					(make-order-functor-category
					 (first ,args)
					 (second ,args)
					 (third ,args)
					 (fourth ,args)
					 (fifth ,args))))
	 ('+-functor-category (setf ,category-instance
				    (make-+-functor-category (first ,args)
							     (second ,args))))
	 ('finite-+-functor-category (setf ,category-instance
					   (make-finite-+-functor-category
					    (first ,args)
					    (second ,args)
					    (third ,args)
					    (fourth ,args)
					    (fifth ,args)
					    (sixth ,args))))
	 ('mod-+-functor-category (setf ,category-instance
					(make-mod-+-functor-category
					 (first ,args)
					 (second ,args)
					 (third ,args)
					 (fourth ,args)
					 (fifth ,args))))
	 ('succ-functor-category (setf ,category-instance
				       (make-succ-functor-category
					(first ,args)
					(second ,args)
					(third ,args)
					(fourth ,args)
					(fifth ,args)
					(sixth ,args))))
	 ('pred-functor-category (setf ,category-instance
				       (make-pred-functor-category
					(first ,args)
					(second ,args)
					(third ,args)
					(fourth ,args)
					(fifth ,args)
					(sixth ,args))))
	 (otherwise (setf ,category-instance
			  (make-instance ,category args))))
       ;; In CLTL it's mentioned that the macro-expander should not perform
       ;; the side-effects.
       `(eval-when (:compile-toplevel :load-toplevel :execute)
	  (make-class-category :class ,class-object
			       :category ,category-instance)))))

(defun make-none-category ()
  (make-instance 'none-category))

(defun make-=-category (=-func)
  (make-instance '=-category :=-function =-func))

(defun make->-category (>-func)
  (make-instance '>-category :>-function >-func))

(defun make-<-category (<-func)
  (make-instance '<-category :<-function <-func))

(defun make-order-category (=-func >-func <-func)
  (make-instance 'order-category
		 :=-function =-func
		 :>-function >-func
		 :<-function <-func))

(defun make-+-category (+-func id)
  (make-instance '+-category
		 :+-function +-func
		 :identity id))

(defun make-finite-+-category (+-func id n-inf p-inf)
  (make-instance 'finite-+-category
		 :+-function +-func
		 :identity id
		 :negative-infinitum n-inf
		 :positive-infinitum p-inf))

(defun make-mod-+-category (+-func id mod)
  (make-instance 'mod-+-category
		 :+-function +-func
		 :identity id
		 :modulus mod))

(defun make-succ-category (=-func >-func <-func succ-func inf)
  (make-instance 'succ-category
		 :=-function =-func
		 :>-function >-func
		 :<-function <-func
		 :succ succ-func
		 :infinitum inf))

(defun make-pred-category (=-func >-func <-func pred-func inf)
  (make-instance 'pred-category
		 :=-function =-func
		 :>-function >-func
		 :<-function <-func
		 :pred pred-func
		 :infinitum inf))

(defun make-functor-category (dom codom)
  (make-instance 'functor-category
		 :domain dom
		 :codomain codom))

(defun make-=-functor-category (dom codom =-func)
  (make-instance '=-functor-category
		 :domain dom
		 :codomain codom
		 :=-function =-func))

(defun make->-functor-category (dom codom >-func)
  (make-instance '>-functor-category
		 :domain dom
		 :codomain codom
		 :>-function >-func))

(defun make-<-functor-category (dom codom <-func)
  (make-instance '<-functor-category
		 :domain dom
		 :codomain codom
		 :<-function <-func))

(defun make-order-functor-category (dom codom =-func >-func <-func)
  (make-instance 'order-functor-category
		 :domain dom
		 :codomain codom
		 :=-function =-func
		 :>-function >-func
		 :<-function <-func))

(defun make-+-functor-category (dom codom +-func id)
  (make-instance '+-functor-category
		 :domain dom
		 :codomain codom
		 :+-function +-func
		 :identity id))

(defun make-finite-+-functor-category (dom codom +-func id n-inf p-inf)
  (make-instance 'finite-+-functor-category
		 :domain dom
		 :codomain codom
		 :+-function +-func
		 :identity id
		 :negative-infinitum n-inf
		 :positive-infinitum p-inf))

(defun make-mod-+-functor-category (dom codom +-func id mod)
  (make-instance 'mod-+-functor-category
		 :domain dom
		 :codomain codom
		 :+-function +-func
		 :identity id
		 :modulus mod))

(defun make-succ-functor-category (dom codom succ-func =-func >-func <-func)
  (make-instance 'succ-functor-category
		 :domain dom
		 :codomain codom
		 :succ succ-func
		 :=-function =-func
		 :>-function >-func
		 :<-function <-func))

(defun make-pred-functor-category (dom codom pred-func =-func >-func <-func)
  (make-instance 'pred-functor-category
		 :domain dom
		 :codomain codom
		 :pred pred-func
		 :=-function =-func
		 :>-function >-func
		 :<-function <-func))

