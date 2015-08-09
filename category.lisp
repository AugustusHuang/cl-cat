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

;;; Different built-in types.
(deftype built-in-category ()
  '(member
    :none-category
    :=-category
    :>-category
    :<-category
    :+-category
    :finite-+-category
    :mod-+-category
    :order-category
    :succ-category
    :pred-category
    :functor-category
    :=-functor-category
    :>-functor-category
    :<-functor-category
    :+-functor-category
    :finite-+-functor-category
    :mod-+-functor-category
    :order-functor-category
    :succ-functor-category
    :pred-functor-category))

(defmacro with-gensyms ((&rest names) &body body)
  `(let ,(loop for i in names collect `(,i (gensym)))
     ,@body))

;;; DEFCATEGORY macro, will define the corresponding class of category and
;;; if given related function, initialize with given function.
;;; CLASS-NAME and CATEGORY-NAME are symbols.
(defmacro defcategory (class-symbol category-symbol)
  (flet ((category-name (class-symbol category-symbol)
	   (let* ((prefix (symbol-name class-symbol))
		  (suffix (ecase category-symbol
			    (:none-category "-NONE")
			    (:=-category "-=")
			    (:>-category "->")
			    (:<-category "-<")
			    (:+-category "-+")
			    (:finite-+-category "-FINITE-+")
			    (:mod-+-category "-MOD-+")
			    (:order-category "-ORDER")
			    (:succ-category "-SUCC")
			    (:pred-category "-PRED")
			    (:functor-category "-FUNCTOR")
			    (:=-functor-category "-=-FUNCTOR")
			    (:>-functor-category "->-FUNCTOR")
			    (:<-functor-category "-<-FUNCTOR")
			    (:+-functor-category "-+-FUNCTOR")
			    (:finite-+-functor-category "-FINITE-+-FUNCTOR")
			    (:mod-+-functor-category "-MOD-+-FUNCTOR")
			    (:order-functor-category "-ORDER-FUNCTOR")
			    (:succ-functor-category "-SUCC-FUNCTOR")
			    (:pred-functor-category "-PRED-FUNCTOR"))))
	     ;; FIXME: Where to INTERN?
	     (intern (concatenate 'string prefix suffix))))
	 (category-slots (class-symbol category-symbol)
	   (ecase category-symbol
	     (:none-category
	      (list (list 'category
			  :allocation :class
			  :initform :none-category)))
	     (:=-category
	      (list (list 'category
			  :allocation :class
			  :initform :=-category)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :accessor '=-func)))
	     (:>-category
	      (list (list 'category
			  :allocation :class
			  :initform :>-category)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :accessor '>-func)))
	     (:<-category
	      (list (list 'category
			  :allocation :class
			  :initform :<-category)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :accessor '<-func)))
	     (:+-category
	      (list (list 'category
			  :allocation :class
			  :initform :+-category)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :accessor '+-func)
		    (list 'id
			  :type class-symbol
			  :allocation :class
			  :accessor 'identity)))
	     (:finite-+-category
	      (list (list 'category
			  :allocation :class
			  :initform :finite-+-category)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :accessor '+-func)
		    (list 'id
			  :type class-symbol
			  :allocation :class
			  :accessor 'identity)
		    (list 'n-inf
			  :type class-symbol
			  :allocation :class
			  :accessor 'negative-infinitum)
		    (list 'p-inf
			  :type class-symbol
			  :allocation :class
			  :accessor 'positive-infinitum)))
	     (:mod-+-category
	      (list (list 'category
			  :allocation :class
			  :initform :mod-+-category)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :accessor '+-func)
		    (list 'id
			  :type class-symbol
			  :allocation :class
			  :accessor 'identity)
		    (list 'mod
			  :type class-symbol
			  :allocation :class
			  :accessor 'modulus)))
	     (:order-category
	      (list (list 'category
			  :allocation :class
			  :initform :order-category)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :accessor '<-func)))
	     (:succ-category
	      (list (list 'category
			  :allocation :class
			  :initform :succ-category)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :accessor '<-func)
		    (list 'succ
			  :type 'function
			  :allocation :class
			  :accessor 'succ)
		    (list 'inf
			  :type 'function
			  :allocation :class
			  :accessor 'infinitum)))
	     (:pred-category
	      (list (list 'category
			  :allocation :class
			  :initform :pred-category)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :accessor '<-func)
		    (list 'pred
			  :type 'function
			  :allocation :class
			  :accessor 'pred)
		    (list 'inf
			  :type 'function
			  :allocation :class
			  :accessor 'infinitum)))
	     (:functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)))
	     (:=-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :=-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :accessor '=-func)))
	     (:>-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :>-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :accessor '>-func)))
	     (:<-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :<-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :accessor '<-func)))
	     (:+-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :+-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :accessor '+-func)
		    (list 'id
			  :type class-symbol
			  :allocation :class
			  :accessor 'identity)))
	     (:finite-+-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :finite-+-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :accessor '+-func)
		    (list 'id
			  :type class-symbol
			  :allocation :class
			  :accessor 'identity)
		    (list 'n-inf
			  :type class-symbol
			  :allocation :class
			  :accessor 'negative-infinitum)
		    (list 'p-inf
			  :type class-symbol
			  :allocation :class
			  :accessor 'positive-infinitum)))
	     (:mod-+-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :mod-+-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :accessor '+-func)
		    (list 'id
			  :type class-symbol
			  :allocation :class
			  :accessor 'identity)
		    (list 'mod
			  :type class-symbol
			  :allocation :class
			  :accessor 'modulus)))
	     (:order-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :order-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :accessor '<-func)))
	     (:succ-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :succ-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :accessor '<-func)
		    (list 'succ
			  :type 'function
			  :allocation :class
			  :accessor 'succ)))
	     (:pred-functor-category
	      (list (list 'category
			  :allocation :class
			  :initform :pred-functor-category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :accessor 'codomain)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :accessor '<-func)
		    (list 'pred
			  :type 'function
			  :allocation :class
			  :accessor 'pred))))))
    ;; We are facing what defined by ourselves...
    ;; TODO: Use CASE instead of ECASE?
    `(defclass ,(category-name class-symbol category-symbol) (class-symbol)
       ,(category-slots class-symbol category-symbol))))

;;; MAKE-OBJECT is a upper-abstraction of MAKE-INSTANCE, it returns a new
;;; instance which obey the category rule.
;;; TODO: Do we need this exactly?

