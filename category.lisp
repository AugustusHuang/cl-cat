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

;;; Default lower (0 and 1)-categories.
;;; NOTE: Here N-category means objects are in level N. e.g. 0-category has
;;; 0-objects and 1-functions (that is, normal OBJECTs and FUNCTIONs).
(deftype built-in-category ()
  '(member
    ;; 0-categories
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
    ;; 1-categories
    :function-category
    :=-function-category
    :>-function-category
    :<-function-category
    :+-function-category
    :finite-+-function-category
    :mod-+-function-category
    :order-function-category
    :succ-function-category
    :pred-function-category
    ;; 2-categories
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

(deftype built-in-category-short ()
  '(member
    :none-0 :=-0 :>-0 :<-0 :+-0 :finite-+-0 :mod-+-0 :order-0 :succ-0 :pred-0
    :none-1 :=-1 :>-1 :<-1 :+-1 :finite-+-1 :mod-+-1 :order-1 :succ-1 :pred-1
    :none-2 :=-2 :>-2 :<-2 :+-2 :finite-+-2 :mod-+-2 :order-2 :succ-2 :pred-2))

;;; From The Common Lisp Cookbook...
(defmacro build-symbol (&rest l)
  (flet ((symstuff (l)
	   `(concatenate 'string
			 ,@(mapcar (lambda (x)
				     (cond ((stringp x)
					    `',x)
					   ((atom x)
					    `',(format nil "~A" x))
					   ((eq (car x) ':<)
					    `(format nil "~A" ,(cadr x)))
					   (t
					    `(format nil "~A" ,x))))
				   l))))
    (let ((p (find-if (lambda (x)
			(and (consp x) (eq (car x) ':package)))
		      l)))
      (cond (p
	     (setq l (remove p l))))
      (let ((pkg (cond ((eq (cadr p) 'nil)
			nil)
		       (t
			`(find-package ',(cadr p))))))
	(cond (p
	       (cond (pkg
		      `(values (intern ,(symstuff l) ,pkg)))
		     (t
		      `(make-symbol ,(symstuff l)))))
	      (t
	       `(values (intern ,(symstuff l)))))))))

;;; DEFCATEGORY macro, will define the corresponding class of category and
;;; if given related function, initialize with given function.
;;; CLASS-NAME and CATEGORY-NAME are symbols of class and category.
(defmacro defcategory (class-name category-symbol)
  (flet ((category-slots (category-symbol)
	   (ecase category-symbol
	     (:none-0
	      (list (list 'category
			  :allocation :class
			  :initform :none-category
			  :reader 'category)))
	     (:=-0
	      (list (list 'category
			  :allocation :class
			  :initform :=-category
			  :reader 'category)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)))
	     (:>-0
	      (list (list 'category
			  :allocation :class
			  :initform :>-category
			  :reader 'category)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)))
	     (:<-0
	      (list (list 'category
			  :allocation :class
			  :initform :<-category
			  :reader 'category)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)))
	     (:+-0
	      (list (list 'category
			  :allocation :class
			  :initform :+-category
			  :reader 'category)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)))
	     (:finite-+-0
	      (list (list 'category
			  :allocation :class
			  :initform :finite-+-category
			  :reader 'category)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)
		    (list 'n-inf
			  :type 'class-name
			  :allocation :class
			  :initarg :negative-infinitum
			  :accessor 'negative-infinitum)
		    (list 'p-inf
			  :type 'class-name
			  :allocation :class
			  :initarg :positive-infinitum
			  :accessor 'positive-infinitum)))
	     (:mod-+-0
	      (list (list 'category
			  :allocation :class
			  :initform :mod-+-category
			  :reader 'category)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)
		    (list 'mod
			  :type 'class-name
			  :allocation :class
			  :initarg :modulus
			  :accessor 'modulus)))
	     (:order-0
	      (list (list 'category
			  :allocation :class
			  :initform :order-category
			  :reader 'category)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)))
	     (:succ-0
	      (list (list 'category
			  :allocation :class
			  :initform :succ-category
			  :reader 'category)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)
		    (list 'succ
			  :type 'function
			  :allocation :class
			  :initarg :succ
			  :accessor 'succ)
		    (list 'inf
			  :type 'class-name
			  :allocation :class
			  :initarg :infinitum
			  :accessor 'infinitum)))
	     (:pred-0
	      (list (list 'category
			  :allocation :class
			  :initform :pred-category
			  :reader 'category)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)
		    (list 'pred
			  :type 'function
			  :allocation :class
			  :initarg :pred
			  :accessor 'pred)
		    (list 'inf
			  :type 'class-name
			  :allocation :class
			  :initarg :infinitum
			  :accessor 'infinitum)))
	     (:none-1
	      (list (list 'category
			  :allocation :class
			  :initform :function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :accessor 'ate
			  :initarg :ate
			  :initform 0)))
	     (:=-1
	      (list (list 'category
			  :allocation :class
			  :initform :=-function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)))
	     (:>-1
	      (list (list 'category
			  :allocation :class
			  :initform :>-function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)))
	     (:<-1
	      (list (list 'category
			  :allocation :class
			  :initform :<-function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)))
	     (:+-1
	      (list (list 'category
			  :allocation :class
			  :initform :+-function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)))
	     (:finite-+-1
	      (list (list 'category
			  :allocation :class
			  :initform :finite-+-function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)
		    (list 'n-inf
			  :type 'class-name
			  :allocation :class
			  :initarg :negative-infinitum
			  :accessor 'negative-infinitum)
		    (list 'p-inf
			  :type 'class-name
			  :allocation :class
			  :initarg :positive-infinitum
			  :accessor 'positive-infinitum)))
	     (:mod-+-1
	      (list (list 'category
			  :allocation :class
			  :initform :mod-+-function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)
		    (list 'mod
			  :type 'class-name
			  :allocation :class
			  :initarg :modulus
			  :accessor 'modulus)))
	     (:order-1
	      (list (list 'category
			  :allocation :class
			  :initform :order-nfunction-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)))
	     (:succ-1
	      (list (list 'category
			  :allocation :class
			  :initform :succ-function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)
		    (list 'succ
			  :type 'function
			  :allocation :class
			  :initarg :succ
			  :accessor 'succ)
		    (list 'inf
			  :type 'class-name
			  :allocation :class
			  :initarg :infinitum
			  :accessor 'infinitum)))
	     (:pred-1
	      (list (list 'category
			  :allocation :class
			  :initform :pred-function-category
			  :reader 'category)
		    (list 'lambda
			  :type 'function
			  :allocation :class
			  :initarg :func
			  :accessor 'func)
		    (list 'ate
			  :initarg :ate
			  :accessor 'ate)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)
		    (list 'pred
			  :type 'function
			  :allocation :class
			  :initarg :pred
			  :accessor 'pred)
		    (list 'inf
			  :type 'class-name
			  :allocation :class
			  :initarg :infinitum
			  :accessor 'infinitum)))
	     (:none-2
	      (list (list 'category
			  :allocation :class
			  :initform :functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)))
	     (:=-2
	      (list (list 'category
			  :allocation :class
			  :initform :=-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)))
	     (:>-2
	      (list (list 'category
			  :allocation :class
			  :initform :>-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)))
	     (:<-2
	      (list (list 'category
			  :allocation :class
			  :initform :<-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)))
	     (:+-2
	      (list (list 'category
			  :allocation :class
			  :initform :+-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)))
	     (:finite-+-2
	      (list (list 'category
			  :allocation :class
			  :initform :finite-+-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)
		    (list 'n-inf
			  :type 'class-name
			  :allocation :class
			  :initarg :negative-infinitum
			  :accessor 'negative-infinitum)
		    (list 'p-inf
			  :type 'class-name
			  :allocation :class
			  :initarg :positive-infinitum
			  :accessor 'positive-infinitum)))
	     (:mod-+-2
	      (list (list 'category
			  :allocation :class
			  :initform :mod-+-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '+-func
			  :type 'function
			  :allocation :class
			  :initarg :+-func
			  :accessor '+-func)
		    (list 'id
			  :type 'class-name
			  :allocation :class
			  :initarg :id
			  :accessor 'id)
		    (list 'mod
			  :type 'class-name
			  :allocation :class
			  :initarg :modulus
			  :accessor 'modulus)))
	     (:order-2
	      (list (list 'category
			  :allocation :class
			  :initform :order-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)))
	     (:succ-2
	      (list (list 'category
			  :allocation :class
			  :initform :succ-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)
		    (list 'succ
			  :type 'function
			  :allocation :class
			  :initarg :succ
			  :accessor 'succ)
		    (list 'inf
			  :type 'class-name
			  :allocation :class
			  :initarg :infinitum
			  :accessor 'infinitum)))
	     (:pred-2
	      (list (list 'category
			  :allocation :class
			  :initform :pred-functor-category
			  :reader 'category)
		    (list 'dom
			  :type 'symbol
			  :allocation :class
			  :initarg :domain
			  :accessor 'domain)
		    (list 'cod
			  :type 'symbol
			  :allocation :class
			  :initarg :codomain
			  :accessor 'codomain)
		    (list '=-func
			  :type 'function
			  :allocation :class
			  :initarg :=-func
			  :accessor '=-func)
		    (list '>-func
			  :type 'function
			  :allocation :class
			  :initarg :>-func
			  :accessor '>-func)
		    (list '<-func
			  :type 'function
			  :allocation :class
			  :initarg :<-func
			  :accessor '<-func)
		    (list 'pred
			  :type 'function
			  :allocation :class
			  :initarg :pred
			  :accessor 'pred)
		    (list 'inf
			  :type 'class-name
			  :allocation :class
			  :initarg :infinitum
			  :accessor 'infinitum))))))
    ;; We are facing what defined by ourselves...
    ;; We use short symbol here...
    `(defclass ,(build-symbol (:< class-name) "-" (:< category-symbol)) ,(list class-name)
       ,(category-slots category-symbol))))

;;; FIND-CATEGORY is a wrapper of FIND-CLASS...
(defun find-category (class-symbol category-symbol)
  (find-class (build-symbol (:< class-symbol) "-" (:< category-symbol))))
