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

(declare (inline none-category-p
		 =-category-p
		 >-category-p
		 <-category-p
		 +-category-p
		 succ-category-p
		 pred-category-p
		 functor-category-p
		 =-functor-category-p
		 >-functor-category-p
		 <-functor-category-p
		 +-functor-category-p
		 succ-functor-category-p
		 pred-functor-category-p))

(defun none-category-p (cc)
  (typep (class-category-category cc) 'none-category))

(defun =-category-p (cc)
  (typep (class-category-category cc) '=-category))

(defun >-category-p (cc)
  (typep (class-category-category cc) '>-category))

(defun <-category-p (cc)
  (typep (class-category-category cc) '<-category))

(defun +-category-p (cc)
  (typep (class-category-category cc) '+-category))

(defun succ-category-p (cc)
  (typep (class-category-category cc) 'succ-category))

(defun pred-category-p (cc)
  (typep (class-category-category cc) 'pred-category))

(defun functor-category-p (cc)
  (typep (class-category-category cc) 'functor-category))

(defun =-functor-category-p (cc)
  (typep (class-category-category cc) '=-functor-category))

(defun >-functor-category-p (cc)
  (typep (class-category-category cc) '>-functor-category))

(defun <-functor-category-p (cc)
  (typep (class-category-category cc) '<-functor-category))

(defun +-functor-category-p (cc)
  (typep (class-category-category cc) '+-functor-category))

(defun succ-functor-category-p (cc)
  (typep (class-category-category cc) 'succ-functor-category))

(defun pred-functor-category-p (cc)
  (typep (class-category-category cc) 'pred-functor-category))
