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

(defpackage :cl-cat
  (:use :cl)
  (:documentation
   "Category level abstraction of Common Lisp.")
  (:export
   :none-category
   :none-category-p
   :=-category
   :=-category-p
   :>-category
   :>-category-p
   :<-category
   :<-category-p
   :+-category
   :+-category-p
   :finite-+-category
   :finite-+-category-p
   :mod-+-category
   :mod-+-category-p
   :succ-category
   :succ-category-p
   :pred-category
   :pred-category-p
   :function-category
   :function-category-p
   :=-function-category
   :=-function-category-p
   :>-function-category
   :>-function-category-p
   :<-function-category
   :<-function-category-p
   :+-function-category
   :+-function-category-p
   :finite-+-function-category
   :finite-+-function-category-p
   :mod-+-function-category
   :mod-+-function-category-p
   :order-function-category
   :order-function-category-p
   :succ-function-category
   :succ-function-category-p
   :pred-function-category
   :pred-function-category-p
   :functor-category
   :functor-category-p
   :=-functor-category
   :=-functor-category-p
   :>-functor-category
   :>-functor-category-p
   :<-functor-cateogry
   :<-functor-category-p
   :+-functor-category
   :+-functor-category-p
   :finite-+-functor-category
   :finite-+-functor-category-p
   :mod-+-functor-category
   :mod-+-functor-category-p
   :succ-functor-category
   :succ-functor-category-p
   :pred-functor-category
   :pred-functor-category-p
   :build-symbol
   :defcategory
   :find-category
   :make-none-object
   :make-=-object
   :make->-object
   :make-<-object
   :make-+-object
   :make-finite-+-object
   :make-mod-+-object
   :make-order-object
   :make-succ-object
   :make-pred-object))

(defpackage :cl-cat-test
  (:use :cl
	:cl-cat)
  (:documentation
   "Test functions and definitions of CL-CAT."))
