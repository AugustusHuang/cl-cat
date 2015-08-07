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

;;; Internal categories are made upon classes and with different method to
;;; indicate the inner relationship between different objects. A lot of
;;; categories are familiar, so I choose to define them internally, user can
;;; choose to extend those categories or define one on their own.

;;; A category has its object set and function set, object corresponding to
;;; our standard CLASS in CLOS, but generic function acts more like an
;;; endomorphism or mapping between objects from different categories.
;;; A category should have its inner relation, for example, a category of
;;; integers will be a total order system, that is, two integers are comparable
;;; and we can always get the result of (min a b), and there's only one
;;; answer falls in >, < and =. Likely if we compactify our finite integer
;;; category, add positive and negative infinitum and decide bignums as
;;; infinitum we can make our END-FINITE-INT a ADD-monoid.
;;; (That is, multiplication functor is addition.) Also it can be viewed
;;; as a monad on FINITE-INT.

;;; Here I will make the general interface to create a brand new category
;;; to build the inner structure of the category, and so on...

;;; NOTE: Here I only adopt the binary version of functions if I don't mention
;;; since we regard the operands as a whole, get the wrapper function then
;;; we use REDUCE to make them run.

;;; NONE-CATEGORY graph looks like
;;; +----------------------------+
;;; | A    B          C          |
;;; |   D        E        F      |
;;; +----------------------------+
;;; Where A..F have no interactions. (Our basic way to make instances acts
;;; like this!)
(defclass none-category () ()
  (:documentation
   "A basic category with no inner functions at all."))

;;; =-CATEGORY graph looks like
;;; +----------------------------+
;;; | A === B ============= C    |
;;; | |     |               |    |
;;; | D === E ============= F    |
;;; +----------------------------+
;;; Not exactly, but all two objects in this category can be compared with
;;; =-FUNCTION.
(defclass =-category (none-category)
  ((=-function
    :initarg :=-function
    :reader =-function))
  (:documentation
   "A category with function to test whether two objects are = or not."))

;;; >-CATEGORY and <-CATEGORY act like =-CATEGORY...
(defclass >-category (none-category)
  ((>-function
    :initarg :>-function
    :reader >-function))
  (:documentation
   "A category with function to test whether an object is > than another."))

(defclass <-category (none-category)
  ((<-function
    :initarg :<-function
    :reader <-function))
  (:documentation
   "A category with function to test whether an object is < than another."))

(defclass order-category (=-category >-category <-category)
  ()
  (:documentation
   "A category with function to test whether an object =, > or < with another object. Only one will be true. (So it's total ordered.)."))

;;; Here the '+' means general '+', even '*' can be interpreted as '+',
;;; it's only a 2-to-1 operad... Like C
;;;                                   ^
;;;                                   |
;;;                              A ---+--- B
(defclass +-category (none-category)
  ((+-function
    :initarg :+-function
    :reader +-function)
   ;; If the ADD function operate on a finite field (or interval) ...
   (negative-infinitum
    :initarg :n-infinitum
    :reader negative-infinitum)
   (positive-infinitum
    :initarg :p-infinitum
    :reader positive-infinitum))
  (:documentation
   "A category with function to add together two objects, the sum of which is also an valid object in this category."))

;;; FIXME: How about --categories? Add zero divisors and we are done?

;;; An object's successor must be > than this object, and an object has only
;;; one successor, but the counting step is not specified.
(defclass succ-category (order-category)
  ((succ-function
    :initarg :succ-function
    :reader succ)
   (infinitum
    :initarg :infinitum
    :reader infinitum))
  (:documentation
   "A category with function to get an object's successor, which is also a valid object in this category."))

(defclass pred-category (order-category)
  ((pred-function
    :initarg :pred-function
    :reader pred)
   (infinitum
    :initarg :infinitum
    :reader infinitum))
  (:documentation
   "A category with function to get an object's predecessor, which is also a valid object in this category."))

(defclass functor-category (none-category)
  ((domain-category
    :initarg :domain
    :reader domain)
   (codomain-category
    :initarg :codomain
    :reader codomain))
  (:documentation
   "A category made up with functors map functions in one category to functions in another."))

(defclass =-functor-category (functor-category =-category)
  ()
  (:documentation
   "A functor category in which functors have natural isomorphism to themselves."))

(defclass >-functor-category (functor-category >-category)
  ()
  (:documentation
   "A functor category in which functors can be compared by >."))

(defclass <-functor-category (functor-category <-category)
  ()
  (:documentation
   "A functor category in which functors can be compared by <."))

(defclass order-functor-category (functor-category order-category)
  ()
  (:documentation
   "A functor category in which functors can be total-ordered."))

(defclass +-functor-category (functor-category +-category)
  ()
  (:documentation
   "A functor category in which the sum of functors is also a functor in this category."))

(defclass succ-functor-category (functor-category succ-category)
  ()
  (:documentation
   "A functor category in which functors have their successors."))

(defclass pred-functor-category (functor-category pred-category)
  ()
  (:documentation
   "A functor category in which functors have their predecessors."))

