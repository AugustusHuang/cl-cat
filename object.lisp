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

;;; MAKE-OBJECT is a upper-abstraction of MAKE-INSTANCE, it returns a new
;;; instance which obey the category rule.
;;; TODO: Do we need this exactly? Since if we have one class-category object,
;;; we can get the class symbol of it and call MAKE-INSTANCE then.
(defun make-none-object (class)
  (make-instance (or (find-category class :none-category)
		     (defcategory class :none-category))))

(defun make-=-object (class &rest =-func)
  (let ((cc (or (find-category class :=-category)
		(defcategory class :=-category))))
    (if (null (car =-func))
	(make-instance cc)
	(make-instance cc :=-func =-func))))

(defun make->-object (class &rest >-func)
  (let ((cc (or (find-category class :>-category)
		(defcategory class :>-category))))
    (if (null (car >-func))
	(make-instance cc)
	(make-instance cc :>-func >-func))))

(defun make-<-object (class &rest <-func)
  (let ((cc (or (find-category class :<-category)
		(defcategory class :<-category))))
    (if (null (car <-func))
	(make-instance cc)
	(make-instance cc :<-func <-func))))

(defun make-+-object (class &rest +-func-id)
  (let ((cc (or (find-category class :+-category)
		(defcategory class :+-category))))
    (case (length +-func-id)
      (2 (make-instance cc :+-func (first +-func-id)
			:identity (second +-func-id)))
      (1 (make-instance cc :+-func (car +-func-id)))
      (0 (make-instance cc))
      (t (error "too many arguments")))))

(defun make-finite-+-object (class &rest +-func-id-ninf-pinf)
  (let ((cc (or (find-category class :finite-+-category)
		(defcategory class :finite-+-category))))
    (case (length +-func-id-ninf-pinf)
      (4 (make-instance cc
			:+-func (first +-func-id-ninf-pinf)
			:identity (second +-func-id-ninf-pinf)
			:negative-infinitum (third +-func-id-ninf-pinf)
			:positive-infinitum (fourth +-func-id-ninf-pinf)))
      (3 (make-instance cc
			:+-func (first +-func-id-ninf-pinf)
			:identity (second +-func-id-ninf-pinf)
			:negative-infinitum (third +-func-id-ninf-pinf)))
      (2 (make-instance cc
			:+-func (first +-func-id-ninf-pinf)
			:identity (second +-func-id-ninf-pinf)))
      (1 (make-instance cc
			:+-func (first +-func-id-ninf-pinf)))
      (0 (make-instance cc))
      (t (error "too many arguments")))))

(defun make-mod-+-object (class &rest +-func-id-mod)
  (let ((cc (or (find-category class :mod-+-category)
		(defcategory class :mod-+-category))))
    (case (length +-func-id-mod)
      (3 (make-instance cc
			:+-func (first +-func-id-mod)
			:identity (second +-func-id-mod)
			:modulus (third +-func-id-mod)))
      (2 (make-instance cc
			:+-func (first +-func-id-mod)
			:identity (second +-func-id-mod)))
      (1 (make-instance cc
			:+-func (first +-func-id-mod)))
      (0 (make-instance cc))
      (t (error "too many arguments")))))

(defun make-order-category (class &rest =->-<-func)
  (let ((cc (or (find-category class :order-category)
		(defcategory class :order-category))))
    (case (length =->-<-func)
      (3 (make-instance cc
			:=-func (first =->-<-func)
			:>-func (second =->-<-func)
			:<-func (third =->-<-func)))
      (2 (make-instance cc
			:=-func (first =->-<-func)
			:>-func (second =->-<-func)))
      (1 (make-instance cc
			:=-func (first =->-<-func)))
      (0 (make-instance cc))
      (t (error "too many arguments")))))

(defun make-succ-object (class &rest =->-<-func-succ-inf)
  (let ((cc (or (find-category class :succ-category)
		(defcategory class :succ-category))))
    (case (length =->-<-func-succ-inf)
      (5 (make-instance cc
			:=-func (first =->-<-func-succ-inf)
			:>-func (second =->-<-func-succ-inf)
			:<-func (third =->-<-func-succ-inf)
			:succ (fourth =->-<-func-succ-inf)
			:infinitum (fifth =->-<-func-succ-inf)))
      (4 (make-instance cc
			:=-func (first =->-<-func-succ-inf)
			:>-func (second =->-<-func-succ-inf)
			:<-func (third =->-<-func-succ-inf)
			:succ (fourth =->-<-func-succ-inf)))
      (3 (make-instance cc
			:=-func (first =->-<-func-succ-inf)
			:>-func (second =->-<-func-succ-inf)
			:<-func (third =->-<-func-succ-inf)))
      (2 (make-instance cc
			:=-func (first =->-<-func-succ-inf)
			:<-func (second =->-<-func-succ-inf)))
      (1 (make-instance cc
			:=-func (first =->-<-func-succ-inf)))
      (0 (make-instance cc))
      (t (error "too many arguments")))))

(defun make-pred-object (class &rest =->-<-func-pred-inf)
  (let ((cc (or (find-category class :pred-category)
		(defcategory class :pred-category))))
    (case (length =->-<-func-pred-inf)
      (5 (make-instance cc
			:=-func (first =->-<-func-pred-inf)
			:>-func (second =->-<-func-pred-inf)
			:<-func (third =->-<-func-pred-inf)
			:pred (fourth =->-<-func-pred-inf)
			:infinitum (fifth =->-<-func-pred-inf)))
      (4 (make-instance cc
			:=-func (first =->-<-func-pred-inf)
			:>-func (second =->-<-func-pred-inf)
			:<-func (third =->-<-func-pred-inf)
			:pred (fourth =->-<-func-pred-inf)))
      (3 (make-instance cc
			:=-func (first =->-<-func-pred-inf)
			:>-func (second =->-<-func-pred-inf)
			:<-func (third =->-<-func-pred-inf)))
      (2 (make-instance cc
			:=-func (first =->-<-func-pred-inf)
			:<-func (second =->-<-func-pred-inf)))
      (1 (make-instance cc
			:=-func (first =->-<-func-pred-inf)))
      (0 (make-instance cc))
      (t (error "too many arguments")))))

;;; TODO: How to make functor-categories (i.e. 2-categories) run?
