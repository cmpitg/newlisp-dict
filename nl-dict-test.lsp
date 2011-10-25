#!/usr/bin/newlisp

;;;
;;; This file is a part of the **newlisp-dict** project, release under
;;; the terms of the MIT License
;;;
;;; See COPYING for more details.
;;;
;;; Copyright (c) 2011 by Dương "Yang" ヤン Hà Nguyễn <cmpitg@gmail.com>
;;;

;;; Require newlisp-unittest

(load "nl-dict.lsp")

(context 'Dict)

(define-test (test_decode-type)
  (assert= "string-t"
           (decode-type "*!*string-t!*!Hello world"))
  (assert= "integer-t"
           (decode-type "*!*integer-t!*!120")))

(define-test (test_->internal-type)
  (assert= "*!*string-t!*!Hello world"
           (->internal-type "Hello world"))
  (assert= "*!*integer-t!*!123"
           (->internal-type 123))
  (assert= "*!*symbol-t!*!hellO"
           (->internal-type 'hellO))
  (assert= "*!*list-t!*!(3 2 1 \"abc\")"
           (->internal-type (list 3 2 1 "abc")))
  (assert= "*!*symbol-t!*!nil"
           (->internal-type nil))
  (assert= "*!*true-t!*!true"
           (->internal-type true))
  )

(define-test (test_get-value)
  (assert= "aoeu"
           (get-value "*!*string-t!*!aoeu")))

(define-test (test_->external-type)
  (assert= "aoeu"
           (->external-type "*!*string-t!*!aoeu"))
  (assert= 2022
           (->external-type "*!*integer-t!*!2022"))
  (assert= 'abc
           (->external-type "*!*symbol-t!*!abc"))
  (assert= '(1 4 5 "a")
           (->external-type "*!*list-t!*!(1 4 5 \"a\")"))
  (assert= true
           (->external-type "*!*true-t!*!true"))
  (assert= nil
           (->external-type "*!*symbol-t!*!nil"))
  )

(define-test (test_-> , d)
  (setq d (Dict:dict 'a "Char: a"
                     "b" "String: b"
                     123 'number-123
                     true true
                     (list 'a 'b 'c) "List: '(a b c)"))

  (assert= "Char: a"     (-> d 'a))
  (assert= "String: b"   (-> d "b"))
  (assert= 'number-123   (-> d 123))
  (assert= true          (-> d true))
  (assert= "List: '(a b c)"
           (-> d '(a b c)))
)

(define-test (test_<- , d)
  (setq d (Dict:dict 'a "Char: a"
                     "b" "String: b"
                     123 'number-123
                     true true
                     (list 'a 'b 'c) "List: '(a b c)"))

  (assert= "Char: a"     (-> d 'a))
  (<- d 'a "Symbol: 'a")
  (assert= "Symbol: 'a"  (-> d 'a))

  (<- d "b" {Just "b"}
      '123 '123)

  (assert= {Just "b"}   (-> d "b"))
  (assert= '123   (-> d 123))
)

(UnitTest:run-all 'Dict)

(context 'MAIN)
(exit)
