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
  (assert= "string"
           (decode-type "\x12string\x13Hello world"))
  (assert= "integer"
           (decode-type "\x12integer\x13120")))

(define-test (test_->internal-type)
  (assert= "\x12string\x13Hello world"
           (->internal-type "Hello world"))
  (assert= "\x12integer\x13123"
           (->internal-type 123))
  (assert= "\x12symbol\x13hellO"
           (->internal-type 'hellO))
  (assert= "\x12list\x13(3 2 1 \"abc\")"
           (->internal-type (list 3 2 1 "abc")))
  (assert= "\x12boolean\x13nil"
           (->internal-type nil))
  (assert= "\x12boolean\x13true"
           (->internal-type true))
  )

(define-test (test_get-value)
  (assert= "aoeu"
           (get-value "\x12string\x13aoeu")))

(define-test (test_->external-type)
  (assert= "aoeu"
           (->external-type "\x12string\x13aoeu"))
  (assert= 2022
           (->external-type "\x12integer\x132022"))
  (assert= 'abc
           (->external-type "\x12symbol\x13abc"))
  (assert= '(1 4 5 "a")
           (->external-type "\x12list\x13(1 4 5 \"a\")"))
  (assert= true
           (->external-type "\x12true\x13true"))
  (assert= nil
           (->external-type "\x12symbol\x13nil"))
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
