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

(define-test (test_get-value)
  (assert= "aoeu"
           (get-value "*!*string-t!*!aoeu")))

(define-test (test_external-type)
  (assert= "aoeu"
           (->external-type "*!*string-t!*!aoeu"))
  (assert= 2022
           (->external-type "*!*integer-t!*!2022")))

(UnitTest:run-all 'Dict)

(context 'MAIN)
(exit)
