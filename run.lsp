#!/usr/bin/newlisp

;;;
;;; This file is a part of the **newlisp-dict** project, release under
;;; the terms of the MIT License
;;;
;;; See COPYING for more details.
;;;
;;; Copyright (c) 2011 by Dương "Yang" ヤン Hà Nguyễn <cmpitg@gmail.com>
;;;

(load "nl-dict.lsp")

(context 'Trying)

(define a-dict (Dict:dict))
(define b-dict (Dict:dict 'a "Char: a"
                          "b" "String: b"
                          123 'number-123
                          true true
                          (list 'a 'b 'c) "List: '(a b c)"))
(context 'A)
(define d (Dict:dict 'a "10"))
(println (->list d))
(context 'Trying)
(println (->list A:d))

;; (println (->list a-dict))
;; (println (->list b-dict))

(context 'MAIN)
(exit)
