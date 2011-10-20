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

(context 'Testing)

(define a-dict (Dict:dict))
(define b-dict (Dict:dict "a" 1))

;; (println (->list a-dict))
;; (println (->list b-dict))

(context 'MAIN)
(exit)
