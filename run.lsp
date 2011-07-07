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

;;;
;;; begin trying

(context 'AAA)

;; (println "New: " (setq hello (Dict:dict-new 'hello)))
(println "New: " (Dict:dict-new 'hello))
(println "In context: " (context))
(println (hello "world" "Hee hee hee"))
(println (hello "world"))
(println (MAIN:__<AAA.hello++0>!! "world"))
(println (MAIN:__<AAA.hello++0>!! "world" "WORLD"))
(println (MAIN:__<AAA.hello++0>!! "world"))
(println (hello "world"))

(println MAIN:hello)

;;; desired output

;; New: __<AAA.hello>!!
;; In context: AAA
;; Hee hee hee
;; Hee hee hee
;; Hee hee hee
;; WORLD
;; WORLD
;; WORLD
;; nil

;;; end trying
;;;

;;;
;;; begin trying again

(local (hello)
  (println "New: " (Dict:dict-new 'hello))
  (println "This should be nil: " (hello "world"))
  (hello "10" "abc")
  (println "This should be (10 abc): " (Dict:get-pairs hello)))

(println "This should be WORLD: " (hello "world"))
(println "This should be nil: " (hello "10"))
(println (Dict:get-pairs hello))

;;; end trying again
;;;