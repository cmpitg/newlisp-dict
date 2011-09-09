#!/usr/bin/newlisp

;;;
;;; This file is a part of the **newlisp-dict** project, release under
;;; the terms of the MIT License
;;;
;;; See COPYING for more details.
;;;
;;; Copyright (c) 2011 by Dương "Yang" ヤン Hà Nguyễn <cmpitg@gmail.com>
;;;

(load-unittest)
(load "nl-dict.lsp")

(context 'DictTest)

(define-test (test_transforming)
  (assert= "__<DictTest.aoeu>!!" (Dict:->dict-name 'aoeu))
  (assert= "__<MAIN.a>!!" (Dict:->dict-name 'MAIN:a))
  (assert= "__<DictTest.*aoeu>!!" (Dict:->dict-name '*aoeu))

  ;; (assert= "__<DictTest.*aoeu>!!:__<DictTest.*aoeu>!!"
  ;;          (string (Dict:->dict-symbol '*aoeu)))
  ;; (assert= "__<DictTest.aoeu>!!:__<DictTest.aoeu>!!"
  ;;          (string (Dict:->dict-symbol 'aoeu)))
  ;; (assert= "__<MAIN.a>!!:__<MAIN.a>!!"
  ;;          (string (Dict:->dict-symbol 'MAIN:a)))
  )

(define-test (test_creating-and-accessing , my-dict my-dict-copy)
  (setq my-dict (Dict:dict-new))
  (my-dict "hello" "HELLO")
  (my-dict "world" ", WORLD!")
  (assert= "HELLO"     (my-dict "hello"))
  (assert= ", WORLD!"  (my-dict "world"))
  (assert= nil         (my-dict "Hello"))

  ;; making a copy of it, does it work?
  (setq my-dict-copy my-dict)
  (assert= "HELLO"     (my-dict-copy "hello"))
  (assert= ", WORLD!"  (my-dict-copy "world"))
  (assert= nil         (my-dict-copy "Hello"))

  ;; making a new one with the same name?
  (setq my-dict (Dict:dict-new))
  (assert= "HELLO"  (my-dict-copy "hello"))
  (assert= nil      (my-dict "hello"))
  )

(define-test (test_creating-and-accessing-after)
  ;; does 'my-dict still contains the old dictionary?
  (assert= nil my-dict)

  ;; how about creating a new one?
  (setq my-dict (Dict:dict-new))
  (my-dict "world" 'world)
  (assert= 'world (my-dict "world"))
  (assert= nil    (my-dict "hello"))
  )

(UnitTest:run-all 'DictTest)

(context 'MAIN)
(exit)
