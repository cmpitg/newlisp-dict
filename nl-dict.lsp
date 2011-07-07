#!/usr/bin/newlisp

;;;
;;; This file is a part of the **newlisp-dict** project, release under
;;; the terms of the MIT License
;;;
;;; See COPYING for more details.
;;;
;;; Copyright (c) 2011 by Dương "Yang" ヤン Hà Nguyễn <cmpitg@gmail.com>
;;;

(context 'Dict)

(constant '+begin-context+ "__<")
(constant '+end-context+   ">!!")

;;;
;;; whenever ``dict-new a-symbol`` is called
;;;
;;; * transform a-symbol into Dict's notation, avoid naming conflicts
;;; * free it from all previous binding: (delete the-symbol)
;;; * create a new context on behalf of it: (define the-symbol:the-symbol)
;;;

(define (->dict-name x , str)
  (setq str (replace ":" (string x) "."))
  (append +begin-context+ str +end-context+))

(define (dict-new x , context-sym cont res)
  ;; transform the dictionary name to context symbol
  (context 'MAIN)
  (setq context-sym (->dict-name x))

  ;; TODO
  ;;
  ;; switch to the context and put the symbol of from
  ;; ``Something:Something`` to ``res``
  (context (sym context-sym))
  (delete (sym context-sym))
  (setq res (sym context-sym))

  ;; now, create the context, which is our dictionary
  (context 'MAIN)
  (eval (expand '(define res) 'res))

  ;; (println "In context: " (context) " " res " " x)

  ;; put the reference to the dictionary into the symbol of `x`
  (set x (eval (sym context-sym))))


(context 'MAIN)
