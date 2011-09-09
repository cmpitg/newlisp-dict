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

(define Dict.DictList:Dict.DictList)

;;;
;;; whenever ``dict-new a-symbol`` is called
;;;
;;; * transform a-symbol into Dict's notation
;;; * void naming conflict simply by counting instances
;;; * create a new context on behalf of it
;;;

(define (->dict-name x , str)
  "Transforming a symbol to a dictionary name"
  (setq str (replace ":" (string x) "."))
  (append +begin-context+ str +end-context+))

(define (dict-new , context-sym-str cont res)
  "Create a new dictionary as the return value"

  ;; transform the dictionary name to context symbol, avoid naming
  ;; conflict
  (context 'MAIN)
  (setq context-sym-str (->dict-name (avoid-conflict-name "NL-DICT.DICT")))
  (println "-- debug >> context-sym-str " context-sym-str)

  ;; switch to the context and set the symbol of form
  ;; ``Something:Something`` into the variable ``res``
  (context (sym context-sym-str))
  (setq res (sym context-sym-str))

  ;; now, create our dictionary
  (context 'MAIN)
  (eval (expand '(define res) 'res))

  ;; and return it!
  (eval (sym context-sym-str)))

(define (get-pairs dict)
  (filter (fn (x) (not (or (starts-with (first x) +begin-context+)
                           (ends-with   (first x)   +end-context+))))
          (dict)))

(define (avoid-conflict-name context-str)
  "Check whether the name is created, and increase the counter if necessary"
  ;; (println "-- avoid... => " context-str " " (Dict.DictList))
  (if (number? (Dict.DictList context-str))
      (begin
        (setq counter (+ 1 (Dict.DictList context-str)))
        (Dict.DictList context-str counter)
        (append context-str "++" (string counter)))
      (begin
        (Dict.DictList context-str 0)
        (append context-str "++0"))))

(context 'MAIN)
