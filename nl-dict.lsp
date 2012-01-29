;;;
;;; This file is a part of the **newlisp-dict** project, release under
;;; the terms of the MIT License
;;;
;;; See COPYING for more details.
;;;
;;; Copyright (c) 2011-2012 by Dương "Yang" ヤン Hà Nguyễn
;;; <cmpitg@gmail.com>
;;;

;;; Require nl-tim

(context 'Dict)

(constant '+begin-context+ "<")
(constant '+end-context+   "!")
(constant '+begin-type+    "\x12")
(constant '+end-type+      "\x13")

(constant '+global-dict-symbol+ "N.d")

(define Dict.DictList:Dict.DictList)

;;; Load nl-tim
(load "nl-tim/nl-tim.lsp")

;;;
;;; whenever ``dict-new a-symbol`` is called
;;;
;;; * transform a-symbol into Dict's notation
;;; * void naming conflict simply by counting instances
;;; * create a new context on behalf of it
;;;

;;;
;;; helpers
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
  (setq context-sym-str (->dict-name (avoid-conflict-name +global-dict-symbol+)))

  ;; switch to the context and set the symbol of form
  ;; ``Something:Something`` into the variable ``res``
  (context (sym context-sym-str))
  (setq res (sym context-sym-str))

  ;; now, create our dictionary
  (context 'MAIN)
  (eval (expand '(define res) 'res))

  ;; and return it!
  (eval (sym context-sym-str)))

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

(define (internal-dict->list internal-dict)
  (map (fn (x)
         (list (->external-type (first x)) (x 1)))
       (filter (fn (x) (not (or (starts-with (first x) +begin-context+)
                                (ends-with   (first x)   +end-context+))))
               (internal-dict))))

(define (decode-type str)
  (1 (- (find +end-type+ str) 1) str))

(define (get-value str)
  ((+ (find +end-type+ str) 1) str))

(define (->internal-type symbol)
  (append +begin-type+ (string (type-of symbol)) +end-type+
          (string symbol)))


(define (->external-type str , tim-type)
  ;; * get the type
  ;; * convert back using ``read-expr`` or manually converting using
  ;;   my own reader/writer

  (setq tim-type  (decode-type str)
        tim-value (get-value str))

  (if (= "dict"    tim-type)    nil
      (= "string"  tim-type)    tim-value
      (read-expr tim-value)))

;;;
;;; main
;;;

(define (dict->list dict)
  "Convert to association list"
  (internal-dict->list (first (rest dict))))

(define (dict)
  "Return a dictionary of the form: (list 'dict-t dict-repr-as-context)"
  (letn ((real-dict     (dict-new))
         (keys-vals     (args))
         (n-keys        (/ (length keys-vals)
                           2)))
    (dotimes (i n-keys)
      (real-dict (->internal-type (keys-vals (* 2 i)))
                 (keys-vals (+ (* 2 i) 1))))

    (list 'dict-t real-dict)))

(define (-> dict key)
  "Getter"
  ((dict 1) (->internal-type key)))

(define (<- dict)
  "Setter"
  (letn ((real-dict     (dict 1))
         (keys-vals     (args))
         (n-keys        (/ (length keys-vals) 2)))
    (dotimes (i n-keys)
      (real-dict (->internal-type (keys-vals (* 2 i)))
                 (keys-vals (+ (* 2 i) 1))))))

(context 'MAIN)

(define dict->list   Dict:dict->list)
(define dict         Dict:dict)
(define ->           Dict:->)
(define <-           Dict:<-)

(make-global 'dict->list
             'dict
             '->
             '<-)
