(ns parser
  (:require [grammars :as wae]))

(defn parser-AE [exp]
  (cond
    (number? exp) 
    (wae/numG exp)

    (and (list? exp) (= (first exp) '+))
    (wae/addG (parser-AE (second exp)) (parser-AE (nth exp 2)))

    (and (list? exp) (= (first exp) '-))
    (wae/subG (parser-AE (second exp)) (parser-AE (nth exp 2)))

    :else
    (throw (IllegalArgumentException. "Expresión AE inválida"))))

(defn parser-WAE [exp]
  (cond
    (number? exp)
    (wae/numG exp)

    (symbol? exp)
    (wae/idG exp)

    (and (list? exp) (= (first exp) '+))
    (wae/addG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))

    (and (list? exp) (= (first exp) '-))
    (wae/subG (parser-WAE (second exp)) (parser-WAE (nth exp 2)))

    (and (list? exp) (= (first exp) 'with))
    (let [[_ [id value] body] exp]
      (wae/withG (wae/bindings id (parser-WAE value)) (parser-WAE body)))
    
    :else
    (throw (IllegalArgumentException. "Expresión WAE inválida"))))