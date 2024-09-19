(ns differentiator.core
  (:require 
            [clojure.string :as clojure.string]
            [hiccups.runtime :as hiccupsrt]
            [dommy.core :as dommy :refer-macros [sel1]])
  (:require-macros [hiccups.core :as hiccups :refer [html]]))

(defn term-negative? [term]
  (< (:coefficient term) 0))

(defn sort-expression [expression]
  (reverse
    (sort 
      (fn [a b] 
        (cond
          (nil? (:symbol a)) -1
          (nil? (:symbol b)) 1
          :else (compare (:exponent a) (:exponent b))))
       expression)))

(defn combine-like-terms [expression]
  (->> expression
      (sort-expression)
      (group-by (juxt :symbol :exponent))
      (map 
        (comp 
          #(reduce 
             (fn [acc v] 
               (assoc acc :coefficient 
                      (+ (:coefficient acc) (:coefficient v)))) %) second))))

(defn normalize-parsed-exponent [exponent]
  (if (nil? exponent)
    1.0
    (cljs.core/parse-double (subs exponent 1))))

(defn normalize-parsed-coefficient [coefficient]
  (if (or (nil? coefficient) (= coefficient ""))
    1.0
    (cljs.core/parse-double coefficient)))

(defn normalize-parsed-symbol [sym]
  (if (= sym "")
    nil
    sym))

(defn normalize-parsed-sign [sign]
  (if (nil? sign)
    1.0
    -1.0))

(defn parse-term-string [term-string]
  (let [[_ sign coefficient sym exponent] 
        (re-matches #"(-)?(\d*\.?\d*)([a-zA-Z]*)(\^\d+\.?\d*)?" term-string)]
    {:coefficient (* (normalize-parsed-sign sign) 
                      (normalize-parsed-coefficient coefficient)) 
     :symbol (normalize-parsed-symbol sym) 
     :exponent (normalize-parsed-exponent exponent)}))

(defn clean-term-string [term-string]
  (clojure.string/replace term-string #"[+\s]*" ""))

(defn parse-expression-string [expression]
  (->> expression
      (re-seq #"[+-]?[^+-]+")
      (map #(parse-term-string (clean-term-string %)))
      (combine-like-terms)))

(defn differentiate-term [term]
  (let [new-coeff (* (:coefficient term) (:exponent term))
        new-exponent (- (:exponent term) 1)]
    (if (nil? (:symbol term))
      nil ; constant term
      (if (= (:exponent term) 1.0)
        (assoc term :symbol nil :exponent 1.0)
        (assoc term :coefficient new-coeff :exponent new-exponent)))))

(defn differentiate-expression [expression]
  (->> expression
      (map differentiate-term)
      (filter #(not (nil? %)))
      (combine-like-terms)))

(defn print-exponent [exponent]
  (if (nil? exponent)
    ""
    (if (= exponent 1.0)
      ""
      (str "^" exponent))))

(defn print-term [term]
  (if (nil? (:symbol term))
    (str (:coefficient term))
    (if (= (:exponent term) 1.0)
      (str (:coefficient term) (:symbol term) (print-exponent (:exponent term)))
      (str (:coefficient term) (:symbol term) (print-exponent (:exponent term))))))

; (= (parse-expression-string (print-expression (parse-expression-string x))) (parse-expression-string x))
(defn print-expression [expression]
  (reduce 
    (fn [acc v] 
      (if (term-negative? v)
        (str acc " - " (print-term (assoc v :coefficient (Math/abs (:coefficient v)))))
        (str acc " + " (print-term v)))) 
    (print-term (first expression)) 
    (rest expression)))


(defn solve-term [term x-value]
  (if (nil? (:symbol term))
    (:coefficient term)
    (* (:coefficient term) (Math/pow x-value (:exponent term)))))


(defn solve-expression [expression x-value]
  (reduce + (map #(solve-term % x-value) expression)))



(enable-console-print!)

(defonce app-state (atom {:expression ""}))


(defn input-html []
  [:input {:type "text" :name "expression" :value (:expression @app-state)}])

(defn result-html []
  [:p {:id "result"} "Result: " (print-expression (differentiate-expression (parse-expression-string (:expression @app-state))))])


(defn page-html []
  (html
    [:h1 "Differentiator"]
    [:form 
     (input-html)
     [:input {:type "submit" :value "Differentiate"}]]
    (result-html)))


(defn expression-input-handler [e]
  (swap! app-state assoc :expression (.. e -target -value)))

(add-watch app-state :on-change
           (fn [_ _ _ _]
              (-> (sel1 :#result)
                (dommy/set-html! (html (result-html)))
                (dommy/listen! :input expression-input-handler))))

(defn init []
  (-> (sel1 :main)
      (dommy/set-html! (page-html))
      (dommy/listen! :input expression-input-handler)))

(defn teardown []
  (->
   (sel1 :main)
   (dommy/set-html! "")
   (dommy/unlisten! :input expression-input-handler))
  )
  

(defn on-js-reload []
  (teardown)
  (init))
