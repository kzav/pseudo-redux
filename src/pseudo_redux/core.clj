(ns pseudo-redux.core
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clostache.parser :as parser])
  (:import (java.io PushbackReader))
  (:gen-class))

(def event-map {:label  "click"
                :text   "change"
                :check  "click"
                :select "change"
                :button "click"
                :table  "click"})

(def verb-map {:label  ""
               :text   "change"
               :check  "change"
               :select "change"
               :button "click"
               :table  "select"})

; 要素タイプの和名
(def type-name-JP {:label  "ラベル"
                   :text   "テキスト"
                   :check  "チェックボックス"
                   :select "ドロップダウンリスト"
                   :button "ボタン"
                   :table  "テーブル"})

; 動詞の和名
(def verb-name-JP {"change" "変更"
                   "select" "選択"})

(defn camel-to-snake
  [s]
  (let [snake (s/upper-case (s/replace s #"([A-Z])" "_$1"))]
    (if (= (first snake) \_)
      (subs snake 1)
      snake)))

(defn camel-to-pascal
  [s]
  (str (s/upper-case (subs s 0 1)) (subs s 1)))

(defn prefixed-name
  [prefix name]
  (str prefix "_" name))

(defn verb
  [id type]
  (str (type verb-map) (camel-to-pascal id)))

(defn read-def
  [path]
  (let [reader (PushbackReader. (io/reader path))]
    (edn/read reader)))

(defn output-name
  [key defs]
  (if (= key :entrypoint)
    (get-in defs [:component :entrypoint])
    (str (get-in defs [:component :name]) (s/upper-case (subs (name key) 0 1)) (subs (name key) 1) ".js")))

(defn common-map
  [defs]
  {:component-name (get-in defs [:component :name])})

;; 状態識別子
;(defmulti state-id (fn [element] (:type element)))
;
;; 状態識別子
;(defmethod state-id :book [book]
;  (str (:title book) "/" (:author book)))

(defn state-map
  [defs]
  (let [html-elements (get-in defs [:component :html-elements])
        state-map     {:elements (for [x html-elements :when (some #(= (:type x) %) [:text :select])]
                                   {:id             (:id x)
                                    :state-id (case (:type x)
                                                :text (:id x)
                                                :select (str "selected" (camel-to-pascal (:id x))))
                                    :state-value (case (:type x)
                                                   :text "\"\""
                                                   :select "null")
                                    })}]
    state-map))

(defn action-map
  [defs state-map]
  (let [component-name  (get-in defs [:component :name])
        html-use-prefix (get-in defs [:component :html-use-prefix])
        html-elements   (get-in defs [:component :html-elements])
        state           (:elements state-map)
        action-map      {:elements (for [x html-elements]
                                     (let [verb-string (if (contains? x :action)
                                                         (:action x)
                                                         ((:type x) verb-map))
                                           name (if (contains? x :action)
                                                  (:action x)
                                                  (verb (:id x) (:type x)))]
                                       {:id             (:id x)
                                        :id-descriptor  (camel-to-snake name)
                                        :id-value       (camel-to-snake
                                                          (if html-use-prefix
                                                            (prefixed-name component-name name)
                                                            name))
                                        :name           name
                                        :comment        (if-let [v (:name x)]
                                                          (str v (get verb-name-JP verb-string))
                                                          name)
                                        :action-value   (case (:type x)
                                                          :text   "currentTarget.value"
                                                          :select "currentTarget.options[currentTarget.selectedIndex]"
                                                          "currentTarget"
                                                          )
                                        :action-code    (case (:type x)
                                                          :text   (str "state." (:state-id (first (filter #(= (:id %) (:id x)) state))) " = action.payload;")
                                                          :select (str "state." (:state-id (first (filter #(= (:id %) (:id x)) state))) " = action.payload;")
                                                          "// TODO"
                                                          )
                                        }))}]
    action-map))

(defn view-map
  [defs state-map]
  (let [component-name  (get-in defs [:component :name])
        html-use-prefix (get-in defs [:component :html-use-prefix])
        html-elements   (get-in defs [:component :html-elements])
        state           (:elements state-map)
        view-map        {:elements (for [x html-elements]
                                     (let [name (camel-to-pascal (:id x))
                                           id-descriptor (camel-to-snake (:id x))]
                                       {:id             (:id x)
                                        :id-descriptor  id-descriptor
                                        :id-value       (if html-use-prefix
                                                          (prefixed-name component-name (:id x))
                                                          (:id x))
                                        :name           name
                                        :event          ((:type x) event-map)
                                        :comment        (if-let [v (:name x)]
                                                          (str v ((:type x) type-name-JP))
                                                          name)
                                        :view-code      (case (:type x)
                                                          :text (str
"    let input = core.getElement("
id-descriptor
");
    input.value = state."
(:state-id (first (filter #(= (:id %) (:id x)) state)))
";
    return input;")
                                                          :select (str
"    let input = core.getElement("
id-descriptor
");
    input.value = state."
(:state-id (first (filter #(= (:id %) (:id x)) state)))
".value;
    return input;")
                                                          :button (str
"    let input = core.getElement("
id-descriptor
");
    return input;")
"    // TODO
    return null;"
                                                          )
                                        }))}]
    view-map))

(defn variable-map
  [key defs]
  (let [common-map  (common-map defs)
        state-map   (state-map defs)
        action-map  (action-map defs state-map)
        view-map    (view-map defs state-map)]
    (if (or (= key :action) (= key :reducer))
      (-> common-map
          (assoc :state-elements (:elements state-map))
          (assoc :action-elements (:elements action-map)))
      (if (= key :view)
        (-> common-map
            (assoc :view-elements (:elements view-map)))
        (-> common-map
            (assoc :state-elements (:elements state-map))
            (assoc :view-elements (:elements view-map))
            (assoc :action-elements (:elements action-map))
            (assoc :bind-elements (for [x (:elements view-map) y (:elements action-map)
                                        :when (= (:id x) (:id y))]
                                    {:bind-name (:name x)
                                     :action-name (:name y)})))))))

(defn output-file
  [template defs]
  (spit
    (output-name (key template) defs)
    (parser/render-resource (val template) (variable-map (key template) defs))
    :encoding "UTF-8"))

; キーワードを小文字に変換
(defn to-lower-case-keyword
  [kw]
  (keyword (s/replace (s/lower-case kw) ":" "")))

; HTML要素タイプのキーワードを小文字に変換
(defn convert-html-element-type-keyword-to-lower-case
  [element]
  (-> element
      (assoc :type (to-lower-case-keyword (:type element)))))

; 全HTML要素タイプのキーワードを小文字に変換
(defn convert-all-html-element-type-keyword-to-lower-case
  [defs]
  (let [before  (get-in defs [:component :html-elements])
        after   (map convert-html-element-type-keyword-to-lower-case before)]
    (-> defs
        (assoc-in [:component :html-elements] after))))

; pdx本体関数
(defn pdx
  [path]
  (let [defs (convert-all-html-element-type-keyword-to-lower-case (read-def path))]
    (doseq [x (:templates defs)]
      (output-file x defs))))

; テスト
(defn foo
  [x]
  (println x "Hello, World!"))

; エントリーポイント(main関数)
(defn -main
  [& args]
  ;(foo (first args)))
  (pdx (first args)))
