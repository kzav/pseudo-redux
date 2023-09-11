(ns pseudo-redux.core
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clostache.parser :as parser])
  (:import (java.io PushbackReader))
  (:gen-class))

(def event-map {:label  "click"
                :text   "change"
                :select "change"
                :check  "click"
                :radio  "change"
                :button "click"
                :table  "click"})

(def verb-map {:label  ""
               :text   "change"
               :select "change"
               :check  "change"
               :radio  "change"
               :button "click"
               :table  "select"})

; 要素タイプの和名
(def type-name-JP {:label  "ラベル"
                   :text   "テキスト"
                   :select "ドロップダウンリスト"
                   :check  "チェックボックス"
                   :radio  "ラジオボタン"
                   :button "ボタン"
                   :table  "テーブル"})

; 動詞の和名
(def verb-name-JP {"change" "変更"
                   "select" "選択"})

; キャメルタイプをスネークタイプに変換
(defn camel-to-snake
  [s]
  (let [snake (s/upper-case (s/replace s #"([A-Z])" "_$1"))]
    (if (= (first snake) \_)
      (subs snake 1)
      snake)))

; キャメルタイプをパスカルタイプに変換
(defn camel-to-pascal
  [s]
  (str (s/upper-case (subs s 0 1)) (subs s 1)))

; プレフィクス名
(defn prefixed-name
  [prefix name]
  (str prefix "_" name))

; コメントを省略
(defn abbreviate-comment
  [s]
  (-> s
      (s/replace #":.+" "")
      (s/replace #"/.+" "")
      (s/replace #"\(.+" "")))

; 動詞
(defn verb
  [id type]
  (str (type verb-map) (camel-to-pascal id)))

; 定義ファイルを読み込む
(defn read-def
  [path]
  (let [reader (PushbackReader. (io/reader path))]
    (edn/read reader)))

; 出力名
(defn output-name
  [key defs]
  (if (= key :entrypoint)
    (get-in defs [:component :entrypoint])
    (str (get-in defs [:component :name]) (s/upper-case (subs (name key) 0 1)) (subs (name key) 1) ".js")))

; 共通マップ
(defn common-map
  [defs]
  {:component-name (get-in defs [:component :name])})

;------------------------------------------------------------------------------
; 状態
;------------------------------------------------------------------------------

; 状態識別子
(defmulti state-id (fn [element] (:type element)))

; テキスト型の状態識別子
(defmethod state-id :text
  [element]
  (:id element))

; ドロップダウンリスト型の状態識別子
(defmethod state-id :select
  [element]
  (str "selected" (camel-to-pascal (:id element))))

; ラジオボタン型の状態識別子
(defmethod state-id :radio
  [element]
  (str "selected" (camel-to-pascal (:group element))))

; その他の状態識別子
(defmethod state-id :default
  [element]
  nil)

; 状態初期値
(defmulti state-value (fn [element] (:type element)))

; テキスト型の状態初期値
(defmethod state-value :text
  [element]
  "\"\"")

; ラジオボタン型の状態初期値
(defmethod state-value :radio
  [element]
  "\"\"")

; その他の状態初期値
(defmethod state-value :default
  [element]
  "null")

; 状態要素マップ
(defn state-map
  [defs]
  (let [html-elements   (get-in defs [:component :html-elements])
        unit-elements   (filter #(nil? (:group %)) html-elements)
        group-elements  (filter #(not (nil? (:group %))) html-elements)
        unit-state      (for [x unit-elements :when (some #(= (:type x) %) [:text :select])]
                          {:id           (:id x)
                           :state-id     (state-id x)
                           :state-value  (state-value x)
                           })
        group-state     (if (empty? group-elements)
                          []
                          (for [x (group-by :group group-elements)]
                            (let [group-id  (key x)
                                  element   (first (val x))]
                              {:id           group-id
                               :state-id     (state-id element)
                               :state-value  (state-value element)
                               })))
        state-map       {:elements (concat unit-state group-state)}
        ]
    state-map))

;------------------------------------------------------------------------------
; アクション
;------------------------------------------------------------------------------

; アクションID
(defmulti action-id (fn [element] (:type element)))

; ラジオボタン型のアクションID
(defmethod action-id :radio
  [element]
  (:group element))

; その他のアクションID
(defmethod action-id :default
  [element]
  (:id element))

; アクション名
(defmulti action-name (fn [element] (:type element)))

; ラジオボタン型のアクション名
(defmethod action-name :radio
  [element]
  (if (contains? element :action)
    (:action element)
    (verb (:group element) (:type element))))

; その他のアクション名
(defmethod action-name :default
  [element]
  (if (contains? element :action)
    (:action element)
    (verb (:id element) (:type element))))

; アクション取得値
(defmulti action-value (fn [element] (:type element)))

; テキスト型のアクション取得値
(defmethod action-value :text
  [element]
  "currentTarget.value")

; ドロップダウンリスト型のアクション取得値
(defmethod action-value :select
  [element]
  "currentTarget.options[currentTarget.selectedIndex]")

; ラジオボタン型のアクション取得値
(defmethod action-value :radio
  [element]
  "util.getCheckedRadioValue(currentTarget.name)")

; その他のアクション取得値
(defmethod action-value :default
  [element]
  "currentTarget")

; リデューサ実装
(defmulti reducer-code (fn [element state] (:type element)))

; テキスト型のリデューサ実装
(defmethod reducer-code :text
  [element state]
  (str "state." (:state-id (first (filter #(= (:id %) (:id element)) state))) " = action.payload;"))

; ドロップダウンリスト型のリデューサ実装
(defmethod reducer-code :select
  [element state]
  (str "state." (:state-id (first (filter #(= (:id %) (:id element)) state))) " = action.payload;"))

; ラジオボタン型のリデューサ実装
(defmethod reducer-code :radio
  [element state]
  (str "state." (:state-id (first (filter #(= (:id %) (:group element)) state))) " = action.payload;"))

; その他のリデューサ実装
(defmethod reducer-code :default
  [element state]
  "// TODO")

; アクション要素マップ
(defn action-map
  [defs state-map]
  (let [component-name  (get-in defs [:component :name])
        html-use-prefix (get-in defs [:component :html-use-prefix])
        html-elements_  (get-in defs [:component :html-elements])
        unit-elements   (filter #(nil? (:group %)) html-elements_)
        group-elements  (for [x (group-by :group (filter #(not (nil? (:group %))) html-elements_))]
                          (first (val x)))
        html-elements   (concat unit-elements group-elements)
        state           (:elements state-map)
        action-map      {:elements (for [x html-elements]
                                     (let [verb-string  (if (contains? x :action)
                                                          (:action x)
                                                          ((:type x) verb-map))
                                           name         (action-name x)]
                                       {:id             (action-id x)
                                        :id-descriptor  (camel-to-snake name)
                                        :id-value       (camel-to-snake
                                                          (if html-use-prefix
                                                            (prefixed-name component-name name)
                                                            name))
                                        :name           name
                                        :comment        (if-let [v (:name x)]
                                                          (str (abbreviate-comment v) (get verb-name-JP verb-string))
                                                          name)
                                        :action-value   (action-value x)
                                        :reducer-code    (reducer-code x state)
                                        }))}]
    action-map))

; ビュー実装
(defmulti view-code (fn [element state id-descriptor] (:type element)))

; テキスト型のビュー実装
(defmethod view-code :text
  [element state id-descriptor]
  (str
    "    let input = core.getElement("
    id-descriptor
    ");\n"
    "    input.value = state."
    (:state-id (first (filter #(= (:id %) (:id element)) state)))
    ";\n"
    "    return input;\n"))

; ドロップダウンリスト型のビュー実装
(defmethod view-code :select
  [element state id-descriptor]
  (str
    "    let input = core.getElement("
    id-descriptor
    ");\n"
    "    input.value = state."
    (:state-id (first (filter #(= (:id %) (:id element)) state)))
    ".value;\n"
    "    return input;\n"))

; ラジオボタン型のビュー実装
(defmethod view-code :radio
  [element state id-descriptor]
  (str
    "    util.setCheckedRadioValue(\""
    (:group element)
    "\", state."
    (:state-id (first (filter #(= (:id %) (:group element)) state)))
    ");\n"
    "    return input;\n"))

; ボタン型のビュー実装
(defmethod view-code :button
  [element state id-descriptor]
  (str
    "    let input = core.getElement("
    id-descriptor
    ");\n"
    "    return input;\n"))

; その他のビュー実装
(defmethod view-code :default
  [element state id-descriptor]
"    // TODO
    return null;")

; ビュー要素
(defn view-element
  [element state use-prefix component-name]
  (let [name (camel-to-pascal (if-let [v (:group element)] v (:id element)))
        id-descriptor (camel-to-snake (:id element))]
    {:id             (:id element)
     :group          (:group element)
     :id-descriptor  id-descriptor
     :id-value       (if use-prefix
                       (prefixed-name component-name (:id element))
                       (:id element))
     :name           name
     :event          ((:type element) event-map)
     :comment        (if-let [v (:name element)]
                       (str (abbreviate-comment v) ((:type element) type-name-JP))
                       name)
     :view-code      (view-code element state id-descriptor)
     }))

; ビュー要素マップ
(defn view-map
  [defs state-map]
  (let [component-name  (get-in defs [:component :name])
        html-use-prefix (get-in defs [:component :html-use-prefix])
        html-elements   (get-in defs [:component :html-elements])
        unit-elements   (filter #(nil? (:group %)) html-elements)
        group-elements  (for [x (group-by :group (filter #(not (nil? (:group %))) html-elements))]
                          (first (val x)))
        state           (:elements state-map)
        view-map        {:elements {:unit   (for [x html-elements]
                                              (view-element x state html-use-prefix component-name))
                                    :group  (for [x (concat unit-elements group-elements)]
                                              (view-element x state html-use-prefix component-name))
                                    }}]
    view-map))

; バインド変数マップ
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
            (assoc :view-elements (get-in view-map [:elements :unit]))
            (assoc :render-elements (get-in view-map [:elements :group])))
        (-> common-map
            (assoc :state-elements (:elements state-map))
            (assoc :view-elements (:elements view-map))
            (assoc :action-elements (:elements action-map))
            (assoc :bind-elements (for [x (:elements view-map) y (:elements action-map)
                                        :when (or
                                                (= (:id x) (:id y))
                                                (= (:group x) (:id y)))]
                                    {:bind-name (:name x)
                                     :action-name (:name y)})))))))

; ソースファイルを出力
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
