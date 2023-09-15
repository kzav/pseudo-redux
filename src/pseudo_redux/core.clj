(ns pseudo-redux.core
  (:require [clojure.string :as s]
            [clojure.java.io :as io]
            [clojure.edn :as edn]
            [clostache.parser :as parser])
  (:import (java.io PushbackReader))
  (:gen-class))

; 構成情報
(def config (atom {}))

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
  (str (type (:verb-map @config)) (camel-to-pascal id)))

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
  (:id element))

; 状態初期値
(defmulti state-value (fn [element] (:type element)))

; テキスト型の状態初期値
(defmethod state-value :text
  [element]
  "\"\"")

; チェックボックス型の状態初期値
(defmethod state-value :check
  [element]
  "false")

; ラジオボタン型の状態初期値
(defmethod state-value :radio
  [element]
  "\"\"")

; その他の状態初期値
(defmethod state-value :default
  [element]
  "null")

; 単体要素状態
(defmulti unit-state (fn [element] (:type element)))

; テーブル型の単体要素状態
(defmethod unit-state :table
  [element]
  [
   {:id           (:id element)
    :state-id     (state-id element)
    :state-value  "[]"
    }
   {:id           (:id element)
    :state-id     (str "selected" (camel-to-pascal (:id element)) "Row")
    :state-value  (state-value element)
    }
   ]
  )

; その他の単体要素状態
(defmethod unit-state :default
  [element]
  {:id           (:id element)
   :state-id     (state-id element)
   :state-value  (state-value element)
   })

; 状態要素マップ
(defn state-map
  [defs]
  (let [html-elements   (get-in defs [:component :html-elements])
        unit-elements   (filter #(nil? (:group %)) html-elements)
        group-elements  (filter #(not (nil? (:group %))) html-elements)
        unit-state      (flatten
                          (for [x unit-elements
                                :when (some #(= (:type x) %) [:text :select :check :table])]
                            (unit-state x)))
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
  (get-in @config [:action-value :text]))

; ドロップダウンリスト型のアクション取得値
(defmethod action-value :select
  [element]
  (get-in @config [:action-value :select]))

; チェックボックス型のアクション取得値
(defmethod action-value :check
  [element]
  (get-in @config [:action-value :check]))

; ラジオボタン型のアクション取得値
(defmethod action-value :radio
  [element]
  (get-in @config [:action-value :radio]))

; その他のアクション取得値
(defmethod action-value :default
  [element]
  (get-in @config [:action-value :default]))

; リデューサ実装
(defmulti reducer-code (fn [element state] (:type element)))

; テキスト型のリデューサ実装
(defmethod reducer-code :text
  [element state]
  (let [code (get-in @config [:reducer-code :text])
        state-id (:state-id (first (filter #(= (:id %) (:id element)) state)))]
    (-> code
        (s/replace "{{state-id}}" state-id))))

; ドロップダウンリスト型のリデューサ実装
(defmethod reducer-code :select
  [element state]
  (let [code (get-in @config [:reducer-code :select])
        state-id (:state-id (first (filter #(= (:id %) (:id element)) state)))]
    (-> code
        (s/replace "{{state-id}}" state-id))))

; チェックボックス型のリデューサ実装
(defmethod reducer-code :check
  [element state]
  (let [code (get-in @config [:reducer-code :check])
        state-id (:state-id (first (filter #(= (:id %) (:id element)) state)))]
    (-> code
        (s/replace "{{state-id}}" state-id))))

; ラジオボタン型のリデューサ実装
(defmethod reducer-code :radio
  [element state]
  (let [code (get-in @config [:reducer-code :radio])
        state-id (:state-id (first (filter #(= (:id %) (:group element)) state)))]
    (-> code
        (s/replace "{{state-id}}" state-id))))

; その他のリデューサ実装
(defmethod reducer-code :default
  [element state]
  (get-in @config [:reducer-code :default]))

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
                                                          ((:type x) (:verb-map @config)))
                                           name         (action-name x)]
                                       {:id             (action-id x)
                                        :id-descriptor  (camel-to-snake name)
                                        :id-value       (camel-to-snake
                                                          (if html-use-prefix
                                                            (prefixed-name component-name name)
                                                            name))
                                        :name           name
                                        :comment        (if-let [v (:name x)]
                                                          (str (abbreviate-comment v) (get (:verb-name-JP @config) verb-string))
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
  (let [code (get-in @config [:view-code :text])
        state-id (:state-id (first (filter #(= (:id %) (:id element)) state)))]
    (-> code
        (s/replace "{{id-descriptor}}" id-descriptor)
        (s/replace "{{state-id}}" state-id))))

; ドロップダウンリスト型のビュー実装
(defmethod view-code :select
  [element state id-descriptor]
  (let [code (get-in @config [:view-code :select])
        state-id (:state-id (first (filter #(= (:id %) (:id element)) state)))]
    (-> code
        (s/replace "{{id-descriptor}}" id-descriptor)
        (s/replace "{{state-id}}" state-id))))

; チェックボックス型のビュー実装
(defmethod view-code :check
  [element state id-descriptor]
  (let [code (get-in @config [:view-code :check])
        state-id (:state-id (first (filter #(= (:id %) (:id element)) state)))]
    (-> code
        (s/replace "{{id-descriptor}}" id-descriptor)
        (s/replace "{{state-id}}" state-id))))

; ラジオボタン型のビュー実装
(defmethod view-code :radio
  [element state id-descriptor]
  (let [code (get-in @config [:view-code :radio])
        group (:group element)
        state-id (:state-id (first (filter #(= (:id %) (:group element)) state)))]
    (-> code
        (s/replace "{{group}}" group)
        (s/replace "{{state-id}}" state-id))))

; ボタン型のビュー実装
(defmethod view-code :button
  [element state id-descriptor]
  (let [code (get-in @config [:view-code :button])]
    (-> code
        (s/replace "{{id-descriptor}}" id-descriptor))))

; テーブル型のビュー実装
(defmethod view-code :table
  [element state id-descriptor]
  (let [code (get-in @config [:view-code :table])
        state-id (:state-id (first (filter #(= (:id %) (:id element)) state)))
        lower-id (s/lower-case (:id element))]
    (-> code
        (s/replace "{{id-descriptor}}" id-descriptor)
        (s/replace "{{state-id}}" state-id)
        (s/replace "{{lower-id}}" lower-id))))

; その他のビュー実装
(defmethod view-code :default
  [element state id-descriptor]
  (get-in @config [:view-code :default]))

; バインド実装
(defmulti bind-code (fn [element id-descriptor name event] (:type element)))

; テーブル型のバインド実装
(defmethod bind-code :table
  [element id-descriptor name event]
  (let [code (get-in @config [:bind-code :table])
        lower-name (s/lower-case name)]
    (-> code
        (s/replace "{{name}}" name)
        (s/replace "{{lower-name}}" lower-name))))

; その他のバインド実装
(defmethod bind-code :default
  [element id-descriptor name event]
  (let [code (get-in @config [:bind-code :default])]
    (-> code
        (s/replace "{{name}}" name)
        (s/replace "{{id-descriptor}}" id-descriptor)
        (s/replace "{{event}}" event))))

; ビュー要素
(defn view-element
  [kw element state use-prefix component-name]
  (let [is-group (and (= kw :group)
                      (not (nil? (:group element))))
        name (camel-to-pascal (if is-group (:group element) (:id element)))
        id-descriptor (camel-to-snake (:id element))
        event ((:type element) (:event-map @config))]
    {:type          (:type element)
     :id            (:id element)
     :group         (:group element)
     :id-descriptor id-descriptor
     :id-value      (if use-prefix
                      (prefixed-name component-name (:id element))
                      (:id element))
     :name          name
     :event         event
     :comment       (if-let [v (:name element)]
                      (str (if is-group (abbreviate-comment v) v) ((:type element) (:type-name-JP @config)))
                      name)
     :view-code     (view-code element state (str "ID_" id-descriptor))
     :bind-code     (bind-code element id-descriptor name event)
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
                                              (view-element :unit x state html-use-prefix component-name))
                                    :group  (for [x (concat unit-elements group-elements)]
                                              (view-element :group x state html-use-prefix component-name))
                                    }}]
    view-map))

; バインド要素
(defmulti bind-element (fn [element action] (:type element)))

; その他のバインド要素
(defmethod bind-element :table
  [element action]
  {:bind-name (str (:name element) "RowSelector")
   :action-name (:name action)})

; その他のバインド要素
(defmethod bind-element :default
  [element action]
  {:bind-name (:name element)
   :action-name (:name action)})

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
            (assoc :view-elements (get-in view-map [:elements :unit]))
            (assoc :action-elements (:elements action-map))
            (assoc :bind-elements (for [x (get-in view-map [:elements :unit]) y (:elements action-map)
                                        :when (or
                                                (= (:id x) (:id y))
                                                (= (:group x) (:id y)))]
                                    (bind-element x y)
                                    )))))))

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
    (reset! config (read-def "config.edn"))
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
