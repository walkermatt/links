(ns ^:figwheel-always links.core
  (:require
    [reagent.core :as reagent :refer [atom]]
    [ajax.core :refer [GET]]))

(enable-console-print!)

; State

(defonce state
  (atom
    {:links [{:title "Loading..." :url "http://example.com" :tags ["loading"] :add_date 0}]
     :tags [["loading" 1]]
     :tag nil}))

(defn load-links! [links]
  (let [tags (sort (frequencies (flatten (map #(:tags %1) links))))]
    (swap! state assoc :links links)
    (swap! state assoc :tags tags)))

(defn filter-by-tag! [tag]
  (swap! state assoc :tag tag))

; AJAX

(defn get-links [handler]
  (GET "links.json" {:handler handler :response-format :json :keywords? true}))

; Regular functions

(defn truncate-str [s n]
  (let [s (clojure.string/trim s)
        p (re-pattern (str ".{1," n "}\\w+"))]
    (if (< (count s) n) s
      (str (.match s p) "..."))))

(defn parse-timestamp [unix]
  (js/Date. (* 1000 unix)))

(defn format-date [d]
  (.toISOString d))

(defonce url-parser (.createElement js/document "a"))

(defn get-domain [url]
  (set! (.-href url-parser) url)
  (.-hostname url-parser))

; Components

(defn tag-option [tag]
  (let [[name freq] tag
        label (if (nil? freq) name (str name " (" freq ")"))]
    [:option {:value name} label]))

(defn filter-on-tag [state]
  (let [tag (:tag state)
        default-tag ["select..." nil]
        value (if (nil? tag) (first default-tag) tag)]
    [:div
     [:label {:for "tag"} "tag: "]
     [:select#tag
      {:on-change #(let [value (.-target.value %)]
                     (filter-by-tag! (if (= (last default-tag) value) nil value)))
       :value value}
      (for [t (into [default-tag] (:tags state))]
        ^{:key t} [tag-option t])]
     (when (not (nil? tag))
       [:button {:on-click #(filter-by-tag! nil)} "clear"])]))

(defn link-tags [tags]
  [:div.tags
   (for [tag tags]
     ^{:key tag} [:button {:on-click #(filter-by-tag! tag)} tag])])

(defn link-detail [link]
  [:div.link-detail
   [:a.title {:href (:url link)}
    [:img {:src (str "https://s2.googleusercontent.com/s2/favicons?domain=" (get-domain (:url link)))}]
    (truncate-str (:title link) 40)]
   [:a.url {:href (:url link)} (truncate-str (:url link) 100)]
   [:time.date (format-date (parse-timestamp (:add_date link)))]
   [link-tags (:tags link)]])

(defn link-list [links]
  [:ul.link-list
   (doall
     (for [link links]
       ^{:key (:url link)} [:li [link-detail link]]))])

(defn app []
  (let [s @state]
    [:div
     [filter-on-tag s]
     [link-list
      (if (nil? (:tag s))
        (:links s)
        (filter #(some (partial = (:tag s)) (:tags %)) (:links s)))]]))

; init

(defn init []
  (get-links load-links!)
  (reagent/render-component [app]
                            (.getElementById js/document "app")))

(init)
