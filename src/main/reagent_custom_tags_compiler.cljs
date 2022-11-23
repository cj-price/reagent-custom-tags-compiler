(ns reagent-custom-tags-compiler
  "A compiler for reagent that uses `:parse-tag` to allow for custom tags using your own
  reagent components."
  (:require
   [clojure.string :as str]
   [goog.object :as gobj]
   [reagent.core :as r]
   [reagent.dom :as rd]
   [reagent.impl.template :as impl]))

(defn- base []
  [:x#id.hello.world "Did this work?. "
   [:span.really-worked ":)"
    [:x.see]]])

(defn- x [props & children]
  (print props)
  (into
   [:span (merge props {:style {:background-color "blue"
                                :color            "white"}})
    "Yes, yes it did. "]
   children))

(defn- handle-custom-tag
  "Given a tag found in `custom-tags`, take the `parsed-tag` and set its tag to the custom tag.
  Reagent will render the component instead of a native html element."
  [parsed-tag custom-tags]
  (if-let [custom-tag (get custom-tags (gobj/get parsed-tag "tag"))]
    (do (gobj/set parsed-tag "tag" custom-tag)
        parsed-tag)
    parsed-tag))

(defn- cached-parse
  "Very similar to `impl/cached-parse`. For custom tags, set custom tag into `->HiccupTag`
  from `impl/parse-tag`."
  [this tag-name _ custom-tags]
  (if-some [cached (impl/cache-get impl/tag-name-cache tag-name)]
    cached
    (let [tag (handle-custom-tag (impl/parse-tag tag-name) custom-tags)]
      (gobj/set impl/tag-name-cache tag-name tag)
      tag)))

(defn- reactify-wrapper
  "Default reactify-wrapper. To my knowledge, `:parse-tag` can only handle native components
  so we must reactify reagent components. This function only handles reagent->react but
  `compiler-with-custom-tags` can be provided with a custom wrapper to handle other cases.

  This function takes the liberty to split `className` and inject the result into the `:class` prop.
  It also separates `children` from the `props` argument.

  In the end, your reagent components should have a declaration akin to react components.
  e.g. `(defn my-comp [props & children] ...)`"
  [reagent-component]
  (r/reactify-component
   (fn wrapped [{:keys [children className] :as props}]
     (let [props (-> props
                     (dissoc :children)
                     (dissoc :className)
                     (assoc :class (str/split className #" ")))]
       (into [reagent-component props] children)))))

(defn compiler-with-custom-tags
  "Creates a reagent compiler with `:parse-tag` set to a map of tag -> reagent component.
  (or any other type that a custom `reactify-wrapper` handle.
  e.g. `(compiler-with-custom-tags {:navigation navigation-menu})`

  The compiler can be used in a reagent render.
  e.g. (reagent-dom/render
         [:custom-tag]
         (.getElementById js/document \"app\")
         (compiler-with-custom-tags {:custom-tag my-comp}))"
  ([custom-tags] (compiler-with-custom-tags custom-tags reactify-wrapper))
  ([custom-tags reactify-wrapper]
   (let [custom-tags (reduce-kv
                      (fn [acc k v]
                        (assoc acc (name k) (reactify-wrapper v)))
                      {} custom-tags)]
     (impl/create-compiler
      {:parse-tag
       (fn [this tag-name _]
         (cached-parse this tag-name _ custom-tags))}))))

(defn ^:dev/after-load init []
  (rd/render
   [base]
   (.getElementById js/document "main")
   (compiler-with-custom-tags
    {:x x})))
