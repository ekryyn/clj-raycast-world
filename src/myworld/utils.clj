(ns myworld.utils
  (:require [quil.core :as q]
            [myworld.engine :as engine :refer [UNIT]]
            ))


(defn create-dialogue [text]
  {:remaining-text text
   :display-text ""
   :last-frame 0})

(defn dialogue-step-letter
  [{:keys [text last-frame remaining-text] :as dialogue} t delta]
  (if (> (- t last-frame) delta)
    (let [[next-letter & xs] remaining-text]
      (-> dialogue
          (assoc :remaining-text xs)
          (update :display-text str next-letter)
          (assoc :last-frame t)
          ;#(if (empty? (:remaining-text %)) nil %)
        ))
    dialogue
    )
  )
