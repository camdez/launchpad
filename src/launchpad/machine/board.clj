(ns launchpad.machine.board
  (:require [launchpad.lp :as lp]))

(def board-width  lp/grid-width)
(def board-height lp/grid-height)

(defn on-board? [x y]
  (and (< -1 x board-width)
       (< -1 y board-height)))

(let [color+lp-color
      [[:off       lp/color-off]
       [:red-dim   lp/color-red-dim]
       [:red       lp/color-red]
       [:amber-dim lp/color-amber-dim]
       [:amber     lp/color-amber]
       [:green-dim lp/color-green-dim]
       [:green     lp/color-green]]]
  (def color
    (->> color+lp-color
         (map-indexed (fn [idx [kw _]] [kw idx]))
         (into {})))
  (def palette
    (map second color+lp-color)))

(defn off? [color-code]
  (= color-code (color :off)))

(def code->lp-color
  (partial nth palette))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Board Values

(def empty-board
  [[0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]])

(def init-board
  [[0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 6 6 0 0 0]
   [0 0 6 6 6 6 0 0]
   [0 0 6 6 6 6 0 0]
   [0 0 0 6 6 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]])

(def exit-board
  [[0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 2 2 0 0 0]
   [0 0 2 2 2 2 0 0]
   [0 0 2 2 2 2 0 0]
   [0 0 0 2 2 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]])

(def options-board
  [[2 2 2 0 0 0 0 0]
   [2 0 2 0 0 0 0 0]
   [2 0 2 0 0 0 0 0]
   [2 2 2 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   (as-> (range) $
     (take (count palette) $)
     (concat $ (repeat 0))
     (take 8 $)
     (vec $))])

(def bottle-board
  [[0 0 0 0 0 0 0 0]
   [0 0 2 2 2 2 0 0]
   [0 0 0 2 2 0 0 0]
   [0 0 2 1 1 2 0 0]
   [0 0 2 1 1 2 0 0]
   [0 0 2 1 1 2 0 0]
   [0 0 2 1 1 2 0 0]
   [0 0 0 2 2 0 0 0]])

;; Interestingly, it sounds like the device can natively draw text.
(def letter-boards
  {
   \A [[0 6 6 6 0 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 6 6 6 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [0 0 0 0 0 0 0 0]]
   \B [[6 6 6 6 0 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 6 6 6 0 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 6 6 6 0 0 0 0]
       [0 0 0 0 0 0 0 0]]
   \C [[0 6 6 6 0 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 0 0 0 6 0 0 0]
       [0 6 6 6 0 0 0 0]
       [0 0 0 0 0 0 0 0]]
   \D [[6 6 6 6 0 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 0 0 0 6 0 0 0]
       [6 6 6 6 0 0 0 0]
       [0 0 0 0 0 0 0 0]]
   \E [[6 6 6 6 6 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 6 6 6 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 6 6 6 6 0 0 0]
       [0 0 0 0 0 0 0 0]]
   \F [[6 6 6 6 6 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 6 6 6 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [6 0 0 0 0 0 0 0]
       [0 0 0 0 0 0 0 0]]
   })
