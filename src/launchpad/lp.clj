(ns launchpad.lp
  (:require [overtone.midi :as midi]))

;; Need to be in User 2 mode (top button 7) on the LaunchPad for
;; sane addressing.  If the `init-board` doesn't look like a
;; green "circle", then you're in the wrong mode.

(def device-name "Launchpad Mini")

(def color-off 12)
(def color-red-dim 13)
(def color-red 15)
(def color-amber-dim 29)
(def color-amber 63)
(def color-yellow 62)
(def color-green-dim 28)
(def color-green 60)

(def grid-width  8)
(def grid-height 8)

(def internal-row-offset 16)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn coords->note [x y]
  {:pre [(<= 0 x grid-width)
         (<= 0 y grid-height)]}
  (+ (* y internal-row-offset) x))

;; TODO: memoize this, or pre-compute
(defn note->coords [note]
  [(mod note grid-width)
   (int (/ note internal-row-offset))])

;;;

(defn main-grid-note? [note]
  (<= 0 note 119))

(def right-button-note? #{8 24 40 56 72 88 104 120})

(defn note->right-button-num [note]
  (int (/ (- note grid-width) internal-row-offset)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn led-on! [lp-out x y color]
  (midi/midi-note-on lp-out (coords->note x y) color))

(defn led-off! [lp-out x y]
  (led-on! lp-out x y color-off))

(defn all-on! [lp-out color]
  (doseq [x (range 8)
          y (range 8)]
    (led-on! lp-out x y color)))

(defn all-off! [lp-out]
  (midi/midi-control lp-out 0 0))
