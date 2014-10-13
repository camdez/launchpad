;; github.com/alda-lang/alda

(ns launchpad.core
  (:require [overtone.midi :refer [midi-control midi-note-on midi-in midi-out midi-handle-events]])
  (:gen-class))

(defn initialize []
  ;; TODO make these look dynamically bound. Or pass as args.
  (def lp-in (midi-in "Launchpad"))
  (def lp-out (midi-out "Launchpad")))

(initialize)

(defn coords->pos [x y]
  {:pre [(< -1 x 9)
         (< -1 y 9)]}
  (+ (* y 16) x))

(defn pos->coords [p]
  [(mod p 8)
   (int (/ p 16))])

(def color-off 12)
(def color-red-dim 13)
(def color-red 15)
(def color-amber-dim 29)
(def color-amber 63)
(def color-yellow 62)
(def color-green-dim 28)
(def color-green 60)

(defn led-on [x y color]
  (midi-note-on lp-out (coords->pos x y) color))

(defn led-off [x y]
  (led-on x y color-off))

(defn all-leds-on [color]
  (doseq [x (range 8)
          y (range 8)]
    (led-on x y color)))

(defn all-off []
  ;(all-leds-on color-off)
  (midi-control lp-out 0 0))

;;;;;

;; (doseq [y (range 8)
;;         x (range 8)]
;;   (led-on x y color-green)
;;   (Thread/sleep 20)
;;   (led-off x y))

(defn wander-step
  ([i] (wander-step i 0 7))
  ([i lower upper] (max lower (min upper (+ i (rand-nth [-1 0 1]))))))

(defn wander [x y]
  (led-on x y color-amber)
  (Thread/sleep 100)
  (let [new-x (wander-step x)
        new-y (wander-step y)]
    (led-off x y)
    (recur new-x new-y)))

;; (defn event-handler [{:keys [note velocity]}]
;;   (midi-note-on lp-out note
;;                 (if (> velocity 0) color-amber color-green)))

;; (initialize)
;; (midi-handle-events lp-in event-handler)

;;; Next steps:
;;; 1. Make a 2D vector representing the screen, add a function to draw it.
;;; 2. Start running functions over the vector. Cellular automata. Blocks that fall. Etc.
;;;    - Tap once for one color, tap again for another, etc.
;;;    - Growing green plants, with a monster running around eating them.
;;;    - Games

(def color-map [color-off color-red-dim color-red color-amber-dim color-amber color-green-dim color-green])
(def code->color
  (partial nth color-map))

(def board
  [[1 0 0 0 0 0 0 1]
   [1 0 0 1 1 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 1 1 0 0 0]
   [0 0 0 0 1 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]])

(def new-board
  [[0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]
   [0 0 0 0 0 0 0 0]])

(defn draw-board [board]
  (dotimes [row-idx (count board)]
    (let [row (nth board row-idx)]
      (dotimes [col-idx (count row)]
        (led-on col-idx row-idx (code->color (nth row col-idx)))))))

(defn gravity-step-old [board]
  (loop [b board
         x 0
         y 0]
    (if (> x 7)
      (if (> y 7)
        b
        (recur b 0 (inc y)))
      (recur (if (and (< y 7)
                      (> (get-in b [y x]) 0)
                      (= (get-in b [(inc y) x]) 0))
               (-> b
                   (update-in [y x]       (constantly 0))
                   (update-in [(inc y) x] (constantly 1)))
               b)
             (inc x) y))))

(defn gravity-step [board]
  (loop [b board
         x 0
         y 0]
    (if (> x 7)
      (if (> y 7)
        b
        (recur b 0 (inc y)))
      (recur (if (and (< (inc y) 8)
                      (> (get-in board [y       x]) 0)
                      (= (get-in board [(inc y) x]) 0))
               (-> b
                   (update-in [y x]       (constantly 0))
                   (update-in [(inc y) x] (constantly (get-in board [y x]))))
               b)
             (inc x) y))))

(defn fade-step [board]
  (vec (map #(vec (map (fn [el] (max 0 (dec el))) %))
        board)))

(defn rain-step
  ([board] (rain-step board (/ 1 10)))
  ([board prob]
     (update-in board [0]
                (fn [v]
                  (vec (map (fn [el]
                              (if (< (rand) prob)
                                (dec (count color-map))
                                el))
                            v))))))

(defn run
  ([step board] (run step board nil))
  ([step board prev-board]
     (draw-board board)
     (let [new-board (step board)]
       (when-not (= board new-board)
         (Thread/sleep 100)
         (recur step new-board board)))))

(def live-board (atom new-board))

(defn run-live [step]
  (swap! live-board step)
  (Thread/sleep 100)
  (recur step))

;;;

;(def monster-pos )

;;;

;(reset! live-board board)

(add-watch live-board :draw
           (fn [key ref old new]
             (draw-board new)))

;(remove-watch live-board :draw)

(def draw-color (.indexOf color-map color-green-dim))

(def last-message (atom nil))
(def top-button-actions
  {
   0 (fn [] (def draw-color (.indexOf color-map color-green)))
   1 (fn [] (def draw-color (.indexOf color-map color-red)))
   2 (fn [] (def draw-color (.indexOf color-map color-amber)))
   7 (fn [] (reset! live-board new-board))
   })

(defn event-handler [{:keys [note velocity] :as msg}]
  (swap! last-message (constantly msg))
  (when (> velocity 0)
    (cond
     (< 103 note 112) ((top-button-actions (- note 104)))
     :else (swap! live-board update-in (reverse (pos->coords note)) #(if (> % 0) 0 draw-color)))))

(midi-handle-events lp-in event-handler)

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

(defn bits [n]
  "Returns the lowest eight bits of number `n` as a vector."
  (mapv #(min (bit-and n %) 1)
        (take 8 (iterate (partial * 2) 1))))

(defn replace-row [board idx row]
  (update-in board [idx] (constantly row)))

(def watch-num (atom 0))
(def time-atoms (map atom (take 6 (repeat 0))))

(dotimes [i (count time-atoms)]
  (add-watch (nth time-atoms i) :draw
             (fn [key ref old new]
               (swap! live-board replace-row (- 7 i) (bits new)))))

(add-watch watch-num :draw
           (fn [key ref old new]
             (swap! live-board replace-row 7 (bits new))))

(defn write-string [str]
  (doseq [letter (seq str)]
    (when-let [board (letter-boards letter)]
      (reset! live-board board)
      (Thread/sleep 500))))

;; (loop []
;;     (let [now (java.util.Calendar/getInstance)]
;;       (reset! (nth time-atoms 0) (.get now java.util.Calendar/SECOND))
;;       (reset! (nth time-atoms 1) (.get now java.util.Calendar/MINUTE))
;;       (reset! (nth time-atoms 2) (.get now java.util.Calendar/HOUR_OF_DAY))
;;       (reset! (nth time-atoms 3) (.get now java.util.Calendar/DAY_OF_MONTH))
;;       (reset! (nth time-atoms 4) (inc (.get now java.util.Calendar/MONTH)))
;;       (reset! (nth time-atoms 5) (.get now java.util.Calendar/YEAR)))
;;     (Thread/sleep 1000)
;;     (recur))

;;; HACK. Stateful.
(defn occasionally! [f interval]
  (let [state (atom (cycle (conj (repeat (dec interval) identity) f)))]
    (fn [x]
      (let [g (first @state)]
        (swap! state rest)
        (g x)))))

;; > (map (occasionally! inc 5) (repeat 10 0))
;; (1 0 0 0 0 1 0 0 0 0)

;;; This would work but `identity' can't handle multiple args! Makes
;;; sense, I suppose. Mapping functions need to combine two values.
#_(defn occasionally [f i]
    (let [state (atom (cycle (conj (repeat (dec i) identity) f)))]
      (fn [& args]         ; would prefer not to limit args, come back
        (let [g (first @state)]
          (swap! state rest)
          (apply g args)))))

(defn chimera [fseq]
  (let [state (atom fseq)]
    (fn [& args]
      (let [[g & _] @state]
        (swap! state rest)
        (apply g args)))))

;; (defn occasionally [fn interval]
;;   (with-each (cycle (list* fn (repeat (dec interval) identity)))))

(map (chimera (cycle [inc dec])) (repeat 10 0))

(defn holla-holla-get-dolla [f1 f2]
  (fn [& args]
    (f1 (map f2 args))))

;; (def h inc)
;; (def g reverse)
;; (def f (holla-holla-get-dolla g h))
;; (f 1 2 3)
;; => (4 3 2)
