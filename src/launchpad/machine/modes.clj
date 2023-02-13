(ns launchpad.machine.modes
  {:clj-kondo/config '{:lint-as {launchpad.machine.modes/forv clojure.core/for}}}
  (:require [launchpad.machine.board :as b]))

;; TODO: pass this in.
(def device-name "IAC Driver")
(def iac-out (overtone.midi/midi-out device-name))

(defmacro forv
  {:style/indent 1}
  [bindings body]
  `(vec
    (for ~bindings
      ~body)))

(defn- paint [{:keys [color] :as state} x y]
  (update-in state [:board y x]
             #(if (pos? %)
                (b/color :off)
                color)))

(defn- lift-bstep
  "Convert a board step function to a step function.

  (board -> board) -> (state -> state)."
  [bstep-fn]
  (fn [state]
    (update state :board bstep-fn)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Step Functions

(defn- play-bottom-step [{:keys [board play-bottom] :as state}]
  (when play-bottom
    (doseq [[idx color] (map-indexed vector (last board))]
      (when-not (b/off? color)
        (overtone.midi/midi-note iac-out (+ idx 36) 80 500))))
  state)

(defn- fade-bstep
  ([board]
   (fade-bstep board 2))
  ([board chance]
   (mapv #(mapv (fn [el]
                  (cond
                    (b/off? el)               el
                    (zero? (rand-int chance)) (dec el)
                    :else                     el))
                %)
         board)))

(defn- gravity-bstep [board]
  (loop [b board
         x 0
         y 0]
    (if (>= x b/board-width)
      (if (>= y b/board-height)
        b
        (recur b 0 (inc y)))
      (recur (if (and (< (inc y) b/board-height)
                      (> (get-in board [y       x]) 0)
                      (= (get-in board [(inc y) x]) 0))
               (-> b
                   (update-in [y x]       (constantly 0))
                   (update-in [(inc y) x] (constantly (get-in board [y x]))))
               b)
             (inc x) y))))

(defn rain-bstep
  ([board] (rain-bstep board (/ 1 10)))
  ([board prob]
     (update-in board [0]
                (fn [row]
                  (mapv (fn [el]
                          (if (and (b/off? el) (< (rand) prob))
                            (dec (count b/palette))
                            el))
                        row)))))

(letfn [(neighbor-count [old-board x y]
          (->> (for [x-off [-1 0 1]
                     y-off [-1 0 1]
                     :when (not= 0 x-off y-off)]
                 [(+ x x-off) (+ y y-off)])
               (filter (fn [[x' y']]
                         (and (b/on-board? x' y')
                              (not (b/off? (get-in old-board [y' x']))))))
               count))]
  (defn life-bstep [board]
    (forv [y (range b/board-height)]
      (forv [x (range b/board-width)]
        (let [live? (not (b/off? (get-in board [y x])))
              n-cnt (neighbor-count board x y)]
          (b/color
           (cond
             (and live? (= 2 n-cnt))        :amber
             (and live? (= 3 n-cnt))        :red
             (and (not live?) (=  3 n-cnt)) :green
             :else                          :off)))))))

(defn eq-step [{:keys [peaks] :as state}]
  (let [levels (->> (fn low-bias-rand []
                      (let [x (rand-int 5)]
                        (if (< x 4)
                          x
                          (+ x (rand-int 5)))))
                    repeatedly
                    (take b/board-width)
                    vec)
        board  (forv [y (range b/board-height)]
                 (forv [x (range b/board-width)]
                   (let [level     (nth levels x)
                         logical-y (- b/board-height y)]
                     (b/color
                      (cond
                        (>= logical-y level) :off
                        (>  logical-y 5)     :red
                        (>  logical-y 2)     :green
                        :else                :green-dim)))))
        peaks  (map (fn [level peak]
                      (max level peak))
                    levels
                    (if peaks
                      (map dec peaks)
                      levels))
        board  (vec
                (reduce (fn [b [x peak]]
                          (let [logical-y (- b/board-height peak)]
                            (if (pos? peak)
                              (assoc-in b [logical-y x] 4)
                              b)))
                        board
                        (map-indexed vector peaks)))]
    (assoc state
           :eq-levels levels
           :board     board
           :peaks     peaks)))


(defn- synth-tap [{:keys [tick] :as state} x y]
  (-> state
      (assoc-in [:freqs x]
                (if (= (dec b/board-height) y)
                  nil
                  (- (dec b/board-height) y)))
      (assoc-in [:set-ticks x] tick)))

(defn- synth-pos
  "Given a note frequency (`freq`) set on a certain tick, `set-tick`,
  where in the column should we light up on tick `tick`?"
  [freq set-tick tick]
  (- freq (let [v (mod (- tick set-tick)
                       (* 2 freq))]
            (if (> v freq)
              (- freq (- v freq))
              v))))

(defn- synth-tick [{:keys [color freqs set-ticks tick] :as state}]
  (-> state
      (update :tick inc)
      (assoc :board b/empty-board)
      (update :board (fn [b]
                       (reduce (fn [b idx]
                                 (if-let [freq (nth freqs idx)]
                                   (assoc-in b
                                             [(- (dec b/board-height)
                                                 (synth-pos freq (nth set-ticks idx) tick))
                                              idx]
                                             color)
                                   b))
                               b
                               (range b/board-width))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Top-Level

(def modes
  {:paint   {:on-tap  paint
             :on-self (fn [{:keys [board old-board] :as state}]
                        (if (and old-board (= board b/empty-board)) ; undo
                          (assoc state
                                 :board old-board)
                          (assoc state  ; clear
                                 :old-board board
                                 :board b/empty-board)))}

   :gravity {:on-tap  paint
             :on-tick (lift-bstep gravity-bstep)}

   :rain    {:on-tick (comp play-bottom-step (lift-bstep (comp rain-bstep gravity-bstep fade-bstep)))}

   :life    {:on-tap  paint
             :on-tick (comp play-bottom-step (lift-bstep life-bstep))}

   :eq      {:on-tick eq-step}

   :synth   {:on-enter #(assoc %
                               :freqs     (vec (repeat b/board-width nil))
                               :set-ticks (vec (repeat b/board-width nil))
                               :tick      0)
             :on-tap   synth-tap
             :on-tick  (comp play-bottom-step synth-tick)
             :on-self  (fn [state]
                         (update state :tick-ms #(case %
                                                   500  250
                                                   250  125
                                                   500)))}

   :options {:on-enter (fn [state]
                         (assoc state
                                :restore (select-keys state [:board :tick-ms])
                                :board b/options-board
                                :tick 0
                                :tick-ms 500))
             :on-leave (fn [{:keys [restore] :as state}]
                         (-> state
                             (merge restore)
                             (dissoc :restore)))
             :on-tap   (fn [{:keys [board] :as state} x y]
                         (cond
                           (= 7 y)  ; color picker row
                           (assoc state :color (get-in board [y x]))

                           (= [1 1] [x y])
                           (update state :debug not)

                           (= [1 2] [x y])
                           (update state :play-bottom not)))
             :on-tick  (fn [{:keys [color debug play-bottom tick] :as state}]
                         (-> state
                             (update :tick inc)
                             (assoc :board
                                    (cond-> b/options-board
                                      (odd? tick)
                                      (assoc-in [7 color] 0)

                                      debug
                                      (assoc-in [1 1] (b/color :green))

                                      play-bottom
                                      (assoc-in [2 1] (b/color :green))))))}

   :exit    {:on-enter (fn [{:keys [exit?] :as state}]
                         (deliver exit? true)
                         state)}})

(def mode-buttons
  [:paint
   :gravity
   :rain
   :eq
   :life
   :synth
   :options
   :exit])
