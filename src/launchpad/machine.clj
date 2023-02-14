(ns launchpad.machine
  (:require
   [launchpad.lp :as lp]
   [launchpad.machine.board :as b]
   [launchpad.machine.modes :as modes]
   [overtone.midi :as midi]))

;; TODO: it's possible to set entire rows at once, or even all LEDs!
(defn- draw-board! [midi-out board]
  (dotimes [row-idx (count board)]
    (let [row (nth board row-idx)]
      (dotimes [col-idx (count row)]
        (lp/led-on! midi-out col-idx row-idx (b/code->lp-color (nth row col-idx)))))))

(defn- draw-mode-buttons! [midi-out mode modes]
  (doseq [[pos-mode note] (map vector modes [8 24 40 56 72 88 104 120])]
    (midi/midi-note-on midi-out note
                       (if (= mode pos-mode)
                         (b/color :red)
                         (b/color :off)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn- handle-msg [state-atom {:keys [command note] :as msg}]
  (when (:debug @state-atom)
    (prn (dissoc msg :msg :device)))
  (when (= :note-off command)
    (cond
      ;; Mode buttons
      (lp/right-button-note? note)
      (when-let [new-mode (->> note
                               lp/note->right-button-num
                               (nth modes/mode-buttons))]
        (let [{:keys [on-enter on-self]} (get modes/modes new-mode)
              {:keys [mode]}             @state-atom
              {:keys [on-leave]}         (get modes/modes mode)
              mode-changed?              (not= new-mode mode)]
          (swap! state-atom
                 (fn select-mode [s]
                   (cond-> s
                     (and on-leave mode-changed?)
                     on-leave

                     true
                     (assoc :mode new-mode) ; TODO: not needed if no mode change!

                     (and on-enter mode-changed?)
                     on-enter

                     (and on-self (not mode-changed?))
                     on-self)))))

      ;; Main grid
      (lp/main-grid-note? note)
      (let [{:keys [mode]}   @state-atom ; might not want to deref here, could do it all in the `swap!`
            {:keys [on-tap]} (get modes/modes mode)]
        (when on-tap
          (let [[x y] (lp/note->coords note)]
            (swap! state-atom
                   (fn [s]
                     (let [ret (on-tap s x y)]
                       (if (map? ret)
                         ret
                         s))))))))))

(defn run [lp-in lp-out]
  (let [exit? (promise)
        state (atom nil)]
    (add-watch state :redraw
               (fn [_k _r old new]
                 (when (not= (dissoc old :tick) (dissoc new :tick))
                   (when (:debug new)
                     (prn new))
                   (when (not= (:board old) (:board new))
                     (draw-board! lp-out (:board new)))
                   (when (not= (:mode old) (:mode new))
                     (draw-mode-buttons! lp-out (:mode new) modes/mode-buttons)))))
    (reset! state
            {:board   b/init-board
             :color   2
             :exit?   exit?
             :mode    (first modes/mode-buttons)
             :tick-ms 200})
    (future
      (try
        (loop []
          (let [{:keys [mode tick-ms]} @state]
            (when-let [on-tick (get-in modes/modes [mode :on-tick])]
              (swap! state on-tick))
            (Thread/sleep (or tick-ms 1000))
            (when-not (realized? exit?)
              (recur))))
        (catch Exception ex
          (prn ex))))
    (try
      (midi/midi-handle-events lp-in (partial handle-msg state))
      @exit?                            ; wait for exit signal
      (swap! state #(assoc % :board b/exit-board))
      (finally
        (.setReceiver (:transmitter lp-in) nil)
        (println "Goodbye!")))
    @state))
