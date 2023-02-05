(ns launchpad.core
  (:require
   [overtone.midi
    :as
    midi
    :refer
    [midi-control midi-in midi-note-on midi-out]]
   [clojure.string :as str])
  (:import
   (javax.sound.midi MidiSystem Receiver ShortMessage)
   (uk.co.xfactorylibrarians.coremidi4j CoreMidiDeviceProvider)))

(comment
  ;; Use `Audio MIDI Setup.app` to create a virtual MIDI device.  Open
  ;; 'IAC Driver' and enable it.

  ;; Need to be in User 2 mode (top button 7) for sane addressing.


  ;; Apple's Midi implementation is broken and doesn't handle
  ;; timestamped MIDI messages, which Ableton requires.
  ;;
  ;; This library provides a working implementation:
  ;; https://github.com/DerekCook/CoreMidi4J

  (CoreMidiDeviceProvider/isLibraryLoaded)

  (def device-name "IAC Driver")

  (defn core-out [nom]
    (->> (CoreMidiDeviceProvider/getMidiDeviceInfo)
         seq
         (keep (fn [info]
                 (when (str/includes? (.getName info) nom)
                   (MidiSystem/getMidiDevice info))))
         (remove (fn [dev]
                   (zero? (.getMaxReceivers dev))))
         first)))


(comment
  (def virtual-out (midi-out "IAC Driver Bus 1"))
  (def virtual-in (midi-in "IAC Driver Bus 1"))
  (midi/midi-handle-events virtual-in
                      (fn [msg]
                        ;;(clojure.pprint/pprint msg)
                        ))

  (def major-scale [1 0 1 0 1 1 0 1 0 1 0 1])
  (def minor-scale [1 0 1 0 1 0 1 1 0 1 0 1])

  (defn scale-notes [scale notes]
    (->> notes
         (map vector (cycle scale))
         (keep (fn [[in? note]]
                 (when (pos? in?)
                   note)))))

  (let [dev (core-out device-name)
        _   (when-not (.isOpen dev)
              (.open dev))
        rec (.getReceiver dev)
        msg (doto (ShortMessage.)
              (.setMessage ShortMessage/NOTE_ON 0 60 127))]
    (.send ^Receiver rec msg (.getMicrosecondPosition dev)))

  (let [dev         (core-out device-name)
        _           (when-not (.isOpen dev)
                      (.open dev))
        rec         (.getReceiver dev)
        virtual-out {:device   dev
                     :receiver rec}
        vel         127
        dur         150
        channel     0]
    (doseq [note   (->> (range)
                        (drop 60)
                        (take (inc 24))
                        (scale-notes minor-scale))]
      (println "Playing" note "to virtual bus")
      (let [msg (ShortMessage.)
            ts  (-> virtual-out :device .getMicrosecondPosition)]
        (.setMessage msg ShortMessage/NOTE_ON channel note vel)
        (midi/midi-send-msg (:receiver virtual-out) msg ts))
      (Thread/sleep dur)
      (let [msg (ShortMessage.)
            ts  (-> virtual-out :device .getMicrosecondPosition)]
        (.setMessage msg ShortMessage/NOTE_OFF channel note 0)
        (midi/midi-send-msg (:receiver virtual-out) msg ts))
      (Thread/sleep (int (/ dur 2)))
      ))
  )

(comment
  (count (.getAvailableInstruments (MidiSystem/getSynthesizer)))

  (let [synth (MidiSystem/getSynthesizer)
        piano (aget (.getAvailableInstruments synth) 0)]
    (.open synth)
    ;;(.isSoundbankSupported synth (.getDefaultSoundbank synth))
    ;;(.loadAllInstruments synth (.getDefaultSoundbank synth))
    (.loadInstrument synth piano)
    (midi/midi-note {:receiver (.getReceiver synth)} 62 60 93)
    (Thread/sleep 1000)
    (.close synth)
    ))

(defn initialize []
  ;; TODO make these look dynamically bound. Or pass as args.
  (def lp-in (midi-in "Launchpad Mini"))
  (def lp-out (midi-out "Launchpad Mini")))

(comment
  (initialize))

(comment
  (midi/midi-handle-events lp-in
                      (fn [msg]
                        (prn msg)
                        #_(midi-note-on lp-out (:note msg) color-off (:channel msg))))

  (def lk-in (midi-in "Launchkey Mini MK3 MIDI Port"))

  (midi/midi-handle-events lk-in
                      (fn [{:keys [channel] :as msg}]
                          (when-not (= 8 channel)
                            (prn msg)
                            #_(midi-note-on lp-out (:note msg) color-red 0)))))

(comment
  (midi/midi-in)
  (midi/midi-sources)
  (midi/midi-sinks)
  (midi/midi-devices))

(defn coords->note [x y]
  {:pre [(< -1 x 9)
         (< -1 y 9)]}
  (+ (* y 16) x))

;; TODO: memoize this, or pre-compute
(defn note->coords [p]
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

;; Side effecting! Maybe put exclamation points on the names.
(defn led-on [x y color]
  (midi-note-on lp-out (coords->note x y) color))

(defn led-off [x y]
  (led-on x y color-off))

(defn all-on [color]
  (doseq [x (range 8)
          y (range 8)]
    (led-on x y color)))

(defn all-off []
  ;(all-leds-on color-off)
  (midi-control lp-out 0 0))

(comment
  (note->coords 59)
  (coords->note 4 5)
  (all-on color-red)
  (led-on 4 5 color-red)
  (all-off))

;;;;;

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

;; TODO: it's possible to set entire rows at once, or even all LEDs!
(defn draw-board [board]
  (dotimes [row-idx (count board)]
    (let [row (nth board row-idx)]
      (dotimes [col-idx (count row)]
        (led-on col-idx row-idx (code->color (nth row col-idx)))))))

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

(defn fade-step
  ([board]
   (fade-step board 2))
  ([board chance]
   (vec (map #(vec (map (fn [el]
                          (cond
                            (zero? el)                el
                            (zero? (rand-int chance)) (dec el)
                            :else                     el))
                        %))
             board))))

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

;;;

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Scenes

(def modes
  ;; Tap on any cell to change the color?
  {:paint   {:init     #(assoc % :color color-red)
             :on-tap   (fn [{:keys [color] :as state} x y]
                         (update-in state [:board y x]
                                    #(if (pos? %)
                                       0
                                       2)))
             :on-reset (fn [state]
                         (assoc state :board new-board))}
   ;; Don't really need `tick` here, this is just an experiment
   :gravity {:init     (fn [state]
                         (assoc state :tick 0))
             :on-reset (fn [state]
                         state)
             :on-tap   (fn [{:keys [color] :as state} x y]
                         (update-in state [:board y x]
                                    #(if (pos? %)
                                       0
                                       2)))
             :on-tick  (fn [state]
                         ;;(update state :tick inc)
                         (update state :board gravity-step))}
   :rain    {:on-tick (let [step (comp gravity-step fade-step rain-step)]
                        (fn [state]
                          (update state :board step)))}
   :eq      {:init    #(assoc % :tick-ms 600)
             :on-tick (fn [{:keys [peaks] :as state}]
                        (let [levels (->> (fn low-bias-rand []
                                            (let [x (rand-int 5)]
                                              (if (< x 4)
                                                x
                                                (+ x (rand-int 5)))))
                                          repeatedly
                                          (take 8)
                                          vec)
                              board  (for [y (range 8)]
                                       (vec
                                        (for [x (range 8)]
                                          (let [level     (nth levels x)
                                                logical-y (- 8 y)]
                                            (cond
                                              (>= logical-y level) 0
                                              (>  logical-y 5)     2 ; red
                                              (>  logical-y 2)     6 ; green
                                              :else                5 ; dim green
                                              )))))
                              peaks  (map (fn [level peak]
                                            (max level peak))
                                          levels
                                          (if peaks
                                            (map dec peaks)
                                            levels))
                              board  (vec
                                      (reduce (fn [b [x peak]]
                                                (let [logical-y (- 8 peak)]
                                                  (if (pos? peak)
                                                    (assoc-in b [logical-y x] 4)
                                                    b)))
                                              board
                                              (map-indexed vector peaks)))]
                          (assoc state
                                 :eq-levels levels
                                 :board     board
                                 :peaks     peaks)))}
   :exit    {:init (fn [{:keys [exit?] :as state}]
                     (deliver exit? true)
                     state)}})

(def mode-buttons
  [:paint
   :gravity          ; could be an option! could have an options mode!
   :rain
   :eq
   nil
   nil
   nil
   :exit])

(defn initial-state []
  {:board   new-board
   :tick-ms 200})

;;

;; TODO: we should probably pass `out` also.
;; TODO: could support down and up for taps
;; TODO: binary clock timer?  Or just block timer?  Hourglass? :)

;; TODO: I could globally track all keys held down.  That would be
;; neat.  Could also track the tick they went down and use that for
;; long presses.

;; TODO: could allow modes to run code when they're being exited, like
;; saving the buffer.

(defn handle-msg [state-atom {:keys [command note] :as msg}]
  (prn (dissoc msg :msg :device))       ; DEBUG: messages
  (when (= :note-off command)
    (cond
      ;; Mode buttons
      (#{8 24 40 56 72 88 104 120} note)
      (when-let [new-mode (let [idx (int (/ (- note 8) 16))]
                            (nth mode-buttons idx))]
        (let [{:keys [init on-reset]} (get modes new-mode)
              {:keys [mode]}          @state-atom]
          ;; TODO: Light up mode buttons
          (swap! state-atom
                 (fn select-mode [s]
                   (cond-> s
                     true
                     (assoc :mode new-mode)

                     (and init (not= new-mode mode))
                     init

                     (and on-reset (= new-mode mode))
                     on-reset)))))

      ;; Main grid
      (<= 0 note 119)
      (let [{:keys [mode]}   @state-atom
            {:keys [on-tap]} (get modes mode)]
        (when on-tap
          (let [[x y] (note->coords note)]
            (swap! state-atom
                   (fn [s]
                     (let [ret (on-tap s x y)]
                       (if (map? ret)
                         ret
                         s))))))))))

;; TODO: initial draw

(defn run [in]
  (let [mode  (first mode-buttons)
        exit? (promise)
        init  (or (get-in modes [mode :init])
                  identity)
        state (-> (initial-state)
                  (assoc :mode mode
                         :exit? exit?)
                  init
                  atom)
        _     (add-watch state :draw
                         (fn [_k _r old new]
                           (when (not= (dissoc old :tick) (dissoc new :tick))
                             (prn new)  ; DEBUG: state
                             (when (not= (:board old) (:board new))
                               ;; TODO: Optimize this to only send what changed
                               (draw-board (:board new))))))
        tickf (future
                (loop []
                  (let [{:keys [mode tick-ms]} @state]
                    (when-let [on-tick (get-in modes [mode :on-tick])]
                      (swap! state on-tick))
                    (Thread/sleep (or tick-ms 1000))
                    (when-not (realized? exit?)
                      (recur)))))]
    (try
      (midi/midi-handle-events in (partial handle-msg state))
      @exit?                            ; wait for exit signal
      (finally
        (.setReceiver (:transmitter in) nil)
        (println "Goodbye!")))
    @tickf                              ; check for exceptions
    @state))

;; How about at option key?? That would be cool!  Could also have
;; pages of tools.  Could even have a palette.

(comment
  (def f (future (run lp-in)))
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Unused

;;; Next steps:
;;; 1. Make a 2D vector representing the screen, add a function to draw it.
;;; 2. Start running functions over the vector. Cellular automata. Blocks that fall. Etc.
;;;    - Tap once for one color, tap again for another, etc.
;;;    - Growing green plants, with a monster running around eating them.
;;;    - Games

(defn bits
  "Returns the lowest eight bits of number `n` as a vector."
  [n]
  (mapv #(min (bit-and n %) 1)
        (take 8 (iterate (partial * 2) 1))))

(defn replace-row [board idx row]
  (update-in board [idx] (constantly row)))

(comment
  (defn write-string [str]
    (doseq [letter (seq str)]
      (when-let [board (letter-boards letter)]
        (reset! live-board board)
        (Thread/sleep 500)))))

(comment
  (write-string "DEADBEEF"))

(comment
  (loop []
    (let [now (java.util.Calendar/getInstance)]
      (reset! (nth time-atoms 0) (.get now java.util.Calendar/SECOND))
      (reset! (nth time-atoms 1) (.get now java.util.Calendar/MINUTE))
      (reset! (nth time-atoms 2) (.get now java.util.Calendar/HOUR_OF_DAY))
      (reset! (nth time-atoms 3) (.get now java.util.Calendar/DAY_OF_MONTH))
      (reset! (nth time-atoms 4) (inc (.get now java.util.Calendar/MONTH)))
      (reset! (nth time-atoms 5) (.get now java.util.Calendar/YEAR)))
    (Thread/sleep 1000)
    (recur)))


(comment
  (doseq [y (range 8)
          x (range 8)]
    (led-on x y color-green)
    (Thread/sleep 20)
    (led-off x y)))

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

(comment
  (wander 3 3))
