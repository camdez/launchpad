(ns demo1
  "Basic MIDI setup."
  (:require
   [clojure.pprint :as pprint]
   [launchpad.lp :as lp]
   [overtone.midi :as midi])
  (:import
   (javax.sound.midi Transmitter)))

(def lp-in  (midi/midi-in  lp/device-name))
(def lp-out (midi/midi-out lp/device-name))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MIDI In

(comment
  ;; To receive events
  (midi/midi-handle-events
   lp-in
   pprint/pprint)

  ;; To stop receiving events
  (.setReceiver ^Transmitter (:transmitter lp-in) nil))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MIDI Out

(comment
  (midi/midi-note-on lp-out (lp/coords->note 0 0) lp/color-green)
  (midi/midi-note-off lp-out (lp/coords->note 0 0))

  (midi/midi-note lp-out (lp/coords->note 0 0) lp/color-green 500)

  (doseq [y (range lp/grid-height)
          x (range lp/grid-width)]
    (midi/midi-note lp-out (lp/coords->note x y) lp/color-green 500)
    (Thread/sleep 50))

  (let [notes (for [y (range lp/grid-height)
                    x (range lp/grid-width)]
                (lp/coords->note x y))
        vels  (cycle [lp/color-red lp/color-amber lp/color-green])
        durs  (repeat 100)]
    (midi/midi-play lp-out notes vels durs)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; MIDI I/O

(comment
  ;; All together now...
  (midi/midi-handle-events
   lp-in
   (fn [{:keys [command note] :as _msg}]
     (case command
       :note-on  (midi/midi-note-on  lp-out note lp/color-red)
       :note-off (midi/midi-note-off lp-out note)
       nil)))

  ;; To stop receiving events
  (.setReceiver ^Transmitter (:transmitter lp-in) nil))
