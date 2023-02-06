(ns demo3
  "Running the machine with the LaunchPad."
  (:require
   [launchpad.lp :as lp]
   [launchpad.machine :as machine]
   [overtone.midi :as midi]))

(def lp-in  (midi/midi-in  lp/device-name))
(def lp-out (midi/midi-out lp/device-name))

(comment
  (def f (future (machine/run lp-in lp-out)))

  (midi/midi-devices) ; all devices
  (midi/midi-sources) ; only *in* devices
  (midi/midi-sinks)   ; only *out* devices

  (midi/midi-in)      ; convenience interface for *in*
  (midi/midi-out))    ; convenience interface for *out*
