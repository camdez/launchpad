(ns demo2
  (:require
   [overtone.midi :as midi])
  (:import
   (javax.sound.midi MidiDevice ShortMessage)
   (uk.co.xfactorylibrarians.coremidi4j CoreMidiDeviceProvider)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hack `overtone.midi` to send timestamps
;;;
;;; Apple's Midi implementation is broken and doesn't handle
;;; timestamped MIDI messages, which Ableton requires.
;;;
;;; This library provides a working implementation:
;;; https://github.com/DerekCook/CoreMidi4J

(assert (CoreMidiDeviceProvider/isLibraryLoaded))

(alter-var-root
 (var midi/midi-note-on)
 (constantly
  (fn
    ([sink note-num vel]
     (midi/midi-note-on sink note-num vel 0))
    ([sink note-num vel channel]
     (let [on-msg (ShortMessage.)
           ts     (.getMicrosecondPosition ^MidiDevice (:device sink))]
       (.setMessage on-msg ShortMessage/NOTE_ON channel note-num vel)
       (midi/midi-send-msg (:receiver sink) on-msg ts))))))

(alter-var-root
 (var midi/midi-note-off)
 (constantly
  (fn
    ([sink note-num]
     (midi/midi-note-off sink note-num 0))
    ([sink note-num channel]
     (let [off-msg (ShortMessage.)
           ts      (.getMicrosecondPosition ^MidiDevice (:device sink))]
       (.setMessage off-msg ShortMessage/NOTE_OFF channel note-num 0)
       (midi/midi-send-msg (:receiver sink) off-msg ts))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Send Notes to Virtual Bus
;;;
;;; Use `Audio MIDI Setup.app` to create a virtual MIDI device.  Open
;;; 'IAC Driver' and enable it.

(def device-name "IAC Driver")
(def iac-out (midi/midi-out device-name))

(def major-scale [1 0 1 0 1 1 0 1 0 1 0 1])
(def minor-scale [1 0 1 0 1 0 1 1 0 1 0 1])

(defn- scale-notes [scale notes]
  (->> notes
       (map vector (cycle scale))
       (keep (fn [[in? note]]
               (when (pos? in?)
                 note)))))

(comment
  (midi/midi-note iac-out 60 80 500)

  (let [notes (->> (range)
                 (drop 60)
                 (take (inc 24))
                 (scale-notes major-scale))
      vels  (repeat 80)
      durs  (repeat 300)]
  (midi/midi-play iac-out notes vels durs)))
