# launchpad

Experiments in talking to the Novation Launchpad (midi controller) and
Ableton Live (DAW) from Clojure for music, [blinkenlights][], and fun.

## Introduction

A while back, I got excited about the potential to use the [Novation
Launchpad][lp] as both an input and output device.  With its 8x8
(main) grid of multi-color LED buttons, it can function as a
low-resolution display, or interactive surface.

The Launchpad is two MIDI devices in one--one MIDI input device
sending (presumably) notes to be played, and an output device where
the "note" pitches in the MIDI messages actually indicate which
buttons to light up, and what color the lights should be.

In this project, I basically took this concept to its limits, first
demonstrating basic communications (`demos/demo1.clj`), then showing
how to create sound via a virtual MIDI device routed to a DAW
(`demos/demo2.clj`), and finally contructing a framework for making AV
demos (à la [the demoscene][demoscene]), a bit like a simple game
engine (`demos/demo3.clj`).

The best introduction to all of this is my talk, [Maps, MIDI, &
Melodies: Multi-Modal Melodic Fun with Clojure][mmm-talk].

## Basic Usage

Open the files in `demo/` and evaluate them in your REPL.

## Status

This project is somewhere between a proof-of-concept and a demo.  It's
not ready to be used as a library.

Notably, there's some side effecting in the engine that I crammed in a
the last minute for audio output that I intend to remove. 

## Documentation

No proper documentation at this time.  Check out [the
video][mmm-talk].

Your Launchpad should be in mode 7 ("user mode 2").  Select this using
the top row of buttons on the Launchpad.

Sending audio to a DAW from code--I use [Ableton Live][ableton-live],
but you could use any DAW--requires creating a virtual MIDI device.
This is easy to do on macOS via the built-in `Audio MIDI Setup.app`
application (see [the video][mmm-talk]) for details).  Different
operating systems will have their own processes for doing so, and may
require 3rd party drivers.

## Testing

No tests at this time.

## Contributing

Open a PR on [the GitHub project][gh] if you'd like to contribute!

## License

Copyright © Cameron Desautels, 2023

Distributed under the MIT License.

[blinkenlights]: https://en.wikipedia.org/wiki/Blinkenlights
[lp]: https://novationmusic.com/en/launch
[ableton-live]: https://www.ableton.com/en/live/
[demoscene]: https://en.wikipedia.org/wiki/Demoscene
[mmm-talk]: https://www.youtube.com/watch?v=je7szzU4J0A
[gh]: https://github.com/camdez/launchpad
