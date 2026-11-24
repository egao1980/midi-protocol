;;;; midi-protocol: MIDI 1.0 events and Standard MIDI Files (SMF), at the event
;;;; level. Brief: cl-stack/docs/capabilities/score.md (midi-protocol, slice SC3).
;;;; Files only: no real-time ports, no MIDI 2.0, no notation semantics.

(defpackage #:midi-protocol
  (:use #:cl)
  (:nicknames #:stack-midi)
  (:documentation "MIDI 1.0 event model and Standard MIDI File reader and writer.

Events are immutable structs (read-only slots, keyword constructors, EQUALP
comparison, readable #S printing). Every concrete type ends in -EVENT. Exported
accessors are the ones for the slots a type introduces. Inherited slots are
read with the parent's accessor: MIDI-EVENT-DELTA, CHANNEL-EVENT-CHANNEL and
TEXT-META-EVENT-TEXT.

A track is a list of events. An SMF is a header (format, division) and a list
of tracks. READ-SMF and WRITE-SMF convert between SMF values and bytes;
TRACK-TO-ABSOLUTE, TRACK-FROM-ABSOLUTE, MERGE-TRACKS, TEMPO-MAP and
TICKS-TO-SECONDS work with absolute time.")
  (:export
   ;; Conditions and restarts (conditions.lisp)
   #:midi-error #:midi-error-reason
   #:midi-parse-error #:midi-parse-error-offset #:midi-parse-error-track
   #:midi-encode-error #:midi-encode-error-event #:midi-encode-error-track
   #:midi-format-warning #:midi-format-warning-offset #:midi-format-warning-reason
   #:skip-chunk #:skip-event #:use-partial-smf
   ;; SMF and division (model.lisp)
   #:smf #:make-smf #:smf-p #:smf-format #:smf-division #:smf-tracks
   #:smpte-division #:make-smpte-division #:smpte-division-p
   #:smpte-division-frames-per-second #:smpte-division-ticks-per-frame
   #:+default-tempo+
   ;; Abstract event types
   #:midi-event #:midi-event-p #:midi-event-delta
   #:channel-event #:channel-event-p #:channel-event-channel
   #:meta-event #:meta-event-p
   #:text-meta-event #:text-meta-event-p #:text-meta-event-text
   ;; Channel voice events
   #:note-off-event #:make-note-off-event #:note-off-event-p
   #:note-off-event-key #:note-off-event-velocity
   #:note-on-event #:make-note-on-event #:note-on-event-p
   #:note-on-event-key #:note-on-event-velocity
   #:poly-pressure-event #:make-poly-pressure-event #:poly-pressure-event-p
   #:poly-pressure-event-key #:poly-pressure-event-pressure
   #:control-change-event #:make-control-change-event #:control-change-event-p
   #:control-change-event-controller #:control-change-event-value
   #:program-change-event #:make-program-change-event #:program-change-event-p
   #:program-change-event-program
   #:channel-pressure-event #:make-channel-pressure-event #:channel-pressure-event-p
   #:channel-pressure-event-pressure
   #:pitch-bend-event #:make-pitch-bend-event #:pitch-bend-event-p
   #:pitch-bend-event-value
   ;; System exclusive
   #:sysex-event #:make-sysex-event #:sysex-event-p #:sysex-event-data
   #:escape-event #:make-escape-event #:escape-event-p #:escape-event-data
   ;; Meta events
   #:sequence-number-event #:make-sequence-number-event #:sequence-number-event-p
   #:sequence-number-event-number
   #:text-event #:make-text-event #:text-event-p
   #:copyright-event #:make-copyright-event #:copyright-event-p
   #:track-name-event #:make-track-name-event #:track-name-event-p
   #:instrument-name-event #:make-instrument-name-event #:instrument-name-event-p
   #:lyric-event #:make-lyric-event #:lyric-event-p
   #:marker-event #:make-marker-event #:marker-event-p
   #:cue-point-event #:make-cue-point-event #:cue-point-event-p
   #:program-name-event #:make-program-name-event #:program-name-event-p
   #:device-name-event #:make-device-name-event #:device-name-event-p
   #:channel-prefix-event #:make-channel-prefix-event #:channel-prefix-event-p
   #:channel-prefix-event-channel
   #:midi-port-event #:make-midi-port-event #:midi-port-event-p #:midi-port-event-port
   #:end-of-track-event #:make-end-of-track-event #:end-of-track-event-p
   #:tempo-event #:make-tempo-event #:tempo-event-p #:tempo-event-tempo
   #:smpte-offset-event #:make-smpte-offset-event #:smpte-offset-event-p
   #:smpte-offset-event-frame-rate #:smpte-offset-event-hours
   #:smpte-offset-event-minutes #:smpte-offset-event-seconds
   #:smpte-offset-event-frames #:smpte-offset-event-subframes
   #:time-signature-event #:make-time-signature-event #:time-signature-event-p
   #:time-signature-event-numerator #:time-signature-event-denominator
   #:time-signature-event-clocks-per-click
   #:time-signature-event-thirty-seconds-per-quarter
   #:key-signature-event #:make-key-signature-event #:key-signature-event-p
   #:key-signature-event-sharps #:key-signature-event-minor-p
   #:sequencer-specific-event #:make-sequencer-specific-event
   #:sequencer-specific-event-p #:sequencer-specific-event-data
   #:unknown-meta-event #:make-unknown-meta-event #:unknown-meta-event-p
   #:unknown-meta-event-type #:unknown-meta-event-data
   ;; Copying (model.lisp)
   #:copy-event
   ;; Codec (read.lisp, write.lisp)
   #:read-smf #:write-smf
   ;; Absolute time (time.lisp)
   #:track-to-absolute #:track-from-absolute #:merge-tracks
   #:tempo-map #:ticks-to-seconds))
