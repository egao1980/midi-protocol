;;;; MIDI0 model: SMF, division, events.
;;;;
;;;; Every struct is immutable (read-only slots). Each concrete type has the
;;;; default keyword constructor MAKE-<TYPE>; abstract types have none. There are
;;;; no copiers: COPY-EVENT makes modified copies. Values compare with EQUALP and
;;;; print readably as #S(...).
;;;;
;;;; Slot types are the field ranges of the SMF 1.0 spec. An implementation may
;;;; check them at construction (SBCL does at default safety). WRITE-SMF checks
;;;; every field anyway, so out-of-range values never reach the bytes.
;;;;
;;;; DATA slots hold any vector of integers 0..255. READ-SMF always returns
;;;; (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)); WRITE-SMF accepts any vector and
;;;; signals MIDI-ENCODE-ERROR for an element that isn't an octet. (A #(...)
;;;; literal is a SIMPLE-VECTOR, which is why the slot type is VECTOR: #S values
;;;; read back.) TEXT slots hold strings of characters with codes 0..255, one
;;;; byte per character (ISO-8859-1, as mido's default charset). Every byte
;;;; sequence reads to a string and writes back unchanged.
;;;;
;;;; Wire layout of each event in a track chunk, after its delta time:
;;;;
;;;;   type                     bytes                    notes
;;;;   note-off-event           8n kk vv
;;;;   note-on-event            9n kk vv                 velocity 0 stays a NOTE-ON-EVENT
;;;;   poly-pressure-event      An kk pp
;;;;   control-change-event     Bn cc vv                 channel mode messages (cc 120..127) too
;;;;   program-change-event     Cn pp
;;;;   channel-pressure-event   Dn pp
;;;;   pitch-bend-event         En ll mm                 VALUE = ll + 128 * mm - 8192
;;;;   sysex-event              F0 <len> <data>          DATA verbatim: it includes the closing F7
;;;;   escape-event             F7 <len> <data>          DATA verbatim
;;;;   meta events              FF <type> <len> <data>   table below
;;;;
;;;; n is the channel 0..15; every other byte of a channel event is a data byte
;;;; 0..127. Delta times and <len> are variable-length quantities (VLQ): big-
;;;; endian base-128, bit 7 set on every byte but the last, at most 4 bytes
;;;; (0..#x0FFFFFFF). WRITE-SMF writes the shortest VLQ.
;;;;
;;;; Meta events ("len" is the only length READ-SMF accepts for that type):
;;;;
;;;;   type  struct                    len   data
;;;;   00    sequence-number-event     0, 2  length 0: NUMBER is NIL; length 2: big-endian u16
;;;;   01    text-event                any   TEXT, one byte per character
;;;;   02    copyright-event           any   "
;;;;   03    track-name-event          any   "
;;;;   04    instrument-name-event     any   "
;;;;   05    lyric-event               any   "
;;;;   06    marker-event              any   "
;;;;   07    cue-point-event           any   "
;;;;   08    program-name-event        any   "
;;;;   09    device-name-event         any   "
;;;;   20    channel-prefix-event      1     channel 0..15
;;;;   21    midi-port-event           1     port 0..255
;;;;   2F    end-of-track-event        0
;;;;   51    tempo-event               3     big-endian u24: microseconds per quarter note
;;;;   54    smpte-offset-event        5     0rrhhhhh mm ss ff uu
;;;;   58    time-signature-event      4     nn dd cc bb; DENOMINATOR = 2^dd
;;;;   59    key-signature-event       2     sf mi; sf signed -7..7 (negative = flats); mi 0 major, 1 minor
;;;;   7F    sequencer-specific-event  any   DATA verbatim
;;;;   else  unknown-meta-event        any   TYPE = the type byte, DATA verbatim
;;;;
;;;; SMPTE offset: rr is the frame rate, 0 = 24, 1 = 25, 2 = 29 (29.97 drop
;;;; frame), 3 = 30 fps; hhhhh the hours 0..23; then minutes 0..59, seconds
;;;; 0..59, frames 0..29 and hundredths of a frame 0..99. A known meta event
;;;; whose length isn't the one above, or whose data is outside these ranges, is
;;;; invalid (read.lisp: SKIP-EVENT and USE-VALUE restarts). An
;;;; UNKNOWN-META-EVENT never has one of the known type bytes.

(in-package #:midi-protocol)

(defconstant +default-tempo+ 500000
  "Microseconds per quarter note before the first TEMPO-EVENT (120 per minute).")

;;; Header ---------------------------------------------------------------------

(defstruct (smpte-division (:copier nil))
  "SMPTE time division. FRAMES-PER-SECOND is 24, 25, 29 (29.97 drop frame,
i.e. 30000/1001 frames per second) or 30. A tick lasts
1 / (frames per second * TICKS-PER-FRAME) seconds, whatever the tempo."
  (frames-per-second 25 :type (member 24 25 29 30) :read-only t)
  (ticks-per-frame 40 :type (integer 1 255) :read-only t))

(defstruct (smf (:copier nil))
  "A Standard MIDI File. FORMAT 0 has exactly one track, 1 has simultaneous
tracks, 2 independent ones. DIVISION is ticks per quarter note (1..32767) or an
SMPTE-DIVISION. TRACKS is a list of tracks; a track is a list of MIDI-EVENTs."
  (format 1 :type (integer 0 2) :read-only t)
  (division 480 :type (or (integer 1 32767) smpte-division) :read-only t)
  (tracks '() :type list :read-only t))

;;; Abstract events ------------------------------------------------------------

(defstruct (midi-event (:constructor nil) (:copier nil))
  "An event in a track. DELTA is the tick count since the previous event of
the track (or since the track start)."
  (delta 0 :type (integer 0 #x0FFFFFFF) :read-only t))

(defstruct (channel-event (:include midi-event) (:constructor nil) (:copier nil))
  "A channel voice message."
  (channel 0 :type (integer 0 15) :read-only t))

(defstruct (meta-event (:include midi-event) (:constructor nil) (:copier nil))
  "A meta event (FF <type> <len> <data>).")

(defstruct (text-meta-event (:include meta-event) (:constructor nil) (:copier nil))
  "A meta event whose data is text (types 01..09)."
  (text "" :type string :read-only t))

;;; Channel voice events -------------------------------------------------------

(defstruct (note-off-event (:include channel-event) (:copier nil))
  (key 60 :type (integer 0 127) :read-only t)
  (velocity 64 :type (integer 0 127) :read-only t))

(defstruct (note-on-event (:include channel-event) (:copier nil))
  (key 60 :type (integer 0 127) :read-only t)
  (velocity 64 :type (integer 0 127) :read-only t))

(defstruct (poly-pressure-event (:include channel-event) (:copier nil))
  (key 60 :type (integer 0 127) :read-only t)
  (pressure 0 :type (integer 0 127) :read-only t))

(defstruct (control-change-event (:include channel-event) (:copier nil))
  (controller 0 :type (integer 0 127) :read-only t)
  (value 0 :type (integer 0 127) :read-only t))

(defstruct (program-change-event (:include channel-event) (:copier nil))
  (program 0 :type (integer 0 127) :read-only t))

(defstruct (channel-pressure-event (:include channel-event) (:copier nil))
  (pressure 0 :type (integer 0 127) :read-only t))

(defstruct (pitch-bend-event (:include channel-event) (:copier nil))
  "VALUE is centred: 0 is no bend, -8192 the lowest, 8191 the highest."
  (value 0 :type (integer -8192 8191) :read-only t))

;;; System exclusive -----------------------------------------------------------

(defstruct (sysex-event (:include midi-event) (:copier nil))
  "F0 <len> <data>. DATA is the <len> bytes after the length, verbatim: a
complete message ends with F7, a first packet of a split message doesn't."
  (data #() :type vector :read-only t))

(defstruct (escape-event (:include midi-event) (:copier nil))
  "F7 <len> <data>: a sysex continuation packet, or bytes sent as they are."
  (data #() :type vector :read-only t))

;;; Meta events ----------------------------------------------------------------

(defstruct (sequence-number-event (:include meta-event) (:copier nil))
  "NUMBER is NIL for the length-0 form."
  (number 0 :type (or null (unsigned-byte 16)) :read-only t))

(defstruct (text-event (:include text-meta-event) (:copier nil)))
(defstruct (copyright-event (:include text-meta-event) (:copier nil)))
(defstruct (track-name-event (:include text-meta-event) (:copier nil)))
(defstruct (instrument-name-event (:include text-meta-event) (:copier nil)))
(defstruct (lyric-event (:include text-meta-event) (:copier nil)))
(defstruct (marker-event (:include text-meta-event) (:copier nil)))
(defstruct (cue-point-event (:include text-meta-event) (:copier nil)))
(defstruct (program-name-event (:include text-meta-event) (:copier nil)))
(defstruct (device-name-event (:include text-meta-event) (:copier nil)))

(defstruct (channel-prefix-event (:include meta-event) (:copier nil))
  (channel 0 :type (integer 0 15) :read-only t))

(defstruct (midi-port-event (:include meta-event) (:copier nil))
  (port 0 :type (unsigned-byte 8) :read-only t))

(defstruct (end-of-track-event (:include meta-event) (:copier nil)))

(defstruct (tempo-event (:include meta-event) (:copier nil))
  "TEMPO is microseconds per quarter note."
  (tempo +default-tempo+ :type (unsigned-byte 24) :read-only t))

(defstruct (smpte-offset-event (:include meta-event) (:copier nil))
  "FRAME-RATE is 24, 25, 29 (29.97 drop frame) or 30."
  (frame-rate 24 :type (member 24 25 29 30) :read-only t)
  (hours 0 :type (integer 0 23) :read-only t)
  (minutes 0 :type (integer 0 59) :read-only t)
  (seconds 0 :type (integer 0 59) :read-only t)
  (frames 0 :type (integer 0 29) :read-only t)
  (subframes 0 :type (integer 0 99) :read-only t))

(defstruct (time-signature-event (:include meta-event) (:copier nil))
  "DENOMINATOR is the note value itself (4, 8, ...), a power of two; the file
stores its base-2 logarithm. CLOCKS-PER-CLICK is MIDI clocks (24 per quarter)
per metronome click; THIRTY-SECONDS-PER-QUARTER is notated 32nd notes per MIDI
quarter note."
  (numerator 4 :type (unsigned-byte 8) :read-only t)
  (denominator 4 :type (integer 1) :read-only t)
  (clocks-per-click 24 :type (unsigned-byte 8) :read-only t)
  (thirty-seconds-per-quarter 8 :type (unsigned-byte 8) :read-only t))

(defstruct (key-signature-event (:include meta-event) (:copier nil))
  "SHARPS counts sharps, negative for flats (-3 = three flats)."
  (sharps 0 :type (integer -7 7) :read-only t)
  (minor-p nil :type boolean :read-only t))

(defstruct (sequencer-specific-event (:include meta-event) (:copier nil))
  (data #() :type vector :read-only t))

(defstruct (unknown-meta-event (:include meta-event) (:copier nil))
  "A meta event of a type not in the table above. TYPE is its type byte."
  (type 0 :type (unsigned-byte 8) :read-only t)
  (data #() :type vector :read-only t))

;;; Data tables (data, not rules) ----------------------------------------------

(defparameter +channel-event-types+
  '((#x8 note-off-event 2) (#x9 note-on-event 2) (#xA poly-pressure-event 2)
    (#xB control-change-event 2) (#xC program-change-event 1)
    (#xD channel-pressure-event 1) (#xE pitch-bend-event 2))
  "(high nibble of the status byte, struct, number of data bytes).")

(defparameter +meta-event-types+
  '((#x00 . sequence-number-event) (#x01 . text-event) (#x02 . copyright-event)
    (#x03 . track-name-event) (#x04 . instrument-name-event) (#x05 . lyric-event)
    (#x06 . marker-event) (#x07 . cue-point-event) (#x08 . program-name-event)
    (#x09 . device-name-event) (#x20 . channel-prefix-event) (#x21 . midi-port-event)
    (#x2F . end-of-track-event) (#x51 . tempo-event) (#x54 . smpte-offset-event)
    (#x58 . time-signature-event) (#x59 . key-signature-event)
    (#x7F . sequencer-specific-event))
  "(meta type byte . struct) for every known meta event type.")

(defparameter +smpte-rates+ '(24 25 29 30)
  "Frame rates by their 2-bit code in an SMPTE offset (index = code). In an
SMPTE division the high byte is the negated rate as a signed byte (#xE8 = -24,
#xE7 = -25, #xE3 = -29, #xE2 = -30).")

;;; Copying --------------------------------------------------------------------

(defun copy-event (event &rest initargs)
  "A fresh event of EVENT's type with EVENT's slots, except those named by
INITARGS, which are constructor keywords and values (:DELTA 0, :CHANNEL 9,
:TEXT \"Fine\", ...). With no INITARGS the copy is EQUALP to EVENT. An initarg
that names no slot of EVENT's type signals an ERROR."
  (declare (ignorable event initargs))
  (%midi0-pending 'copy-event))
