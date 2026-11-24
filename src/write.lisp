;;;; MIDI0 SMF writer.
;;;;
;;;; (WRITE-SMF SMF DESTINATION &key (RUNNING-STATUS T) (IF-EXISTS :SUPERSEDE))
;;;; DESTINATION is
;;;;   - NIL: return a fresh (SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)) holding the file;
;;;;   - a binary output stream of element type (UNSIGNED-BYTE 8): write the file
;;;;     to it and return the stream;
;;;;   - a pathname designator: write the file, opened with IF-EXISTS, and return
;;;;     DESTINATION.
;;;; WRITE-SMF checks and encodes the whole SMF before it writes a byte, so a
;;;; MIDI-ENCODE-ERROR leaves a stream untouched and creates or changes no file.
;;;;
;;;; Output, all fields big-endian:
;;;;   "MThd" 00 00 00 06 <format u16> <track count u16> <division u16>
;;;;   then for each track, in order: "MTrk" <length u32> <events>
;;;; No other chunks are written. A ticks-per-quarter division is written as
;;;; is; an SMPTE-DIVISION as the negated frame rate (a signed byte: -24, -25,
;;;; -29, -30) followed by the ticks per frame.
;;;;
;;;; Each event is its delta as the shortest VLQ, then its bytes from the layout
;;;; table in model.lisp, every <len> the shortest VLQ. When a track's last event
;;;; isn't an END-OF-TRACK-EVENT, one with delta 0 is appended (as mido does), so
;;;; an empty track is 00 FF 2F 00.
;;;;
;;;; Running status. With RUNNING-STATUS T (the default, and what mido writes), a
;;;; channel event whose status byte equals that of the previous event of the
;;;; track, when that previous event is a channel event, omits the status byte.
;;;; A meta, sysex or escape event in between resets it, so the next channel
;;;; event writes its status byte again. With RUNNING-STATUS NIL every channel
;;;; event writes its status byte.
;;;;
;;;; MIDI-ENCODE-ERROR, with a REASON, TRACK (the index of the track at fault,
;;;; or NIL when the SMF as a whole is) and EVENT (the offending track element,
;;;; or NIL when the problem is the SMF or a whole track), for
;;;;   - FORMAT 0 with a track count other than 1, or more than 65535 tracks;
;;;;   - a track that isn't a list, or a track element that isn't a MIDI-EVENT;
;;;;   - an END-OF-TRACK-EVENT that isn't the last event of its track;
;;;;   - a field outside its slot type in model.lisp (when construction let it
;;;;     through);
;;;;   - a DATA element that isn't an integer 0..255, or a TEXT character whose
;;;;     code is above 255;
;;;;   - a TIME-SIGNATURE-EVENT denominator that isn't a power of two;
;;;;   - an UNKNOWN-META-EVENT whose TYPE is a known meta type byte;
;;;;   - a <len> above #x0FFFFFFF or a chunk longer than #xFFFFFFFF bytes.
;;;;
;;;; Byte I/O builds on binary-protocol (PACK for fixed-width fields; VLQs and
;;;; the 24-bit tempo as in read.lisp).

(in-package #:midi-protocol)

(defun write-smf (smf destination &key (running-status t) (if-exists :supersede))
  "Write SMF as a Standard MIDI File to DESTINATION (NIL for a fresh octet
vector, a binary output stream, or a pathname designator opened with
IF-EXISTS). RUNNING-STATUS T omits repeated channel status bytes. Signals
MIDI-ENCODE-ERROR, before writing anything, for an SMF it can't write."
  (declare (ignorable smf destination running-status if-exists))
  (%midi0-pending 'write-smf))
