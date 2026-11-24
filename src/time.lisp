;;;; MIDI0 absolute time: ticks from deltas and back, merging tracks, tempo maps
;;;; and seconds.
;;;;
;;;; A timed event is a cons (TICK . EVENT): TICK is the absolute tick (the sum
;;;; of the deltas of the event and every event before it in its track).
;;;;
;;;; TRACK-TO-ABSOLUTE returns a fresh list of timed events, one per event, in
;;;; track order. The events are the track's own objects (their deltas
;;;; unchanged).
;;;;
;;;; TRACK-FROM-ABSOLUTE builds a track from timed events in any order:
;;;;   1. STABLE-SORT them by tick (equal ticks keep their order in the input);
;;;;   2. drop every END-OF-TRACK-EVENT;
;;;;   3. give each remaining event the delta from the previous one (the first
;;;;      from tick 0), copying it with COPY-EVENT when its delta changes;
;;;;   4. append one (MAKE-END-OF-TRACK-EVENT) at the largest tick in the input,
;;;;      END-OF-TRACK-EVENTs included (tick 0 for an empty input).
;;;; So the result is always a well-formed track that ends with exactly one
;;;; END-OF-TRACK-EVENT, and (track-from-absolute (track-to-absolute track)) is
;;;; EQUALP to TRACK when TRACK ends with its only END-OF-TRACK-EVENT.
;;;;
;;;; MERGE-TRACKS is
;;;;   (track-from-absolute (loop for track in tracks
;;;;                              append (track-to-absolute track)))
;;;; so events at the same tick keep track order, then their order in the track.
;;;; This is mido's merge_tracks; use it to turn a format 1 SMF into format 0.
;;;;
;;;; TEMPO-MAP takes an SMF (all its tracks, merged as by MERGE-TRACKS) or one
;;;; track (a list of events: use that for format 2, whose tracks are
;;;; independent). It returns a fresh list of (TICK . TEMPO) conses, TEMPO in
;;;; microseconds per quarter note, ascending by tick:
;;;;   - one entry for each tick that has a TEMPO-EVENT, holding the tempo of the
;;;;     last TEMPO-EVENT at that tick in merged order;
;;;;   - first, (0 . +DEFAULT-TEMPO+) when no TEMPO-EVENT is at tick 0.
;;;; Equal tempos at different ticks are all kept. With no TEMPO-EVENT at all
;;;; the map is ((0 . 500000)).
;;;;
;;;; TICKS-TO-SECONDS gives the exact time of an absolute tick, as a rational:
;;;;   - DIVISION an integer (ticks per quarter note): the sum, over the tempo
;;;;     map's segments before TICKS, of ticks-in-segment * tempo
;;;;     / (1000000 * division). A tempo takes effect at its own tick. TEMPO-MAP
;;;;     NIL means +DEFAULT-TEMPO+ throughout.
;;;;   - DIVISION an SMPTE-DIVISION: ticks / (fps * ticks-per-frame), with fps
;;;;     30000/1001 for 29. TEMPO-MAP is ignored.
;;;; This agrees with mido's playback times (MidiFile iteration) up to float
;;;; rounding.

(in-package #:midi-protocol)

(defun track-to-absolute (track)
  "A fresh list of (TICK . EVENT), one per event of TRACK, in order."
  (declare (ignorable track))
  (%midi0-pending 'track-to-absolute))

(defun track-from-absolute (timed-events)
  "A track from (TICK . EVENT) conses in any order: stable-sorted by tick,
END-OF-TRACK-EVENTs dropped, deltas recomputed, and one END-OF-TRACK-EVENT
appended at the largest tick (see the file header)."
  (declare (ignorable timed-events))
  (%midi0-pending 'track-from-absolute))

(defun merge-tracks (tracks)
  "One track holding every event of TRACKS at its absolute time; ties keep
track order. Ends with one END-OF-TRACK-EVENT at the latest tick."
  (declare (ignorable tracks))
  (%midi0-pending 'merge-tracks))

(defun tempo-map (source)
  "The (TICK . MICROSECONDS-PER-QUARTER) entries of SOURCE, an SMF or a track,
starting at tick 0 (see the file header)."
  (declare (ignorable source))
  (%midi0-pending 'tempo-map))

(defun ticks-to-seconds (ticks division &optional tempo-map)
  "Exact seconds (a rational) at absolute tick TICKS, for DIVISION (ticks per
quarter note, or an SMPTE-DIVISION) and TEMPO-MAP (from TEMPO-MAP; NIL means
120 quarter notes per minute throughout)."
  (declare (ignorable ticks division tempo-map))
  (%midi0-pending 'ticks-to-seconds))
