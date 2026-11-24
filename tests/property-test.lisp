;;;; MIDI0 properties over seeded pseudo-random SMFs (every event type, both
;;;; running-status modes, SMPTE and ticks-per-quarter divisions).

(in-package #:midi-protocol/tests)

(defun random-event ()
  "Any event a track may hold before its END-OF-TRACK-EVENT, including what
mido can't represent (escape events, 8-bit sysex, length-0 sequence numbers)."
  (if (< (rnd 4) 3)
      (random-mido-event)
      (let ((delta (rnd-pick 0 (rnd 200) #x0FFFFFFF)))
        (ecase (rnd 8)
          (0 (make-escape-event :delta delta :data (octets (loop repeat (rnd 12) collect (rnd 256)))))
          (1 (make-sysex-event :delta delta :data (octets (loop repeat (rnd 300) collect (rnd 256)))))
          (2 (make-sequence-number-event :delta delta :number nil))
          (3 (make-instrument-name-event :delta delta :text (random-text)))
          (4 (make-track-name-event :delta delta :text (make-string (rnd 400) :initial-element #\z)))
          (5 (make-cue-point-event :delta delta :text (random-text)))
          (6 (make-device-name-event :delta delta :text (random-text)))
          (7 (make-unknown-meta-event :delta delta :type #xFF :data (octets (rnd 256))))))))

(defparameter +property-runs+ 150)

(deftest write-then-read-is-identity
  (let* ((*seed* 7)
         (bad (loop for i from 0 below +property-runs+
                    for smf = (random-smf #'random-event)
                    for running-status = (evenp i)
                    for back = (read-smf (write-smf smf nil :running-status running-status))
                    unless (equalp smf back) collect i)))
    (ok (null bad) (report bad))))

(deftest read-then-write-is-stable
  (let* ((*seed* 11)
         (bad (loop for i from 0 below +property-runs+
                    for running-status = (evenp i)
                    for bytes = (write-smf (random-smf #'random-event) nil
                                           :running-status running-status)
                    unless (equalp bytes (write-smf (read-smf bytes) nil
                                                    :running-status running-status))
                      collect i)))
    (ok (null bad) (report bad))))

(deftest running-status-only-saves-bytes
  (let* ((*seed* 13)
         (bad (loop for i from 0 below +property-runs+
                    for smf = (random-smf #'random-event)
                    for with = (write-smf smf nil :running-status t)
                    for without = (write-smf smf nil :running-status nil)
                    unless (and (<= (length with) (length without))
                                (equalp (read-smf with) (read-smf without)))
                      collect i)))
    (ok (null bad) (report bad))))

(defun timed-without-eot (track)
  (remove-if (lambda (timed) (end-of-track-event-p (cdr timed))) (track-to-absolute track)))

(defun undelta (timed-events)
  "TIMED-EVENTS with every event's delta set to 0, so events compare by content."
  (mapcar (lambda (timed) (cons (car timed) (copy-event (cdr timed) :delta 0))) timed-events))

(deftest merge-tracks-invariants
  (let* ((*seed* 17)
         (bad (loop for i from 0 below +property-runs+
                    for tracks = (smf-tracks (random-smf #'random-event))
                    for merged = (merge-tracks tracks)
                    for timed = (track-to-absolute merged)
                    for ticks = (mapcar #'car timed)
                    unless (and (end-of-track-event-p (car (last merged)))
                                (= 1 (count-if #'end-of-track-event-p merged))
                                (equal ticks (sort (copy-list ticks) #'<=))
                                (= (car (last ticks))
                                   (reduce #'max (mapcar (lambda (track)
                                                           (reduce #'+ track :key #'midi-event-delta))
                                                         tracks)
                                           :initial-value 0))
                                (= (1- (length merged))
                                   (reduce #'+ (mapcar (lambda (track)
                                                         (length (timed-without-eot track)))
                                                       tracks)))
                                ;; the events, at their ticks, in stable tick order
                                (equalp (undelta (timed-without-eot merged))
                                        (stable-sort (undelta (loop for track in tracks
                                                                    append (timed-without-eot track)))
                                                     #'< :key #'car)))
                      collect i)))
    (ok (null bad) (report bad))))

(deftest absolute-round-trip-on-random-tracks
  (let* ((*seed* 19)
         (bad (loop for i from 0 below +property-runs+
                    for track in (loop repeat +property-runs+
                                       append (smf-tracks (random-smf #'random-event)))
                    unless (equalp track (track-from-absolute (track-to-absolute track)))
                      collect i)))
    (ok (null bad) (report bad))))
