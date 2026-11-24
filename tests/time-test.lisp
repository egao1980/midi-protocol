;;;; MIDI0 absolute time: TRACK-TO-ABSOLUTE, TRACK-FROM-ABSOLUTE, MERGE-TRACKS,
;;;; TEMPO-MAP, TICKS-TO-SECONDS. Rules in src/time.lisp; playback times from
;;;; mido 1.3.3 (tests/fixtures/midi0-seconds.sexp).

(in-package #:midi-protocol/tests)

(deftest track-to-absolute-sums-deltas
  (let* ((a (make-note-on-event))
         (b (make-note-on-event :delta 10 :velocity 0))
         (c (make-text-event :text "x"))
         (d (make-end-of-track-event :delta 5))
         (timed (track-to-absolute (list a b c d))))
    (ok (equal '(0 10 10 15) (mapcar #'car timed)))
    (ok (and (= 4 (length timed)) (every #'eq (list a b c d) (mapcar #'cdr timed)))
        "the track's own events"))
  (ok (null (track-to-absolute '()))))

(deftest track-from-absolute-sorts-stably
  (let ((on60 (make-note-on-event :delta 99 :key 60))
        (name (make-track-name-event :text "n"))
        (off60 (make-note-on-event :key 60 :velocity 0))
        (on62 (make-note-on-event :key 62))
        (off62 (make-note-off-event :key 62)))
    (ok (equalp (list (make-note-on-event :key 60) (make-track-name-event :text "n")
                      (make-note-on-event :delta 20 :key 60 :velocity 0)
                      (make-note-on-event :key 62) (make-note-off-event :delta 20 :key 62)
                      (make-end-of-track-event))
                (track-from-absolute (list (cons 20 off60) (cons 0 on60) (cons 20 on62)
                                           (cons 0 name) (cons 40 off62)))))))

(deftest-parametrize track-from-absolute-ends-the-track
    ((timed expected)
     :ids ("empty" "only-eot" "eot-after" "eot-before" "two-eots")
     ('() (list (make-end-of-track-event)))
     ((list (cons 50 (make-end-of-track-event))) (list (make-end-of-track-event :delta 50)))
     ((list (cons 40 (make-program-change-event)) (cons 100 (make-end-of-track-event)))
      (list (make-program-change-event :delta 40) (make-end-of-track-event :delta 60)))
     ((list (cons 10 (make-end-of-track-event)) (cons 40 (make-program-change-event)))
      (list (make-program-change-event :delta 40) (make-end-of-track-event)))
     ((list (cons 5 (make-end-of-track-event)) (cons 0 (make-program-change-event))
            (cons 70 (make-end-of-track-event :delta 3)))
      (list (make-program-change-event) (make-end-of-track-event :delta 70))))
  (ok (equalp expected (track-from-absolute timed))))

(deftest absolute-round-trip-on-fixtures
  (let ((bad (loop for (name) in +canonical-fixtures+
                   append (loop for track in (smf-tracks (fixture-smf name))
                                for i from 0
                                unless (equalp track (track-from-absolute (track-to-absolute track)))
                                  collect (list name i)))))
    (ok (null bad) (report bad))))

;;; Merging --------------------------------------------------------------------

(deftest merge-tracks-matches-mido
  (ok (equalp (first (smf-tracks (fixture-smf "type0-merged.mid")))
              (merge-tracks (smf-tracks (fixture-smf "type1-ensemble.mid"))))))

(deftest merge-tracks-keeps-track-order-on-ties
  (ok (equalp (list (make-program-change-event :program 1)
                    (make-note-on-event :delta 10 :key 60)
                    (make-note-on-event :key 70)
                    (make-end-of-track-event :delta 30))
              (merge-tracks
               (list (list (make-program-change-event :program 1)
                           (make-note-on-event :delta 10 :key 60)
                           (make-end-of-track-event :delta 5))
                     (list (make-note-on-event :delta 10 :key 70)
                           (make-end-of-track-event :delta 30))))))
  (ok (equalp (list (make-end-of-track-event)) (merge-tracks '())))
  (let ((track (first (smf-tracks (fixture-smf "type0-melody.mid")))))
    (ok (equalp track (merge-tracks (list track))) "one track merges to itself")))

;;; Tempo maps -----------------------------------------------------------------

(deftest-parametrize tempo-maps
    ((source expected)
     :ids ("ensemble" "melody" "no-tempo" "late-first-tempo" "equal-tempos-kept"
           "tempo-in-a-later-track" "same-tick-last-wins" "a-track")
     ((fixture-smf "type1-ensemble.mid")
      '((0 . 500000) (1920 . 750000) (3360 . 410000) (4800 . 450000)))
     ((fixture-smf "type0-melody.mid") '((0 . 600000)))
     ((make-smf :tracks (list (list (make-note-on-event)))) '((0 . 500000)))
     ((make-smf :tracks (list (list (make-tempo-event :delta 100 :tempo 400000))))
      '((0 . 500000) (100 . 400000)))
     ((make-smf :tracks (list (list (make-tempo-event :tempo 500000)
                                    (make-tempo-event :delta 960 :tempo 500000))))
      '((0 . 500000) (960 . 500000)))
     ((make-smf :tracks (list (list (make-note-on-event :delta 10))
                              (list (make-tempo-event :delta 480 :tempo 300000))))
      '((0 . 500000) (480 . 300000)))
     ((make-smf :tracks (list (list (make-tempo-event :delta 480 :tempo 300000))
                              (list (make-tempo-event :delta 480 :tempo 200000))))
      '((0 . 500000) (480 . 200000)))
     ((list (make-tempo-event :tempo 250000) (make-tempo-event :delta 96 :tempo 1000000))
      '((0 . 250000) (96 . 1000000))))
  (ok (equal expected (tempo-map source))))

;;; Seconds --------------------------------------------------------------------

(deftest-parametrize ticks-to-seconds-exact
    ((ticks division map seconds)
     (0 480 '((0 . 500000)) 0)
     (480 480 '((0 . 500000)) 1/2)
     (960 480 nil 1)
     (1 3 '((0 . 1000000)) 1/3)
     (1000 480 '((0 . 500000) (1920 . 750000)) 25/24)
     (1920 480 '((0 . 500000) (1920 . 750000)) 2)
     (2400 480 '((0 . 500000) (1920 . 750000)) 11/4)
     (480 480 '((0 . 500000) (480 . 1000000)) 1/2)
     (960 480 '((0 . 500000) (480 . 1000000)) 3/2)
     (1000 (make-smpte-division :frames-per-second 25 :ticks-per-frame 40) nil 1)
     (1000 (make-smpte-division :frames-per-second 25 :ticks-per-frame 40) '((0 . 1)) 1)
     (2400 (make-smpte-division :frames-per-second 29 :ticks-per-frame 80) nil 1001/1000)
     (96 (make-smpte-division :frames-per-second 24 :ticks-per-frame 4) nil 1)
     (3000 (make-smpte-division :frames-per-second 30 :ticks-per-frame 100) nil 1))
  (let ((got (ticks-to-seconds ticks division map)))
    (ok (typep got 'rational) "exact")
    (ok (eql seconds got) (format nil "~S, got ~S" seconds got))))

(deftest ticks-to-seconds-default-map
  (ok (eql 1 (ticks-to-seconds 960 480))))

(deftest-parametrize seconds-match-mido
    ((name) :rows (mapcar #'first (fixture-sexp "midi0-seconds.sexp")))
  (let* ((rows (rest (assoc name (fixture-sexp "midi0-seconds.sexp") :test #'string=)))
         (smf (fixture-smf name))
         (map (tempo-map smf))
         (bad (loop for (tick seconds) in rows
                    for ours = (ticks-to-seconds tick (smf-division smf) map)
                    unless (< (abs (- ours seconds)) 1/1000000000)
                      collect (list tick seconds ours))))
    (ok (plusp (length rows)))
    (ok (null bad) (report bad))))
