;;;; MIDI0 malformed input: error offsets, track indices, restarts, partial
;;;; results; and the writer's MIDI-ENCODE-ERRORs. Offsets follow the rules in
;;;; src/read.lisp. In a format 0 file the MThd chunk is bytes 0..13, the first
;;;; MTrk chunk header 14..21 and its first event starts at 22.

(in-package #:midi-protocol/tests)

(deftest condition-types
  (ok (subtypep 'midi-parse-error 'midi-error))
  (ok (subtypep 'midi-parse-error 'parse-error))
  (ok (subtypep 'midi-encode-error 'midi-error))
  (ok (subtypep 'midi-error 'error))
  (ok (subtypep 'midi-format-warning 'warning))
  (ok (not (subtypep 'midi-format-warning 'error)))
  (ok (search "byte 23" (princ-to-string (make-condition 'midi-parse-error :offset 23 :track 0
                                                         :reason "bad")))))

(deftest-parametrize error-offsets
    ((bytes offset track)
     :ids ("not-mthd" "empty" "id-cut" "bad-id-cut" "length-cut" "header-length-4" "format-3"
           "division-0" "smpte-23-fps" "smpte-0-ticks" "header-cut" "long-header-cut"
           "no-track" "second-track-missing" "chunk-header-cut" "chunk-past-data"
           "event-past-chunk" "no-running-status" "system-common" "real-time"
           "status-for-data" "program-data-high" "delta-vlq-5-bytes" "length-vlq-5-bytes"
           "sysex-past-chunk" "meta-past-chunk" "tempo-length-2" "tempo-length-4"
           "key-8-sharps" "key-8-flats" "key-mode-2" "eot-length-1" "channel-prefix-16"
           "midi-port-length-0" "sequence-number-length-1" "time-signature-length-3"
           "smpte-offset-hours-24" "smpte-offset-bit-7" "smpte-offset-minutes-60"
           "smpte-offset-frames-30" "smpte-offset-subframes-100" "smpte-offset-length-4"
           "second-track-bad-status" "alien-chunk-cut" "track-index-skips-alien"
           "running-status-per-track" "note-on-cut" "delta-cut")
     ((octets "RIFF" (u32 6) (u16 0) (u16 1) (u16 96)) 0 nil)
     ((octets) 0 nil)
     ((octets "MT") 2 nil)
     ((octets "XY") 2 nil)
     ((octets "MThd" 0 0) 6 nil)
     ((octets "MThd" (u32 4) (u16 0) (u16 1)) 4 nil)
     ((octets (mthd 3 1 96) (mtrk +eot+)) 8 nil)
     ((octets (mthd 0 1 0) (mtrk +eot+)) 12 nil)
     ((octets (mthd 0 1 #xE928) (mtrk +eot+)) 12 nil)
     ((octets (mthd 0 1 #xE700) (mtrk +eot+)) 12 nil)
     ((subseq (mthd 0 1 96) 0 10) 10 nil)
     ((octets "MThd" (u32 8) (u16 0) (u16 1) (u16 96) #xAA) 15 nil)
     ((mthd 0 1 96) 14 nil)
     ((octets (mthd 1 2 96) (mtrk +eot+)) 26 nil)
     ((octets (mthd 0 1 96) "MTr") 17 nil)
     ((octets (mthd 0 1 96) "MTrk" (u32 16) 0 #x90 60 64) 26 0)
     ((octets (mthd 0 1 96) "MTrk" (u32 3) 0 #x90 60 64 +eot+) 25 0)
     ((smf-bytes 0 96 (list 0 60 64 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xF1 0 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xF8 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #x90 60 #x90 +eot+)) 25 0)
     ((smf-bytes 0 96 (list 0 #xC0 #x80 +eot+)) 24 0)
     ((smf-bytes 0 96 (list #x80 #x80 #x80 #x80 0 #x90 60 64 +eot+)) 22 0)
     ((smf-bytes 0 96 (list 0 #xFF 1 #x80 #x80 #x80 #x80 0 +eot+)) 25 0)
     ((smf-bytes 0 96 (list 0 #xF0 5 1 2)) 27 0)
     ((smf-bytes 0 96 (list 0 #xFF #x01 10 65)) 27 0)
     ((smf-bytes 0 96 (list 10 #xFF #x51 2 7 #xA1 5 #xFF #x2F 0)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x51 4 0 7 #xA1 #x20 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x59 2 8 0 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x59 2 #xF8 0 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x59 2 0 2 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x2F 1 0)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x20 1 16 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x21 0 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF 0 1 5 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x58 3 4 2 24 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x54 5 24 0 0 0 0 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x54 5 #x80 0 0 0 0 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x54 5 0 60 0 0 0 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x54 5 0 0 0 30 0 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x54 5 0 0 0 0 100 +eot+)) 23 0)
     ((smf-bytes 0 96 (list 0 #xFF #x54 4 0 0 0 0 +eot+)) 23 0)
     ((smf-bytes 1 96 +eot+ (list 0 #xF4 +eot+)) 35 1)
     ((octets (mthd 0 1 96) "JUNK" (u32 16) 1 2) 24 nil)
     ((octets (mthd 1 2 96) (mtrk +eot+) (chunk "JUNK" 1 2) (mtrk 0 #xF4 +eot+)) 45 1)
     ((smf-bytes 1 96 (list 0 #x90 60 64 +eot+) (list 0 62 64 +eot+)) 39 1)
     ((octets (mthd 0 1 96) "MTrk" (u32 8) 0 #x90 60) 25 0)
     ((octets (mthd 0 1 96) "MTrk" (u32 8) #x81) 23 0))
  (let ((c (parse-failure (lambda () (read-smf bytes)))))
    (ok (typep c 'midi-parse-error) "signals MIDI-PARSE-ERROR")
    (when (typep c 'midi-parse-error)
      (ok (eql offset (midi-parse-error-offset c))
          (format nil "offset ~D, got ~S" offset (midi-parse-error-offset c)))
      (ok (eql track (midi-parse-error-track c))
          (format nil "track ~S, got ~S" track (midi-parse-error-track c)))
      (ok (stringp (midi-error-reason c)) "a reason"))))

;;; Every proper prefix of a valid file fails at its own length ----------------

(defparameter +truncation-fixtures+
  '("type0-melody.mid" "type1-ensemble.mid" "smpte-division.mid" "type0-empty-track.mid"
    "type1-no-tracks.mid"))

(deftest-parametrize truncation-offsets
    ((name) :rows +truncation-fixtures+)
  (let* ((bytes (fixture-octets name))
         (bad (loop for end from 0 below (length bytes)
                    for failure = (failure-offset (subseq bytes 0 end))
                    unless (and (consp failure) (eql end (first failure)))
                      collect (list end failure))))
    (ok (null bad) (report bad))))

(defun track-prefix-p (partial full)
  (and (<= (length partial) (length full))
       (every #'equalp partial (subseq full 0 (length partial)))))

(deftest-parametrize truncation-partial-results
    ((name) :rows +truncation-fixtures+)
  ;; From the end of the header on, USE-PARTIAL-SMF returns the complete tracks so
  ;; far and an event prefix of the one being read.
  (let* ((bytes (fixture-octets name))
         (full (fixture-smf name))
         (bad (loop for end from 14 below (length bytes)
                    for partial = (handler-case
                                      (read-invoking 'midi-protocol:use-partial-smf
                                                     (subseq bytes 0 end))
                                    (midi-parse-error (c) c))
                    unless (and (smf-p partial)
                                (= (smf-format partial) (smf-format full))
                                (equalp (smf-division partial) (smf-division full))
                                (let ((n (length (smf-tracks partial))))
                                  (and (<= n (length (smf-tracks full)))
                                       (every #'equalp (butlast (smf-tracks partial))
                                              (smf-tracks full))
                                       (or (zerop n)
                                           (track-prefix-p (car (last (smf-tracks partial)))
                                                           (nth (1- n) (smf-tracks full)))))))
                      collect (list end partial))))
    (ok (equalp full (read-smf bytes)) "the whole file reads")
    (ok (null bad) (report bad))))

;;; Restarts -------------------------------------------------------------------

(deftest-parametrize restarts-offered-per-error
    ((bytes expected)
     :ids ("header" "missing-track" "alien-cut" "chunk-header-cut" "bad-status" "meta-past-chunk"
           "bad-meta" "bad-vlq")
     ((octets (mthd 3 1 96) (mtrk +eot+)) '())
     ((mthd 0 1 96) '(use-partial-smf))
     ((octets (mthd 0 1 96) "JUNK" (u32 16) 1 2) '(use-partial-smf))
     ((octets (mthd 0 1 96) "MTr") '(use-partial-smf))
     ((smf-bytes 0 96 (list 0 #xF4 +eot+)) '(skip-chunk use-partial-smf))
     ((smf-bytes 0 96 (list 0 #xFF #x01 10 65)) '(skip-chunk use-partial-smf))
     ((smf-bytes 0 96 (list 0 #xFF #x51 2 7 #xA1 +eot+))
      '(skip-chunk skip-event use-partial-smf use-value))
     ((smf-bytes 0 96 (list #x80 #x80 #x80 #x80 0 #xC0 1 +eot+)) '(skip-chunk use-partial-smf)))
  (ok (equal expected (restarts-offered bytes))))

(deftest skip-chunk-drops-a-bad-track
  (let ((smf (read-invoking 'midi-protocol:skip-chunk
                            (smf-bytes 1 96 (list 0 #xC0 5 +eot+) (list 0 #xF4 +eot+)
                                       (list 0 #xC0 6 +eot+)))))
    (ok (= 1 (smf-format smf)))
    (ok (equalp (list (list (make-program-change-event :program 5) (make-end-of-track-event))
                      (list (make-program-change-event :program 6) (make-end-of-track-event)))
                (smf-tracks smf)))))

(deftest skip-chunk-on-the-last-track-ends-the-read
  (let ((smf (read-invoking 'midi-protocol:skip-chunk
                            (octets (mthd 0 1 96) "MTrk" (u32 16) 0 #x90 60 64))))
    (ok (smf-p smf))
    (ok (null (smf-tracks smf)) "the dropped chunk counts toward NTRKS")))

(deftest skip-chunk-past-the-data-then-missing-track
  (let ((seen '()))
    (handler-case
        (handler-bind ((midi-parse-error
                         (lambda (c)
                           (push (list (midi-parse-error-offset c) (midi-parse-error-track c)) seen)
                           (skip-chunk c))))
          (read-smf (octets (mthd 1 2 96) "MTrk" (u32 16) 0 #x90 60 64)))
      (midi-parse-error (c) (push (list :unhandled (midi-parse-error-offset c)) seen)))
    (ok (equal '((26 0) (26 nil) (:unhandled 26)) (reverse seen)))))

(deftest skip-event-moves-its-delta-on
  (ok (equalp (list (list (make-end-of-track-event :delta 15)))
              (smf-tracks (read-invoking 'midi-protocol:skip-event
                                         (smf-bytes 0 96 (list 10 #xFF #x51 2 7 #xA1
                                                               5 #xFF #x2F 0))))))
  (ok (equalp (list (list (make-program-change-event :delta 12 :program 1)
                          (make-end-of-track-event)))
              (smf-tracks (read-invoking 'midi-protocol:skip-event
                                         (smf-bytes 0 96 (list 3 #xFF #x51 2 0 0
                                                               4 #xFF #x59 2 9 0
                                                               5 #xC0 1 +eot+))))))
  (ok (equalp (list (list (make-note-on-event :key 60 :velocity 64)
                          (make-note-on-event :delta 2 :key 62 :velocity 64)
                          (make-end-of-track-event)))
              (smf-tracks (read-invoking 'midi-protocol:skip-event
                                         (smf-bytes 0 96 (list 0 #x90 60 64 0 #xFF #x20 1 99
                                                               2 62 64 +eot+)))))
      "running status survives a skipped event"))

(deftest use-value-replaces-a-bad-meta-event
  (ok (equalp (list (list (make-text-event :delta 10 :text "?") (make-end-of-track-event :delta 5)))
              (smf-tracks (read-invoking 'use-value
                                         (smf-bytes 0 96 (list 10 #xFF #x51 2 7 #xA1
                                                               5 #xFF #x2F 0))
                                         (make-text-event :text "?")))))
  (ok (equalp (list (list (make-key-signature-event :delta 4) (make-end-of-track-event)))
              (smf-tracks (read-invoking 'use-value
                                         (smf-bytes 0 96 (list 4 #xFF #x59 2 9 0 +eot+))
                                         (make-key-signature-event :delta 99))))
      "the replacement gets the bad event's delta"))

(deftest use-partial-smf-returns-what-was-read
  (let ((smf (read-invoking 'midi-protocol:use-partial-smf
                            (smf-bytes 1 96 (list 0 #xC0 5 +eot+)
                                       (list 0 #x90 60 64 #x60 60 0 0 #xF4)))))
    (ok (= 1 (smf-format smf)))
    (ok (= 96 (smf-division smf)))
    (ok (equalp (list (list (make-program-change-event :program 5) (make-end-of-track-event))
                      (list (make-note-on-event :key 60 :velocity 64)
                            (make-note-on-event :delta 96 :key 60 :velocity 0)))
                (smf-tracks smf))))
  (ok (equalp (list (list (make-program-change-event :program 5) (make-end-of-track-event)))
              (smf-tracks (read-invoking 'midi-protocol:use-partial-smf
                                         (octets (mthd 1 2 96) (mtrk 0 #xC0 5 +eot+)))))
      "missing track")
  (ok (equalp (list (list (make-program-change-event :program 5) (make-end-of-track-event)))
              (smf-tracks
               (handler-bind ((midi-parse-error
                                (lambda (c) (or (skip-chunk c) (use-partial-smf c)))))
                 (read-smf (octets (mthd 1 3 96) (mtrk 0 #xC0 5 +eot+) (mtrk 0 #xF4 +eot+))))))
      "a track dropped by SKIP-CHUNK is not in the partial result"))

(deftest restart-functions-without-a-restart
  (ok (null (skip-chunk)))
  (ok (null (skip-event)))
  (ok (null (use-partial-smf))))

(deftest canonical-fixtures-read-without-warnings
  (let ((bad (loop for (name) in +canonical-fixtures+
                   for warnings = (nth-value 1 (read-with-warnings (fixture-octets name)))
                   when warnings collect (list name warnings))))
    (ok (null bad) (report bad))))

;;; Writer errors --------------------------------------------------------------

(deftest-parametrize encode-errors
    ((smf event track)
     :ids ("format-0-no-tracks" "format-0-two-tracks" "track-not-a-list" "not-an-event"
           "end-of-track-not-last" "sysex-256" "sysex-keyword" "escape-negative"
           "sequencer-specific-float" "unknown-meta-300" "text-above-255" "denominator-3"
           "denominator-6" "unknown-meta-tempo-type" "unknown-meta-eot-type"
           "unknown-meta-program-name-type" "too-many-tracks")
     ((make-smf :format 0 :tracks '()) nil nil)
     ((make-smf :format 0 :tracks (list nil nil)) nil nil)
     ((make-smf :format 1 :tracks (list nil (vector (make-end-of-track-event)))) nil 1)
     ((make-smf :format 1 :tracks (list (list (make-note-on-event)) (list (make-note-on-event) 42)))
      42 1)
     ((make-smf :format 0 :tracks (list (list (make-end-of-track-event) (make-note-on-event))))
      (make-end-of-track-event) 0)
     ((make-smf :format 0 :tracks (list (list (make-sysex-event :data #(1 256)))))
      (make-sysex-event :data #(1 256)) 0)
     ((make-smf :format 0 :tracks (list (list (make-sysex-event :data #(1 :a)))))
      (make-sysex-event :data #(1 :a)) 0)
     ((make-smf :format 0 :tracks (list (list (make-escape-event :data #(-1)))))
      (make-escape-event :data #(-1)) 0)
     ((make-smf :format 0 :tracks (list (list (make-sequencer-specific-event :data #(1.5)))))
      (make-sequencer-specific-event :data #(1.5)) 0)
     ((make-smf :format 0 :tracks (list (list (make-unknown-meta-event :type #x60 :data #(300)))))
      (make-unknown-meta-event :type #x60 :data #(300)) 0)
     ((make-smf :format 0 :tracks (list (list (make-lyric-event :text (string (code-char 256))))))
      (make-lyric-event :text (string (code-char 256))) 0)
     ((make-smf :format 0 :tracks (list (list (make-time-signature-event :denominator 3))))
      (make-time-signature-event :denominator 3) 0)
     ((make-smf :format 0 :tracks (list (list (make-time-signature-event :denominator 6))))
      (make-time-signature-event :denominator 6) 0)
     ((make-smf :format 0 :tracks (list (list (make-unknown-meta-event :type #x51 :data #(1 2 3)))))
      (make-unknown-meta-event :type #x51 :data #(1 2 3)) 0)
     ((make-smf :format 0 :tracks (list (list (make-unknown-meta-event :type #x2F))))
      (make-unknown-meta-event :type #x2F) 0)
     ((make-smf :format 0 :tracks (list (list (make-unknown-meta-event :type #x08 :data #(65)))))
      (make-unknown-meta-event :type #x08 :data #(65)) 0)
     ((make-smf :format 1 :tracks (make-list 65536)) nil nil))
  (let ((c (encode-failure smf)))
    (ok (typep c 'midi-encode-error) "signals MIDI-ENCODE-ERROR")
    (when (typep c 'midi-encode-error)
      (ok (equalp event (midi-encode-error-event c)) (format nil "event ~S" event))
      (ok (eql track (midi-encode-error-track c)) (format nil "track ~S" track))
      (ok (stringp (midi-error-reason c)) "a reason"))))

(deftest writer-accepts-the-edges
  (ok (equalp (octets (mthd 1 2 96) (mtrk +eot+) (mtrk +eot+))
              (write-smf (make-smf :format 1 :division 96 :tracks (list nil nil)) nil)))
  (ok (equalp (octets (mthd 0 1 96) (mtrk 0 #xF0 2 #xFF 0 +eot+))
              (write-smf (make-smf :format 0 :division 96
                                   :tracks (list (list (make-sysex-event :data (vector 255 0)))))
                         nil))
      "a simple vector of octets")
  (ok (equalp (octets (mthd 0 1 96) (mtrk 0 #xFF 1 1 255 +eot+))
              (write-smf (make-smf :format 0 :division 96
                                   :tracks (list (list (make-text-event
                                                        :text (string (code-char 255))))))
                         nil))))
