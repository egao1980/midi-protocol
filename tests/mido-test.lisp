;;;; MIDI0 against mido 1.3.3 (MIT): files mido wrote, read as mido reads them;
;;;; the same events written byte for byte as mido writes them; and a live
;;;; cross-read where mido reads what WRITE-SMF wrote (out of process, skipped
;;;; when uv is absent). Fixtures: tests/fixtures/PROVENANCE.md.

(in-package #:midi-protocol/tests)

(deftest fixture-table
  (ok (= 8 (length *fixtures*)))
  (ok (= 8 (length +canonical-fixtures+)))
  (ok (every (lambda (entry) (assoc (first entry) *fixtures* :test #'string=))
             +canonical-fixtures+)))

(deftest-parametrize reads-what-mido-read
    ((name) :rows (mapcar #'first +canonical-fixtures+))
  (let ((ours (read-smf (fixture-octets name)))
        (mido (fixture-smf name)))
    (ok (= (smf-format mido) (smf-format ours)))
    (ok (equalp (smf-division mido) (smf-division ours)))
    (ok (= (length (smf-tracks mido)) (length (smf-tracks ours))))
    (ok (equalp mido ours) "every event")))

(deftest-parametrize writes-what-mido-wrote
    ((name running-status) :rows +canonical-fixtures+)
  (ok (equalp (fixture-octets name)
              (write-smf (fixture-smf name) nil :running-status running-status))))

(deftest-parametrize fixture-round-trip
    ((name running-status) :rows +canonical-fixtures+)
  (let ((bytes (fixture-octets name)))
    (ok (equalp bytes (write-smf (read-smf bytes) nil :running-status running-status)))))

(deftest running-status-twins-read-alike
  (let ((with (read-smf (fixture-octets "type1-ensemble.mid")))
        (without (read-smf (fixture-octets "type1-ensemble-no-rs.mid"))))
    (ok (equalp with without))
    (ok (equalp (fixture-octets "type1-ensemble-no-rs.mid") (write-smf with nil :running-status nil)))
    (ok (equalp (fixture-octets "type1-ensemble.mid") (write-smf without nil)))))

(deftest merged-fixture-is-format-0-of-the-ensemble
  (let ((ensemble (read-smf (fixture-octets "type1-ensemble.mid"))))
    (ok (equalp (fixture-octets "type0-merged.mid")
                (write-smf (make-smf :format 0 :division (smf-division ensemble)
                                     :tracks (list (merge-tracks (smf-tracks ensemble))))
                           nil)))))

;;; Live cross-read ------------------------------------------------------------

(defun random-mido-event ()
  "A random event mido 1.3.3 reads back unchanged: no escape events, sysex
without 8-bit bytes, no length-0 sequence numbers, and meta types mido doesn't
know (08, unknown) only at delta 0, since mido drops their delta when it reads."
  (let ((delta (rnd-pick 0 0 0 (rnd 128) (rnd 20000) (rnd-pick #x7F #x80 #x3FFF #x4000 #x0FFFFFFF)))
        (channel (rnd 16)))
    (ecase (rnd 22)
      (0 (make-note-on-event :delta delta :channel channel :key (rnd 128) :velocity (rnd 128)))
      (1 (make-note-off-event :delta delta :channel channel :key (rnd 128) :velocity (rnd 128)))
      (2 (make-poly-pressure-event :delta delta :channel channel :key (rnd 128) :pressure (rnd 128)))
      (3 (make-control-change-event :delta delta :channel channel :controller (rnd 128)
                                    :value (rnd 128)))
      (4 (make-program-change-event :delta delta :channel channel :program (rnd 128)))
      (5 (make-channel-pressure-event :delta delta :channel channel :pressure (rnd 128)))
      (6 (make-pitch-bend-event :delta delta :channel channel :value (- (rnd 16384) 8192)))
      (7 (make-sysex-event :delta delta
                           :data (octets (loop repeat (rnd 40) collect (rnd 128)) #xF7)))
      (8 (make-sequence-number-event :delta delta :number (rnd 65536)))
      (9 (make-text-event :delta delta :text (random-text)))
      (10 (make-lyric-event :delta delta :text (random-text)))
      (11 (make-marker-event :delta delta :text (random-text)))
      (12 (make-program-name-event :delta 0 :text (random-text)))
      (13 (make-channel-prefix-event :delta delta :channel channel))
      (14 (make-midi-port-event :delta delta :port (rnd 256)))
      (15 (make-tempo-event :delta delta :tempo (rnd #x1000000)))
      (16 (make-smpte-offset-event :delta delta :frame-rate (rnd-pick 24 25 29 30) :hours (rnd 24)
                                   :minutes (rnd 60) :seconds (rnd 60) :frames (rnd 30)
                                   :subframes (rnd 100)))
      (17 (make-time-signature-event :delta delta :numerator (1+ (rnd 32))
                                     :denominator (expt 2 (rnd 7)) :clocks-per-click (rnd 256)
                                     :thirty-seconds-per-quarter (rnd 256)))
      (18 (make-key-signature-event :delta delta :sharps (- (rnd 15) 7) :minor-p (= 1 (rnd 2))))
      (19 (make-sequencer-specific-event :delta delta
                                         :data (octets (loop repeat (rnd 20) collect (rnd 256)))))
      (20 (make-unknown-meta-event :delta 0 :type (rnd-pick #x0A #x0F #x60 #x7E)
                                   :data (octets (loop repeat (rnd 10) collect (rnd 256)))))
      (21 (make-copyright-event :delta delta :text (random-text))))))

(defun random-text ()
  (map 'string #'code-char (loop repeat (rnd 30) collect (rnd 256))))

(defun random-smf (event-fn)
  (let* ((format (rnd 2))
         (tracks (loop repeat (if (zerop format) 1 (1+ (rnd 4)))
                       collect (append (loop repeat (rnd 25) collect (funcall event-fn))
                                       (list (make-end-of-track-event :delta (rnd 1000)))))))
    (make-smf :format format
              :division (if (zerop (rnd 5))
                            (make-smpte-division :frames-per-second (rnd-pick 24 25 29 30)
                                                 :ticks-per-frame (1+ (rnd 255)))
                            (1+ (rnd 32767)))
              :tracks tracks)))

(defun uv-available-p ()
  (handler-case (zerop (stack-process:run '("uv" "--version") :output nil :error-output nil
                                                              :timeout 60))
    (error () nil)))

(deftest mido-reads-what-we-write
  (if (not (uv-available-p))
      (skip "uv not found: the live mido cross-read needs uv on PATH")
      (let* ((*seed* 20261123)
             (smfs (loop repeat 60 collect (random-smf #'random-mido-event)))
             (bytes (apply #'octets
                           (loop for smf in smfs
                                 for i from 0
                                 collect (write-smf smf nil :running-status (evenp i))))))
        (multiple-value-bind (code out err)
            (stack-process:run (list "uv" "run" "--no-project" "--with" "mido==1.3.3"
                                     "python" "-I"
                                     (namestring (fixture-path "generate-midi0.py")) "--dump")
                               :input bytes :output :string :error-output :string :timeout 600
                               :env '(("PYTHONIOENCODING" . "utf-8")))
          (ok (zerop code) err)
          (let ((entries (with-standard-io-syntax
                           (let ((*read-eval* nil)
                                 (*package* (find-package '#:midi-protocol/tests)))
                             (read-from-string out)))))
            (ok (= (length smfs) (length entries)))
            (let ((bad (loop for smf in smfs
                             for entry in entries
                             for i from 0
                             unless (equalp smf (apply #'sexp-smf entry))
                               collect i)))
              (ok (null bad) (report bad))))))))
