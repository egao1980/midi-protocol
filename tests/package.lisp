(defpackage #:midi-protocol/tests
  (:use #:cl #:rove #:midi-protocol))

(in-package #:midi-protocol/tests)

;;; Bytes ----------------------------------------------------------------------

(defun octets (&rest parts)
  "An octet vector from PARTS: integers 0..255, strings (one byte per
character) and lists or vectors of those, flattened in order."
  (let ((out (make-array 16 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0)))
    (labels ((add (x)
               (etypecase x
                 (integer (vector-push-extend x out))
                 (string (loop for ch across x do (vector-push-extend (char-code ch) out)))
                 (sequence (map nil #'add x)))))
      (map nil #'add parts))
    (coerce out '(simple-array (unsigned-byte 8) (*)))))

(defun u16 (n) (list (ldb (byte 8 8) n) (ldb (byte 8 0) n)))
(defun u32 (n) (list (ldb (byte 8 24) n) (ldb (byte 8 16) n) (ldb (byte 8 8) n) (ldb (byte 8 0) n)))

(defun mthd (format ntrks division)
  "A 14-byte MThd chunk. DIVISION is the raw 16-bit field."
  (octets "MThd" (u32 6) (u16 format) (u16 ntrks) (u16 division)))

(defun chunk (id &rest body)
  (let ((bytes (apply #'octets body)))
    (octets id (u32 (length bytes)) bytes)))

(defun mtrk (&rest body) (apply #'chunk "MTrk" body))

(defparameter +eot+ '(0 #xFF #x2F 0) "Delta 0 then END-OF-TRACK.")

(defun smf-bytes (format division &rest track-bodies)
  "A whole file: MThd with NTRKS = the number of TRACK-BODIES, then one MTrk each."
  (octets (mthd format (length track-bodies) division)
          (mapcar (lambda (body) (mtrk body)) track-bodies)))

(defun track-body (events &key (running-status t))
  "The bytes inside the MTrk chunk WRITE-SMF writes for the single track EVENTS."
  (let ((bytes (write-smf (make-smf :format 0 :division 96 :tracks (list events)) nil
                          :running-status running-status)))
    (subseq bytes 22)))

(defun read-track (body)
  "The single track READ-SMF reads from a format 0 file around BODY."
  (first (smf-tracks (read-smf (smf-bytes 0 96 body)))))

;;; Fixtures -------------------------------------------------------------------

(defun fixture-path (name)
  (asdf:system-relative-pathname "midi-protocol/tests" (format nil "tests/fixtures/~A" name)))

(defun fixture-octets (name)
  (with-open-file (in (fixture-path name) :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(defun fixture-sexp (name)
  "The single top-level list in tests/fixtures/NAME."
  (with-open-file (in (fixture-path name) :external-format :utf-8)
    (with-standard-io-syntax
      (let ((*read-eval* nil)
            (*package* (find-package '#:midi-protocol/tests)))
        (read in)))))

(defun text-of (x)
  "A fixture text: a string, or a list of character codes."
  (if (stringp x) x (map 'string #'code-char x)))

(defun data-of (list)
  (coerce list '(simple-array (unsigned-byte 8) (*))))

(defun sexp-event (form)
  "The event for a fixture form (KIND DELTA FIELDS...), as generate-midi0.py writes it."
  (destructuring-bind (kind delta &rest f) form
    (macrolet ((text (make) `(,make :delta delta :text (text-of (first f)))))
      (ecase kind
        (:note-off (make-note-off-event :delta delta :channel (first f) :key (second f)
                                        :velocity (third f)))
        (:note-on (make-note-on-event :delta delta :channel (first f) :key (second f)
                                      :velocity (third f)))
        (:poly-pressure (make-poly-pressure-event :delta delta :channel (first f)
                                                  :key (second f) :pressure (third f)))
        (:control-change (make-control-change-event :delta delta :channel (first f)
                                                    :controller (second f) :value (third f)))
        (:program-change (make-program-change-event :delta delta :channel (first f)
                                                    :program (second f)))
        (:channel-pressure (make-channel-pressure-event :delta delta :channel (first f)
                                                        :pressure (second f)))
        (:pitch-bend (make-pitch-bend-event :delta delta :channel (first f) :value (second f)))
        (:sysex (make-sysex-event :delta delta :data (data-of (first f))))
        (:escape (make-escape-event :delta delta :data (data-of (first f))))
        (:sequence-number (make-sequence-number-event :delta delta :number (first f)))
        (:text (text make-text-event))
        (:copyright (text make-copyright-event))
        (:track-name (text make-track-name-event))
        (:instrument-name (text make-instrument-name-event))
        (:lyric (text make-lyric-event))
        (:marker (text make-marker-event))
        (:cue-point (text make-cue-point-event))
        (:program-name (text make-program-name-event))
        (:device-name (text make-device-name-event))
        (:channel-prefix (make-channel-prefix-event :delta delta :channel (first f)))
        (:midi-port (make-midi-port-event :delta delta :port (first f)))
        (:end-of-track (make-end-of-track-event :delta delta))
        (:tempo (make-tempo-event :delta delta :tempo (first f)))
        (:smpte-offset (destructuring-bind (rate h m s fr sub) f
                         (make-smpte-offset-event :delta delta :frame-rate rate :hours h
                                                  :minutes m :seconds s :frames fr
                                                  :subframes sub)))
        (:time-signature (destructuring-bind (n d c b) f
                           (make-time-signature-event :delta delta :numerator n :denominator d
                                                      :clocks-per-click c
                                                      :thirty-seconds-per-quarter b)))
        (:key-signature (make-key-signature-event :delta delta :sharps (first f)
                                                  :minor-p (second f)))
        (:sequencer-specific (make-sequencer-specific-event :delta delta
                                                            :data (data-of (first f))))
        (:unknown-meta (make-unknown-meta-event :delta delta :type (first f)
                                                :data (data-of (second f))))))))

(defun sexp-division (x)
  (if (consp x)
      (make-smpte-division :frames-per-second (second x) :ticks-per-frame (third x))
      x))

(defun sexp-smf (format division tracks)
  (make-smf :format format :division (sexp-division division)
            :tracks (mapcar (lambda (track) (mapcar #'sexp-event track)) tracks)))

(defparameter *fixtures* (fixture-sexp "midi0-files.sexp")
  "(file format division tracks) for each .mid fixture, as mido read it.")

(defun fixture-smf (name)
  "The SMF mido read from fixture NAME."
  (destructuring-bind (format division tracks)
      (rest (or (assoc name *fixtures* :test #'string=) (error "no fixture ~A" name)))
    (sexp-smf format division tracks)))

(defparameter +canonical-fixtures+
  '(("type0-melody.mid" t) ("type1-ensemble.mid" t) ("vlq-and-lengths.mid" t)
    ("smpte-division.mid" t) ("type0-empty-track.mid" t) ("type1-no-tracks.mid" t)
    ("type0-merged.mid" t) ("type1-ensemble-no-rs.mid" nil))
  "Each fixture with the RUNNING-STATUS that writes it back byte for byte.")

;;; Conditions -----------------------------------------------------------------

(defun parse-failure (thunk)
  "The MIDI-PARSE-ERROR THUNK signals (warnings muffled), or :NONE when it
returns. Other errors propagate, so a stub fails the test."
  (handler-case (handler-bind ((midi-format-warning #'muffle-warning))
                  (funcall thunk)
                  :none)
    (midi-parse-error (c) c)))

(defun failure-offset (bytes)
  "(offset track) of the MIDI-PARSE-ERROR reading BYTES, or :NONE."
  (let ((c (parse-failure (lambda () (read-smf bytes)))))
    (if (eq c :none)
        :none
        (list (midi-parse-error-offset c) (midi-parse-error-track c)))))

(defun encode-failure (smf)
  "The MIDI-ENCODE-ERROR writing SMF signals, or :NONE."
  (handler-case (progn (write-smf smf nil) :none)
    (midi-encode-error (c) c)))

(defun read-with-warnings (source)
  "(values smf warnings) reading SOURCE, collecting and muffling MIDI-FORMAT-WARNINGs."
  (let ((warnings '()))
    (values (handler-bind ((midi-format-warning
                             (lambda (w) (push w warnings) (muffle-warning w))))
              (read-smf source))
            (nreverse warnings))))

(defun read-invoking (restart source &rest args)
  "READ-SMF SOURCE, answering every MIDI-PARSE-ERROR with RESTART (a symbol in
midi-protocol, or CL:USE-VALUE) and ARGS. Warnings are muffled."
  (handler-bind ((midi-format-warning #'muffle-warning)
                 (midi-parse-error
                   (lambda (c)
                     (let ((r (find-restart restart c)))
                       (when r (apply #'invoke-restart r args))))))
    (read-smf source)))

(defun restarts-offered (bytes)
  "The midi-protocol restarts (and USE-VALUE) visible at the MIDI-PARSE-ERROR
reading BYTES, as a sorted list of names; :NONE when there is no error."
  (block nil
    (handler-bind ((midi-format-warning #'muffle-warning)
                   (midi-parse-error
                     (lambda (c)
                       (return
                         (sort (loop for name in '(midi-protocol:skip-chunk
                                                   midi-protocol:skip-event
                                                   midi-protocol:use-partial-smf
                                                   use-value)
                                     when (find-restart name c) collect name)
                               #'string< :key #'symbol-name)))))
      (read-smf bytes)
      :none)))

;;; Reporting ------------------------------------------------------------------

(defun report (bad)
  (format nil "~D mismatches~@[, first: ~S~]" (length bad)
          (and bad (subseq bad 0 (min 5 (length bad))))))

;;; Deterministic pseudo-random numbers (a 31-bit LCG, the same on every Lisp) ---

(defvar *seed* 1)

(defun rnd (n)
  "An integer in [0, N)."
  (setf *seed* (mod (+ (* *seed* 1103515245) 12345) (expt 2 31)))
  (mod (ash *seed* -8) n))

(defun rnd-pick (&rest choices) (nth (rnd (length choices)) choices))
