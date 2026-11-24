;;;; MIDI0 codec: the wire layout of every event, VLQs, running status, the
;;;; header, end of track, sources and destinations. Expected bytes follow the
;;;; tables in src/model.lisp and the rules in src/read.lisp and src/write.lisp.

(in-package #:midi-protocol/tests)

;;; Event layout ---------------------------------------------------------------

(deftest-parametrize event-wire-layout
    ((event bytes)
     ((make-note-off-event :channel 3 :key 60 :velocity 64) '(#x83 #x3C #x40))
     ((make-note-on-event :channel 0 :key 60 :velocity 100) '(#x90 #x3C #x64))
     ((make-note-on-event :channel 15 :key 127 :velocity 0) '(#x9F #x7F #x00))
     ((make-poly-pressure-event :channel 15 :key 127 :pressure 1) '(#xAF #x7F #x01))
     ((make-control-change-event :channel 1 :controller 7 :value 100) '(#xB1 #x07 #x64))
     ((make-control-change-event :channel 0 :controller 123 :value 0) '(#xB0 #x7B #x00))
     ((make-program-change-event :channel 9 :program 0) '(#xC9 #x00))
     ((make-program-change-event :channel 0 :program 127) '(#xC0 #x7F))
     ((make-channel-pressure-event :channel 2 :pressure 90) '(#xD2 #x5A))
     ((make-pitch-bend-event :value 0) '(#xE0 #x00 #x40))
     ((make-pitch-bend-event :value -8192) '(#xE0 #x00 #x00))
     ((make-pitch-bend-event :value 8191) '(#xE0 #x7F #x7F))
     ((make-pitch-bend-event :channel 5 :value 1) '(#xE5 #x01 #x40))
     ((make-pitch-bend-event :value -1) '(#xE0 #x7F #x3F))
     ((make-sysex-event :data (octets #x7E #x7F #x09 #x01 #xF7))
      '(#xF0 #x05 #x7E #x7F #x09 #x01 #xF7))
     ((make-sysex-event :data (octets #x43 #x12)) '(#xF0 #x02 #x43 #x12))
     ((make-sysex-event :data (octets)) '(#xF0 #x00))
     ((make-sysex-event :data (octets #x80 #xFF #xF7)) '(#xF0 #x03 #x80 #xFF #xF7))
     ((make-escape-event :data (octets #x01 #x02 #xF7)) '(#xF7 #x03 #x01 #x02 #xF7))
     ((make-escape-event :data (octets #xF8)) '(#xF7 #x01 #xF8))
     ((make-sequence-number-event :number 7) '(#xFF #x00 #x02 #x00 #x07))
     ((make-sequence-number-event :number 65535) '(#xFF #x00 #x02 #xFF #xFF))
     ((make-sequence-number-event :number nil) '(#xFF #x00 #x00))
     ((make-text-event :text "abc") '(#xFF #x01 #x03 #x61 #x62 #x63))
     ((make-text-event :text "") '(#xFF #x01 #x00))
     ((make-text-event :text "Ab") '(#xFF #x01 #x02 #x41 #x62))
     ((make-text-event :text (string (code-char 233))) '(#xFF #x01 #x01 #xE9))
     ((make-text-event :text (map 'string #'code-char '(0 10 13 34 92 255)))
      '(#xFF #x01 #x06 #x00 #x0A #x0D #x22 #x5C #xFF))
     ((make-copyright-event :text "C") '(#xFF #x02 #x01 #x43))
     ((make-track-name-event :text "T") '(#xFF #x03 #x01 #x54))
     ((make-instrument-name-event :text "I") '(#xFF #x04 #x01 #x49))
     ((make-lyric-event :text "la") '(#xFF #x05 #x02 #x6C #x61))
     ((make-marker-event :text "M") '(#xFF #x06 #x01 #x4D))
     ((make-cue-point-event :text "Q") '(#xFF #x07 #x01 #x51))
     ((make-program-name-event :text "P") '(#xFF #x08 #x01 #x50))
     ((make-device-name-event :text "D") '(#xFF #x09 #x01 #x44))
     ((make-channel-prefix-event :channel 15) '(#xFF #x20 #x01 #x0F))
     ((make-midi-port-event :port 255) '(#xFF #x21 #x01 #xFF))
     ((make-tempo-event :tempo 500000) '(#xFF #x51 #x03 #x07 #xA1 #x20))
     ((make-tempo-event :tempo #xFFFFFF) '(#xFF #x51 #x03 #xFF #xFF #xFF))
     ((make-tempo-event :tempo 0) '(#xFF #x51 #x03 #x00 #x00 #x00))
     ((make-smpte-offset-event :frame-rate 30 :hours 23 :minutes 59 :seconds 59
                               :frames 29 :subframes 99)
      '(#xFF #x54 #x05 #x77 #x3B #x3B #x1D #x63))
     ((make-smpte-offset-event :frame-rate 29 :hours 1 :minutes 2 :seconds 3
                               :frames 4 :subframes 5)
      '(#xFF #x54 #x05 #x41 #x02 #x03 #x04 #x05))
     ((make-smpte-offset-event :frame-rate 25) '(#xFF #x54 #x05 #x20 0 0 0 0))
     ((make-smpte-offset-event :frame-rate 24) '(#xFF #x54 #x05 #x00 0 0 0 0))
     ((make-time-signature-event :numerator 6 :denominator 8 :clocks-per-click 36)
      '(#xFF #x58 #x04 #x06 #x03 #x24 #x08))
     ((make-time-signature-event) '(#xFF #x58 #x04 #x04 #x02 #x18 #x08))
     ((make-time-signature-event :numerator 5 :denominator 1) '(#xFF #x58 #x04 #x05 #x00 #x18 #x08))
     ((make-time-signature-event :numerator 3 :denominator 32 :thirty-seconds-per-quarter 16)
      '(#xFF #x58 #x04 #x03 #x05 #x18 #x10))
     ((make-key-signature-event :sharps -3 :minor-p t) '(#xFF #x59 #x02 #xFD #x01))
     ((make-key-signature-event :sharps 7) '(#xFF #x59 #x02 #x07 #x00))
     ((make-key-signature-event :sharps -7) '(#xFF #x59 #x02 #xF9 #x00))
     ((make-sequencer-specific-event :data (octets 0 0 #x41 1))
      '(#xFF #x7F #x04 #x00 #x00 #x41 #x01))
     ((make-unknown-meta-event :type #x60 :data (octets 1 2 3)) '(#xFF #x60 #x03 #x01 #x02 #x03))
     ((make-unknown-meta-event :type #x0A :data (octets)) '(#xFF #x0A #x00))
     ((make-unknown-meta-event :type #xFF :data (octets 9)) '(#xFF #xFF #x01 #x09)))
  (let ((body (octets 0 bytes +eot+)))
    (ok (equalp body (track-body (list event) :running-status nil)) "written")
    (ok (equalp body (track-body (list event) :running-status t)) "a lone event has no running status")
    (let ((read (read-track body)))
      (ok (equalp (list event (make-end-of-track-event)) read) "read back")
      (when (typep event 'text-meta-event)
        (ok (string= (text-meta-event-text event) (text-meta-event-text (first read)))
            "text exactly (EQUALP ignores case)")))))

(deftest read-data-is-a-simple-octet-vector
  (let ((e (first (read-track (octets 0 #xF0 2 1 #xF7 +eot+)))))
    (ok (typep (sysex-event-data e) '(simple-array (unsigned-byte 8) (*))))
    (ok (equalp #(1 #xF7) (sysex-event-data e)))))

;;; Variable-length quantities -------------------------------------------------

(deftest-parametrize delta-vlq
    ((delta vlq)
     ;; The table in the SMF 1.0 spec.
     (0 '(#x00)) (#x40 '(#x40)) (#x7F '(#x7F)) (#x80 '(#x81 #x00)) (#x2000 '(#xC0 #x00))
     (#x3FFF '(#xFF #x7F)) (#x4000 '(#x81 #x80 #x00)) (#x100000 '(#xC0 #x80 #x00))
     (#x1FFFFF '(#xFF #xFF #x7F)) (#x200000 '(#x81 #x80 #x80 #x00))
     (#x8000000 '(#xC0 #x80 #x80 #x00)) (#x0FFFFFFF '(#xFF #xFF #xFF #x7F)))
  (let ((body (octets vlq #x90 #x3C #x40 +eot+))
        (event (make-note-on-event :delta delta :key 60 :velocity 64)))
    (ok (equalp body (track-body (list event))))
    (ok (equalp (list event (make-end-of-track-event)) (read-track body)))))

(deftest-parametrize length-vlq
    ((n len)
     (0 '(#x00)) (127 '(#x7F)) (128 '(#x81 #x00)) (300 '(#x82 #x2C))
     (16383 '(#xFF #x7F)) (16384 '(#x81 #x80 #x00)))
  (let* ((text (make-string n :initial-element #\a))
         (data (make-array n :element-type '(unsigned-byte 8) :initial-element 7))
         (text-body (octets 0 #xFF #x01 len text +eot+))
         (sysex-body (octets 0 #xF0 len data +eot+)))
    (ok (equalp text-body (track-body (list (make-text-event :text text)))))
    (ok (equalp sysex-body (track-body (list (make-sysex-event :data data)))))
    (ok (string= text (text-meta-event-text (first (read-track text-body)))))
    (ok (equalp data (sysex-event-data (first (read-track sysex-body)))))))

(deftest-parametrize longer-vlqs-read
    ((vlq value)
     ('(#x80 #x00) 0) ('(#x80 #x80 #x80 #x00) 0) ('(#x80 #x81 #x00) 128)
     ('(#xFF #xFF #xFF #x7F) #x0FFFFFFF))
  ;; Not the shortest form, but at most 4 bytes: accepted.
  (ok (= value (midi-event-delta (first (read-track (octets vlq #xC0 1 +eot+)))))))

(deftest longer-length-vlq-reads
  (ok (equalp (list (make-sysex-event :data #(1 2 #xF7)) (make-end-of-track-event))
              (read-track (octets 0 #xF0 #x80 #x80 #x03 1 2 #xF7 +eot+)))))

;;; Running status -------------------------------------------------------------

(defun running-status-track ()
  (list (make-note-on-event :key 60 :velocity 100)
        (make-note-on-event :delta 96 :key 60 :velocity 0)
        (make-note-on-event :channel 1 :key 60 :velocity 100)
        (make-note-on-event :channel 1 :key 64 :velocity 100)
        (make-lyric-event :text "a")
        (make-note-on-event :channel 1 :key 67 :velocity 100)
        (make-control-change-event :channel 1 :controller 64 :value 127)
        (make-control-change-event :delta 10 :channel 1 :controller 64 :value 0)
        (make-sysex-event :data (octets 1 #xF7))
        (make-control-change-event :channel 1 :controller 64 :value 1)
        (make-escape-event :data (octets #xF8))
        (make-control-change-event :channel 1 :controller 64 :value 2)
        (make-program-change-event :channel 1 :program 5)
        (make-program-change-event :channel 1 :program 6)
        (make-end-of-track-event)))

(defparameter +with-running-status+
  '(#x00 #x90 #x3C #x64   #x60 #x3C #x00        #x00 #x91 #x3C #x64   #x00 #x40 #x64
    #x00 #xFF #x05 #x01 #x61                    #x00 #x91 #x43 #x64
    #x00 #xB1 #x40 #x7F   #x0A #x40 #x00        #x00 #xF0 #x02 #x01 #xF7
    #x00 #xB1 #x40 #x01   #x00 #xF7 #x01 #xF8   #x00 #xB1 #x40 #x02
    #x00 #xC1 #x05        #x00 #x06             #x00 #xFF #x2F #x00))

(defparameter +without-running-status+
  '(#x00 #x90 #x3C #x64   #x60 #x90 #x3C #x00   #x00 #x91 #x3C #x64   #x00 #x91 #x40 #x64
    #x00 #xFF #x05 #x01 #x61                    #x00 #x91 #x43 #x64
    #x00 #xB1 #x40 #x7F   #x0A #xB1 #x40 #x00   #x00 #xF0 #x02 #x01 #xF7
    #x00 #xB1 #x40 #x01   #x00 #xF7 #x01 #xF8   #x00 #xB1 #x40 #x02
    #x00 #xC1 #x05        #x00 #xC1 #x06        #x00 #xFF #x2F #x00))

(deftest writer-running-status
  (ok (equalp (octets +with-running-status+) (track-body (running-status-track))) "default T")
  (ok (equalp (octets +with-running-status+)
              (track-body (running-status-track) :running-status t)))
  (ok (equalp (octets +without-running-status+)
              (track-body (running-status-track) :running-status nil))))

(deftest reader-running-status
  (ok (equalp (running-status-track) (read-track (octets +with-running-status+))))
  (ok (equalp (running-status-track) (read-track (octets +without-running-status+)))))

(deftest running-status-survives-meta-and-sysex-on-read
  ;; The writer always repeats the status after these; the reader accepts files that don't.
  (ok (equalp (list (make-note-on-event :key 60 :velocity 100)
                    (make-text-event :text "A")
                    (make-note-on-event :key 62 :velocity 100)
                    (make-sysex-event :data (octets #xF7))
                    (make-note-on-event :key 64 :velocity 100)
                    (make-escape-event :data (octets #xF8))
                    (make-note-on-event :key 65 :velocity 100)
                    (make-tempo-event :tempo 400000)
                    (make-note-on-event :key 67 :velocity 100)
                    (make-end-of-track-event))
              (read-track (octets 0 #x90 60 100   0 #xFF 1 1 65   0 62 100   0 #xF0 1 #xF7
                                  0 64 100   0 #xF7 1 #xF8   0 65 100
                                  0 #xFF #x51 3 #x06 #x1A #x80   0 67 100   +eot+)))))

;;; Header ---------------------------------------------------------------------

(deftest header-fields
  (let ((smf (read-smf (mthd 1 0 480))))
    (ok (= 1 (smf-format smf)))
    (ok (= 480 (smf-division smf)))
    (ok (null (smf-tracks smf))))
  (ok (equalp (mthd 1 0 480) (write-smf (make-smf :format 1 :division 480) nil)))
  (ok (equalp (octets (mthd 0 1 96) (mtrk +eot+))
              (write-smf (make-smf :format 0 :division 96 :tracks (list (list))) nil))))

(deftest-parametrize ticks-per-quarter
    ((division) (1) (96) (480) (32767))
  (let ((bytes (octets (mthd 1 1 division) (mtrk +eot+))))
    (ok (eql division (smf-division (read-smf bytes))))
    (ok (equalp bytes (write-smf (read-smf bytes) nil)))))

(deftest-parametrize smpte-division
    ((raw fps ticks)
     (#xE728 25 40) (#xE804 24 4) (#xE350 29 80) (#xE264 30 100) (#xE701 25 1) (#xE8FF 24 255))
  (let ((bytes (octets (mthd 1 1 raw) (mtrk +eot+)))
        (division (make-smpte-division :frames-per-second fps :ticks-per-frame ticks)))
    (ok (equalp division (smf-division (read-smf bytes))))
    (ok (equalp bytes (write-smf (make-smf :format 1 :division division
                                           :tracks (list (list (make-end-of-track-event))))
                                 nil)))))

(deftest-parametrize formats
    ((format) (0) (1) (2))
  (let ((bytes (octets (mthd format 1 96) (mtrk 0 #xC0 1 +eot+))))
    (ok (= format (smf-format (read-smf bytes))))
    (ok (equalp bytes (write-smf (read-smf bytes) nil)))))

(deftest format-0-track-count-is-not-checked-on-read
  (ok (= 2 (length (smf-tracks (read-smf (smf-bytes 0 96 +eot+ +eot+)))))))

(deftest longer-header-is-skipped
  (let ((smf (read-smf (octets "MThd" (u32 8) (u16 0) (u16 1) (u16 96) #xAA #xBB
                               (mtrk 0 #xC0 1 +eot+)))))
    (ok (= 96 (smf-division smf)))
    (ok (equalp (list (list (make-program-change-event :program 1) (make-end-of-track-event)))
                (smf-tracks smf)))))

(deftest alien-chunks-are-skipped
  (multiple-value-bind (smf warnings)
      (read-with-warnings
       (octets (mthd 1 2 96) (chunk "XFIH" 1 2 3) (mtrk +eot+) (chunk "MThd" (mthd 0 1 96))
               (chunk "JUNK") (mtrk 0 #xC0 2 +eot+)))
    (ok (null warnings))
    (ok (equalp (list (list (make-end-of-track-event))
                      (list (make-program-change-event :program 2) (make-end-of-track-event)))
                (smf-tracks smf)))))

(deftest data-after-the-last-track-is-not-read
  (multiple-value-bind (smf warnings)
      (read-with-warnings (octets (smf-bytes 0 96 (list 0 #xC0 3 +eot+)) "junk" #xF4 #x00))
    (ok (null warnings))
    (ok (equalp (list (list (make-program-change-event :program 3) (make-end-of-track-event)))
                (smf-tracks smf)))))

;;; End of track ---------------------------------------------------------------

(deftest writer-appends-end-of-track
  (ok (equalp (octets 0 #x90 #x3C #x40 +eot+) (track-body (list (make-note-on-event)))))
  (ok (equalp (octets +eot+) (track-body (list))))
  (ok (equalp (octets 5 #xFF #x2F 0) (track-body (list (make-end-of-track-event :delta 5)))))
  (ok (equalp (octets 0 #xC0 1 #x81 #x40 #xFF #x2F 0)
              (track-body (list (make-program-change-event :program 1)
                                (make-end-of-track-event :delta 192))))))

(deftest end-of-track-delta-is-kept
  (let ((track (list (make-note-on-event) (make-note-on-event :delta 96 :velocity 0)
                     (make-end-of-track-event :delta 480))))
    (ok (equalp track (read-track (track-body track))))))

(deftest bytes-after-end-of-track-warn
  (multiple-value-bind (smf warnings)
      (read-with-warnings (smf-bytes 1 96 (list +eot+ 0 #x90 60 64) (list 0 #xC0 5 +eot+)))
    (ok (= 1 (length warnings)))
    (ok (typep (first warnings) 'warning))
    (ok (eql 26 (midi-format-warning-offset (first warnings))))
    (ok (stringp (midi-format-warning-reason (first warnings))))
    (ok (equalp (list (list (make-end-of-track-event))
                      (list (make-program-change-event :program 5) (make-end-of-track-event)))
                (smf-tracks smf)))))

(deftest missing-end-of-track-warns
  (multiple-value-bind (smf warnings) (read-with-warnings (smf-bytes 0 96 '(0 #x90 60 64)))
    (ok (= 1 (length warnings)))
    (ok (eql 26 (midi-format-warning-offset (first warnings))))
    (ok (equalp (list (list (make-note-on-event :key 60 :velocity 64))) (smf-tracks smf))
        "kept as read, no END-OF-TRACK added")))

(deftest unhandled-warnings-do-not-stop-the-read
  (let ((*error-output* (make-broadcast-stream)))
    (ok (equalp (list (list (make-note-on-event :key 60 :velocity 64)))
                (smf-tracks (read-smf (smf-bytes 0 96 '(0 #x90 60 64))))))))

;;; Sources and destinations ---------------------------------------------------

(defmacro with-temp-dir ((var) &body body)
  (let ((dir (gensym "DIR")))
    `(stack-pathlib:with-temp-directory (,dir)
       (let ((,var (uiop:ensure-directory-pathname (stack-pathlib:path-pathname ,dir))))
         ,@body))))

(defun file-octets (path)
  (with-open-file (in path :element-type '(unsigned-byte 8))
    (let ((bytes (make-array (file-length in) :element-type '(unsigned-byte 8))))
      (read-sequence bytes in)
      bytes)))

(deftest read-from-every-source
  (let* ((name "type0-melody.mid")
         (expected (fixture-smf name))
         (path (fixture-path name)))
    (ok (equalp expected (read-smf (fixture-octets name))) "octet vector")
    (ok (equalp expected (read-smf path)) "pathname")
    (ok (equalp expected (read-smf (namestring path))) "namestring")
    (ok (equalp expected (with-open-file (in path :element-type '(unsigned-byte 8))
                           (read-smf in)))
        "stream")
    (let ((adjustable (make-array 0 :element-type '(unsigned-byte 8) :adjustable t
                                    :fill-pointer 0)))
      (loop for b across (fixture-octets name) do (vector-push-extend b adjustable))
      (ok (equalp expected (read-smf adjustable)) "non-simple octet vector"))
    (ok (signals (read-smf 42) 'type-error))
    (ok (signals (read-smf '(77 84 104 100)) 'type-error))))

(deftest stream-reading-stops-after-the-last-track
  (with-temp-dir (dir)
    (let ((path (merge-pathnames "two.mid" dir)))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (fixture-octets "type0-melody.mid") out)
        (write-sequence (fixture-octets "type1-ensemble.mid") out)
        (write-sequence (octets 1 2 3) out))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (ok (equalp (fixture-smf "type0-melody.mid") (read-smf in)))
        (ok (equalp (fixture-smf "type1-ensemble.mid") (read-smf in)))
        (ok (equal '(1 2 3) (list (read-byte in) (read-byte in) (read-byte in))))
        (ok (eq :eof (read-byte in nil :eof)))))))

(deftest stream-offsets-count-from-the-start-position
  (with-temp-dir (dir)
    (let ((path (merge-pathnames "two.mid" dir)))
      (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
        (write-sequence (fixture-octets "type0-melody.mid") out)
        (write-sequence (smf-bytes 0 96 (list 0 #xF4 +eot+)) out))
      (with-open-file (in path :element-type '(unsigned-byte 8))
        (read-smf in)
        (ok (equal '(23 0) (let ((c (parse-failure (lambda () (read-smf in)))))
                             (if (eq c :none)
                                 :none
                                 (list (midi-parse-error-offset c)
                                       (midi-parse-error-track c))))))))))

(deftest write-to-every-destination
  (let* ((smf (fixture-smf "type0-melody.mid"))
         (bytes (fixture-octets "type0-melody.mid"))
         (vector (write-smf smf nil)))
    (ok (typep vector '(simple-array (unsigned-byte 8) (*))))
    (ok (equalp bytes vector))
    (with-temp-dir (dir)
      (let ((path (merge-pathnames "out.mid" dir)))
        (ok (equal path (write-smf smf path)) "returns the pathname")
        (ok (equalp bytes (file-octets path)))
        (ok (signals (write-smf smf path :if-exists :error) 'file-error))
        (write-smf (make-smf :format 0 :tracks (list nil)) (namestring path))
        (ok (equalp (octets (mthd 0 1 480) (mtrk +eot+)) (file-octets path))
            "superseded by default")
        (let ((stream-path (merge-pathnames "stream.mid" dir)))
          (with-open-file (out stream-path :direction :output :element-type '(unsigned-byte 8))
            (ok (eq out (write-smf smf out))))
          (ok (equalp bytes (file-octets stream-path))))))))

(deftest encode-errors-write-nothing
  (let ((bad (make-smf :format 0 :tracks (list nil nil))))
    (with-temp-dir (dir)
      (let ((path (merge-pathnames "bad.mid" dir)))
        (ok (signals (write-smf bad path) 'midi-encode-error))
        (ok (null (probe-file path)) "no file"))
      (let ((path (merge-pathnames "stream.mid" dir)))
        (with-open-file (out path :direction :output :element-type '(unsigned-byte 8))
          (ok (signals (write-smf bad out) 'midi-encode-error)))
        (ok (= 0 (length (file-octets path))) "nothing written")))))
