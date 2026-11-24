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

;;; Output buffer --------------------------------------------------------------

(defun %make-output ()
  (make-array 1024 :element-type '(unsigned-byte 8) :adjustable t :fill-pointer 0))

(defun %put-byte (out byte)
  (vector-push-extend byte out))

(defun %put-octets (out octets)
  (loop for byte across octets
        do (vector-push-extend byte out)))

;;; Checks ---------------------------------------------------------------------

(defun %octets-p (x)
  "True for a vector whose every element is an integer 0..255."
  (and (vectorp x) (every (lambda (e) (typep e '(unsigned-byte 8))) x)))

(defun %latin-1-p (x)
  "True for a string whose every character has a code 0..255."
  (and (stringp x) (every (lambda (c) (< (char-code c) 256)) x)))

(defun %power-of-two-p (x)
  (and (typep x '(integer 1)) (= 1 (logcount x))))

(defun %division-ok-p (division)
  (typecase division
    ((integer 1 32767) t)
    (smpte-division
     (and (member (smpte-division-frames-per-second division) +smpte-rates+)
          (typep (smpte-division-ticks-per-frame division) '(integer 1 255))))
    (t nil)))

(defun %proper-list-p (x)
  (and (listp x) (null (cdr (last x)))))

;;; Events ---------------------------------------------------------------------

(defun %meta-payload (event fail)
  "(values type-byte data) for the meta EVENT, after checking its fields: DATA
is a vector of octets. FAIL is called with a reason for a field WRITE-SMF can't
write."
  (macrolet ((field (form type)
               `(let ((value ,form))
                  (if (typep value ',type)
                      value
                      (funcall fail (format nil "~(~A~) ~S is not of type ~S"
                                            ',(first form) value ',type))))))
    (flet ((type-byte ()
             (car (find-if (lambda (row) (typep event (cdr row))) +meta-event-types+)))
           (octets (data what)
             (if (%octets-p data)
                 data
                 (funcall fail (format nil "~(~A~) holds an element that isn't an octet" what))))
           (latin-1 (text)
             (if (%latin-1-p text)
                 (map 'octet-vector #'char-code text)
                 (funcall fail "text has a character whose code is above 255"))))
      (etypecase event
        (unknown-meta-event
         (let ((type (field (unknown-meta-event-type event) (unsigned-byte 8))))
           (when (assoc type +meta-event-types+)
             (funcall fail (format nil "unknown meta event with the known type byte #x~2,'0X"
                                   type)))
           (values type (octets (unknown-meta-event-data event) 'unknown-meta-event-data))))
        (sequence-number-event
         (let ((number (field (sequence-number-event-number event) (or null (unsigned-byte 16)))))
           (values (type-byte) (if number (stack-binary:pack ">H" number) #()))))
        (text-meta-event
         (values (type-byte) (latin-1 (field (text-meta-event-text event) string))))
        (channel-prefix-event
         (values (type-byte) (vector (field (channel-prefix-event-channel event) (integer 0 15)))))
        (midi-port-event
         (values (type-byte) (vector (field (midi-port-event-port event) (unsigned-byte 8)))))
        (end-of-track-event
         (values (type-byte) #()))
        (tempo-event
         (values (type-byte)
                 (pack-integer (field (tempo-event-tempo event) (unsigned-byte 24)) 3)))
        (smpte-offset-event
         (let ((rate (field (smpte-offset-event-frame-rate event) (member 24 25 29 30)))
               (hours (field (smpte-offset-event-hours event) (integer 0 23))))
           (values (type-byte)
                   (vector (logior (ash (position rate +smpte-rates+) 5) hours)
                           (field (smpte-offset-event-minutes event) (integer 0 59))
                           (field (smpte-offset-event-seconds event) (integer 0 59))
                           (field (smpte-offset-event-frames event) (integer 0 29))
                           (field (smpte-offset-event-subframes event) (integer 0 99))))))
        (time-signature-event
         (let ((denominator (time-signature-event-denominator event)))
           (unless (and (%power-of-two-p denominator) (<= (integer-length denominator) 256))
             (funcall fail (format nil "time signature denominator ~S is not a power of two ~
                                        from 1 to 2^255" denominator)))
           (values (type-byte)
                   (vector (field (time-signature-event-numerator event) (unsigned-byte 8))
                           (1- (integer-length denominator))
                           (field (time-signature-event-clocks-per-click event) (unsigned-byte 8))
                           (field (time-signature-event-thirty-seconds-per-quarter event)
                                  (unsigned-byte 8))))))
        (key-signature-event
         (let ((sharps (field (key-signature-event-sharps event) (integer -7 7)))
               (minor-p (field (key-signature-event-minor-p event) boolean)))
           (values (type-byte) (stack-binary:pack ">bB" sharps (if minor-p 1 0)))))
        (sequencer-specific-event
         (values (type-byte)
                 (octets (sequencer-specific-event-data event) 'sequencer-specific-event-data)))))))

(defun %channel-bytes (event fail)
  "(values status data-bytes) for the channel EVENT, after checking its fields."
  (macrolet ((field (form type)
               `(let ((value ,form))
                  (if (typep value ',type)
                      value
                      (funcall fail (format nil "~(~A~) ~S is not of type ~S"
                                            ',(first form) value ',type))))))
    (let ((nibble (first (find-if (lambda (row) (typep event (second row)))
                                  +channel-event-types+)))
          (channel (field (channel-event-channel event) (integer 0 15))))
      (values
       (logior (ash nibble 4) channel)
       (etypecase event
         (note-off-event
          (list (field (note-off-event-key event) (integer 0 127))
                (field (note-off-event-velocity event) (integer 0 127))))
         (note-on-event
          (list (field (note-on-event-key event) (integer 0 127))
                (field (note-on-event-velocity event) (integer 0 127))))
         (poly-pressure-event
          (list (field (poly-pressure-event-key event) (integer 0 127))
                (field (poly-pressure-event-pressure event) (integer 0 127))))
         (control-change-event
          (list (field (control-change-event-controller event) (integer 0 127))
                (field (control-change-event-value event) (integer 0 127))))
         (program-change-event
          (list (field (program-change-event-program event) (integer 0 127))))
         (channel-pressure-event
          (list (field (channel-pressure-event-pressure event) (integer 0 127))))
         (pitch-bend-event
          (let ((u (+ 8192 (field (pitch-bend-event-value event) (integer -8192 8191)))))
            (list (ldb (byte 7 0) u) (ldb (byte 7 7) u)))))))))

(defun %encode-event (out event previous-status running-status fail)
  "Append EVENT, its delta then its bytes, to OUT. PREVIOUS-STATUS is the status
byte of the previous event when that was a channel event, else NIL. Returns the
same for EVENT."
  (let ((delta (midi-event-delta event)))
    (unless (typep delta '(integer 0 #x0FFFFFFF))
      (funcall fail (format nil "delta ~S is not an integer 0..#x0FFFFFFF" delta)))
    (flet ((put-data (data)
             (when (> (length data) #x0FFFFFFF)
               (funcall fail "data longer than #x0FFFFFFF bytes"))
             (%put-octets out (encode-vlq (length data)))
             (%put-octets out data)))
      (%put-octets out (encode-vlq delta))
      (etypecase event
        (channel-event
         (multiple-value-bind (status data) (%channel-bytes event fail)
           (unless (and running-status (eql status previous-status))
             (%put-byte out status))
           (dolist (byte data)
             (%put-byte out byte))
           status))
        (sysex-event
         (let ((data (sysex-event-data event)))
           (unless (%octets-p data)
             (funcall fail "sysex-event-data holds an element that isn't an octet"))
           (%put-byte out #xF0)
           (put-data data)
           nil))
        (escape-event
         (let ((data (escape-event-data event)))
           (unless (%octets-p data)
             (funcall fail "escape-event-data holds an element that isn't an octet"))
           (%put-byte out #xF7)
           (put-data data)
           nil))
        (meta-event
         (multiple-value-bind (type data) (%meta-payload event fail)
           (%put-byte out #xFF)
           (%put-byte out type)
           (put-data data)
           nil))))))

;;; Tracks and file ------------------------------------------------------------

(defun %encode-track (out track index running-status)
  "Append the MTrk chunk of TRACK, the INDEX-th, to OUT."
  (unless (%proper-list-p track)
    (error 'midi-encode-error :track index :reason "track is not a list"))
  (let ((chunk-start (fill-pointer out))
        (previous-status nil)
        (ended nil))
    (%put-octets out (stack-binary:pack ">4sI" "MTrk" 0))
    (loop for (event . more) on track
          do (flet ((fail (reason)
                      (error 'midi-encode-error :event event :track index :reason reason)))
               (unless (typep event 'midi-event)
                 (fail "track element is not a MIDI-EVENT"))
               (when (and (end-of-track-event-p event) more)
                 (fail "end-of-track event before the last event of the track"))
               (setf previous-status
                     (%encode-event out event previous-status running-status #'fail)
                     ended (end-of-track-event-p event))))
    (unless ended
      (%put-octets out #(0 #xFF #x2F 0)))
    (let ((length (- (fill-pointer out) chunk-start 8)))
      (when (> length #xFFFFFFFF)
        (error 'midi-encode-error :track index :reason "track chunk longer than #xFFFFFFFF bytes"))
      (stack-binary:pack-into out ">I" (list length) :start (+ chunk-start 4)))))

(defun %encode-smf (smf running-status)
  "The whole file for SMF, as a fresh simple octet vector."
  (check-type smf smf)
  (let ((file-format (smf-format smf))
        (division (smf-division smf))
        (tracks (smf-tracks smf)))
    (flet ((fail (reason)
             (error 'midi-encode-error :reason reason)))
      (unless (typep file-format '(integer 0 2))
        (fail (format nil "format ~S, not 0, 1 or 2" file-format)))
      (unless (%division-ok-p division)
        (fail (format nil "division ~S is neither 1..32767 ticks per quarter note nor a valid ~
                           SMPTE-DIVISION" division)))
      (unless (%proper-list-p tracks)
        (fail "tracks is not a list"))
      (let ((count (length tracks)))
        (when (> count 65535)
          (fail (format nil "~D tracks, more than 65535" count)))
        (when (and (= file-format 0) (/= count 1))
          (fail (format nil "format 0 with ~D tracks, not 1" count)))
        (let ((out (%make-output)))
          (%put-octets out (stack-binary:pack ">4sIHH" "MThd" 6 file-format count))
          (%put-octets out (etypecase division
                             (integer (stack-binary:pack ">H" division))
                             (smpte-division
                              (stack-binary:pack ">bB"
                                                 (- (smpte-division-frames-per-second division))
                                                 (smpte-division-ticks-per-frame division)))))
          (loop for track in tracks
                for index from 0
                do (%encode-track out track index running-status))
          (let ((bytes (make-array (length out) :element-type '(unsigned-byte 8))))
            (replace bytes out)
            bytes))))))

(defun %output-stream-p (x)
  (and (streamp x) (output-stream-p x)))

(defun write-smf (smf destination &key (running-status t) (if-exists :supersede))
  "Write SMF as a Standard MIDI File to DESTINATION (NIL for a fresh octet
vector, a binary output stream, or a pathname designator opened with
IF-EXISTS). RUNNING-STATUS T omits repeated channel status bytes. Signals
MIDI-ENCODE-ERROR, before writing anything, for an SMF it can't write."
  (check-type destination (or null (satisfies %output-stream-p) pathname string))
  (let ((bytes (%encode-smf smf running-status)))
    (etypecase destination
      (null bytes)
      ((satisfies %output-stream-p)
       (write-sequence bytes destination)
       destination)
      ((or pathname string)
       (with-open-file (out destination :direction :output :element-type '(unsigned-byte 8)
                                        :if-exists if-exists :if-does-not-exist :create)
         ;; OUT is NIL when IF-EXISTS is NIL and the file exists: nothing is
         ;; written and the result is NIL, as with OPEN.
         (when out
           (write-sequence bytes out)
           destination))))))
