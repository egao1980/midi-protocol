;;;; MIDI0 SMF reader.
;;;;
;;;; (READ-SMF SOURCE) returns an SMF. SOURCE is
;;;;   - a (VECTOR (UNSIGNED-BYTE 8)): offsets count from index 0;
;;;;   - a binary input stream of element type (UNSIGNED-BYTE 8): offsets count
;;;;     from its position when READ-SMF was called. READ-SMF reads chunk by
;;;;     chunk and, on success, leaves the stream just after the last track chunk
;;;;     it needed, so more data (a second SMF, say) can follow;
;;;;   - a pathname designator (pathname or string): that file, opened with
;;;;     element type (UNSIGNED-BYTE 8).
;;;; Anything else signals a TYPE-ERROR.
;;;;
;;;; Header. The source starts with an MThd chunk, all fields big-endian:
;;;;   "MThd" <length u32> <format u16> <ntrks u16> <division u16>
;;;;   - not "MThd": error at offset 0;
;;;;   - LENGTH below 6: error at 4. Bytes after the first 6 of a longer header
;;;;     are skipped;
;;;;   - FORMAT not 0, 1 or 2: error at 8. NTRKS isn't checked against FORMAT;
;;;;   - DIVISION, at 12. Bit 15 clear: ticks per quarter note, and 0 is an
;;;;     error. Bit 15 set: the high byte, read as a signed byte, is the negated
;;;;     frame rate, -24, -25, -29 or -30 (anything else is an error), and the
;;;;     low byte is ticks per frame (0 is an error). That gives an
;;;;     SMPTE-DIVISION.
;;;;
;;;; Chunks. After the header, chunks (<id: 4 bytes> <length u32> <data>) are
;;;; read until NTRKS track chunks, id "MTrk", have been read. Chunks with any
;;;; other id (alien chunks, a second MThd included) are skipped by their
;;;; length. Nothing after the NTRKS-th track chunk is read. The SMF's TRACKS
;;;; hold one track per MTrk chunk, in file order.
;;;;
;;;; Events. A track chunk holds <delta VLQ> <event> pairs, read until an
;;;; END-OF-TRACK-EVENT or the end of the chunk. The byte after the delta
;;;; decides the event (layout table in model.lisp):
;;;;   80..EF   a channel event with that status byte; its data bytes follow.
;;;;   00..7F   running status: this byte is the first data byte of a channel
;;;;            event with the last channel status byte of this track. Meta,
;;;;            sysex and escape events don't change the running status (mido
;;;;            reads the same way). Before the track's first channel event there
;;;;            is none, and the byte is an error.
;;;;   F0, F7   SYSEX-EVENT, ESCAPE-EVENT: <len VLQ> then <len> bytes.
;;;;   FF       a meta event: <type byte> <len VLQ> then <len> bytes, decoded by
;;;;            the meta table in model.lisp.
;;;;   F1..F6, F8..FE   an error: system common and real-time messages have no
;;;;            place in a file.
;;;; A channel event's data byte in 80..FF is an error. A VLQ (delta or <len>)
;;;; has at most 4 bytes.
;;;;
;;;; Error offsets (MIDI-PARSE-ERROR-OFFSET). A field is checked once all of its
;;;; bytes are there; until then, running out of bytes is the error.
;;;;   - The data runs out (in the header, a chunk header, a VLQ or an event, or
;;;;     before NTRKS track chunks were found), or an event runs past the end of
;;;;     its chunk: the end of what was available, i.e. the end of the data or
;;;;     the chunk's declared end, whichever comes first. So every proper prefix
;;;;     of a valid SMF fails at its own length.
;;;;   - A bad byte (a status byte not allowed in a file, a data byte with no
;;;;     running status, a status byte where a data byte belongs): that byte.
;;;;   - A bad field (a header field above, a VLQ whose 4th byte has bit 7 set, a
;;;;     known meta event whose length or data is invalid): the field's first
;;;;     byte; for a meta event, its FF.
;;;; MIDI-PARSE-ERROR-TRACK is the index of the MTrk chunk being read (0 for the
;;;; first), or NIL in the header, in an alien chunk and between chunks.
;;;;
;;;; When the data ends inside a track chunk, the events that end before the
;;;; data does are read first; the error comes at the first event cut off.
;;;;
;;;; Warnings: READ-SMF WARNs a MIDI-FORMAT-WARNING and goes on for
;;;;   - bytes after the END-OF-TRACK-EVENT in a chunk: offset of the first of
;;;;     them; they are skipped;
;;;;   - a track chunk that ends without an END-OF-TRACK-EVENT: offset of the
;;;;     chunk's end; the track is kept as read, with no END-OF-TRACK-EVENT added.
;;;;
;;;; Restarts (conditions.lisp):
;;;;   - USE-PARTIAL-SMF for every error after the header chunk was read. Errors
;;;;     in the header chunk have no restarts.
;;;;   - SKIP-CHUNK for every error inside a track chunk once its 8-byte chunk
;;;;     header was read. Reading goes on at the chunk's declared end, or at the
;;;;     end of the data when that comes first. The dropped chunk still counts
;;;;     toward NTRKS, so skipping the last one ends the read normally.
;;;;   - SKIP-EVENT and USE-VALUE for a known meta event whose length or data is
;;;;     invalid. Its bytes are all there, so reading goes on after them.
;;;;
;;;; Partial result (USE-PARTIAL-SMF): an SMF with the header's FORMAT and
;;;; DIVISION, the tracks completed before the error, and, when the error is
;;;; inside a track chunk, that track with the events read before the failing
;;;; one (no END-OF-TRACK-EVENT added). Tracks dropped by SKIP-CHUNK aren't in it.
;;;;
;;;; Round trip: (write-smf (read-smf bytes) nil) gives BYTES back when the file
;;;; is canonical: header length 6, no alien chunks, every track ending with its
;;;; only END-OF-TRACK-EVENT, shortest VLQs, and running status used exactly
;;;; where WRITE-SMF puts it (write.lisp). mido writes canonical files.
;;;;
;;;; Byte I/O builds on binary-protocol: UNPACK-FROM for the fixed-width
;;;; big-endian fields. Variable-length quantities and 24-bit integers are gaps
;;;; there (egao1980/binary-protocol#2, #3): add them to binary-protocol and use
;;;; them here instead of a private copy. binary-protocol#4 (UNPACK-FROM scans
;;;; the whole buffer on every call) makes field-by-field parsing quadratic until
;;;; it is fixed.

(in-package #:midi-protocol)

;;; Byte source ----------------------------------------------------------------

(defconstant +read-block+ 65536
  "The most a stream read allocates before it has seen that many bytes arrive.")

(defstruct (%reader (:constructor %make-reader (octets stream)) (:copier nil) (:predicate nil))
  "READ-SMF's state: the source (OCTETS, a simple octet vector, or STREAM), the
absolute offset POS of its next unread byte, the completed TRACKS (newest first)
and the EVENTS of the track chunk being read (newest first), or :NONE outside a
track chunk. USE-PARTIAL-SMF builds its result from TRACKS and EVENTS."
  (octets nil :type (or null octet-vector))
  (stream nil)
  (pos 0 :type (integer 0))
  (tracks '() :type list)
  (events :none))

(defun %take (reader n)
  "Consume the next N bytes of READER's source, or as many as are left. Returns
(values buffer start end base): the bytes are BUFFER[START, END), and BUFFER[I]
is at absolute offset BASE + I. A vector source is not copied. A stream is read
with READ-SEQUENCE for exactly those bytes, so nothing after them is consumed."
  (let ((pos (%reader-pos reader))
        (octets (%reader-octets reader)))
    (if octets
        (let ((end (min (length octets) (+ pos n))))
          (setf (%reader-pos reader) end)
          (values octets pos end 0))
        (let ((buffer (make-array (min n +read-block+) :element-type '(unsigned-byte 8)))
              (filled 0))
          (loop
            (setf filled (read-sequence buffer (%reader-stream reader) :start filled))
            (when (or (< filled (length buffer)) (= filled n))
              (return))
            ;; Full, and more is wanted: grow only as far as bytes keep coming.
            (let ((bigger (make-array (min n (* 2 (length buffer)))
                                      :element-type '(unsigned-byte 8))))
              (replace bigger buffer)
              (setf buffer bigger)))
          (setf (%reader-pos reader) (+ pos filled))
          (values buffer 0 filled pos)))))

(defun %skip (reader n)
  "Consume the next N bytes of READER's source, or as many as are left, without
keeping them. Returns how many were consumed."
  (let ((pos (%reader-pos reader))
        (octets (%reader-octets reader)))
    (if octets
        (let ((end (min (length octets) (+ pos n))))
          (setf (%reader-pos reader) end)
          (- end pos))
        (let ((scratch (make-array (min n 4096) :element-type '(unsigned-byte 8)))
              (done 0))
          (loop while (< done n)
                do (let* ((want (min (length scratch) (- n done)))
                          (got (read-sequence scratch (%reader-stream reader) :end want)))
                     (incf done got)
                     (when (< got want)
                       (return))))
          (setf (%reader-pos reader) (+ pos done))
          done))))

(defun %unpack (format buffer start)
  "STACK-BINARY:UNPACK of FORMAT on the bytes of BUFFER it needs from START. It
gets a fresh slice of those bytes only, never the whole buffer
(egao1980/binary-protocol#4)."
  (stack-binary:unpack format
                       (subseq buffer start (+ start (stack-binary:calcsize format)))))

(defun %id= (octets id)
  "True when the 4 OCTETS of a chunk id spell the string ID."
  (and (= (length octets) 4)
       (every (lambda (octet char) (= octet (char-code char))) octets id)))

(defun %parse-error (offset track reason)
  (error 'midi-parse-error :offset offset :track track :reason reason))

(defun %latin-1 (buffer start length)
  "The LENGTH bytes of BUFFER at START as a string, one character per byte."
  (let ((text (make-string length)))
    (dotimes (k length text)
      (setf (char text k) (code-char (aref buffer (+ start k)))))))

;;; Header and chunks ----------------------------------------------------------

(defun %read-header (reader)
  "Read the MThd chunk. Returns (values format ntrks division). No restarts."
  (multiple-value-bind (buffer start end base) (%take reader 14)
    (flet ((fail (index reason)
             (%parse-error (+ base index) nil reason))
           (need (count)
             (when (< (- end start) count)
               (%parse-error (+ base end) nil "data ends inside the header chunk"))))
      (need 4)
      (unless (%id= (first (%unpack ">4s" buffer start)) "MThd")
        (fail start "not a Standard MIDI File: no MThd chunk"))
      (need 8)
      (let ((length (first (%unpack ">I" buffer (+ start 4)))))
        (when (< length 6)
          (fail (+ start 4) (format nil "header chunk length ~D, below 6" length)))
        (need 10)
        (let ((file-format (first (%unpack ">H" buffer (+ start 8)))))
          (unless (<= file-format 2)
            (fail (+ start 8) (format nil "format ~D, not 0, 1 or 2" file-format)))
          (need 12)
          (let ((ntrks (first (%unpack ">H" buffer (+ start 10))))
                (at (+ start 12)))
            (need 14)
            (let ((division
                    (if (logbitp 15 (first (%unpack ">H" buffer at)))
                        (destructuring-bind (rate ticks) (%unpack ">bB" buffer at)
                          (unless (member (- rate) +smpte-rates+)
                            (fail at (format nil "SMPTE division of ~D frames per second"
                                             (- rate))))
                          (when (zerop ticks)
                            (fail at "SMPTE division of 0 ticks per frame"))
                          (make-smpte-division :frames-per-second (- rate)
                                               :ticks-per-frame ticks))
                        (let ((ticks (first (%unpack ">H" buffer at))))
                          (when (zerop ticks)
                            (fail at "division of 0 ticks per quarter note"))
                          ticks))))
              (let ((extra (- length 6)))
                (when (< (%skip reader extra) extra)
                  (%parse-error (%reader-pos reader) nil "data ends inside the header chunk")))
              (values file-format ntrks division))))))))

(defun %read-track-chunk (reader index)
  "Read chunks up to and including the next MTrk chunk, track INDEX, skipping
alien chunks by their length."
  (loop
    (multiple-value-bind (buffer start end base) (%take reader 8)
      (when (< (- end start) 8)
        (%parse-error (+ base end) nil "data ends inside a chunk header"))
      (destructuring-bind (id length) (%unpack ">4sI" buffer start)
        (cond ((%id= id "MTrk")
               (%read-track-body reader length index)
               (return))
              ((< (%skip reader length) length)
               (%parse-error (%reader-pos reader) nil "data ends inside a chunk")))))))

(defun %read-track-body (reader length index)
  "Read the body of an MTrk chunk of declared LENGTH into a new track, under the
SKIP-CHUNK restart."
  (multiple-value-bind (buffer start end base) (%take reader length)
    (setf (%reader-events reader) '())
    (restart-case
        (progn
          (%read-events reader buffer start end (+ start length) base index)
          (push (reverse (%reader-events reader)) (%reader-tracks reader)))
      (skip-chunk ()
        :report "Drop this track and go on after its chunk."
        nil))
    (setf (%reader-events reader) :none)))

;;; Events ---------------------------------------------------------------------

(defun %read-replacement-event ()
  (format *query-io* "~&Event to use instead (a form, evaluated): ")
  (force-output *query-io*)
  (list (eval (read *query-io*))))

(defun %decode-meta (type buffer start length)
  "Decode the meta event with type byte TYPE whose data is the LENGTH bytes of
BUFFER at START (all present). Returns (values initargs nil), the constructor
keywords besides :DELTA, or (values nil reason) when a known type has an
invalid length or data (model.lisp, meta event table)."
  (let ((struct (cdr (assoc type +meta-event-types+))))
    (flet ((at (k) (aref buffer (+ start k)))
           (data () (subseq buffer start (+ start length))))
      (macrolet ((check (test reason)
                   `(unless ,test
                      (return-from %decode-meta (values nil ,reason))))
                 (check-length (&rest lengths)
                   `(check (member length ',lengths)
                           (format nil "~(~A~) of length ~D, not ~{~D~^ or ~}"
                                   struct length ',lengths))))
        (values
         (ecase struct
           ((nil)
            (list :type type :data (data)))
           (sequence-number-event
            (check-length 0 2)
            (list :number (and (= length 2) (first (%unpack ">H" buffer start)))))
           ((text-event copyright-event track-name-event instrument-name-event lyric-event
             marker-event cue-point-event program-name-event device-name-event)
            (list :text (%latin-1 buffer start length)))
           (channel-prefix-event
            (check-length 1)
            (check (<= (at 0) 15) (format nil "channel prefix ~D, above 15" (at 0)))
            (list :channel (at 0)))
           (midi-port-event
            (check-length 1)
            (list :port (at 0)))
           (end-of-track-event
            (check-length 0)
            '())
           (tempo-event
            (check-length 3)
            (list :tempo (unpack-integer buffer 3 :offset start)))
           (smpte-offset-event
            (check-length 5)
            (let ((hours (ldb (byte 5 0) (at 0))))
              (check (not (logbitp 7 (at 0))) "SMPTE offset with bit 7 of its first byte set")
              (check (<= hours 23) (format nil "SMPTE offset hours ~D, above 23" hours))
              (check (<= (at 1) 59) (format nil "SMPTE offset minutes ~D, above 59" (at 1)))
              (check (<= (at 2) 59) (format nil "SMPTE offset seconds ~D, above 59" (at 2)))
              (check (<= (at 3) 29) (format nil "SMPTE offset frames ~D, above 29" (at 3)))
              (check (<= (at 4) 99) (format nil "SMPTE offset subframes ~D, above 99" (at 4)))
              (list :frame-rate (nth (ldb (byte 2 5) (at 0)) +smpte-rates+) :hours hours
                    :minutes (at 1) :seconds (at 2) :frames (at 3) :subframes (at 4))))
           (time-signature-event
            (check-length 4)
            (list :numerator (at 0) :denominator (ash 1 (at 1))
                  :clocks-per-click (at 2) :thirty-seconds-per-quarter (at 3)))
           (key-signature-event
            (check-length 2)
            (destructuring-bind (sharps mode) (%unpack ">bB" buffer start)
              (check (<= -7 sharps 7)
                     (format nil "key signature of ~D sharps, not -7..7" sharps))
              (check (<= mode 1) (format nil "key signature mode ~D, not 0 or 1" mode))
              (list :sharps sharps :minor-p (= mode 1))))
           (sequencer-specific-event
            (list :data (data))))
         nil)))))

(defun %read-events (reader buffer start end declared-end base index)
  "Read the events of the track chunk body BUFFER[START, END) onto READER's
EVENTS, until an END-OF-TRACK-EVENT or END. DECLARED-END is where the chunk's
length says the body ends: END is before it when the data ran out first. BUFFER[I]
is at absolute offset BASE + I; INDEX is the track's index."
  (let ((i start)
        (running nil)
        (carry 0))
    (labels ((fail (at reason)
               (%parse-error (+ base at) index reason))
             (run-out ()
               (fail end (if (< end declared-end)
                             "data ends inside a track chunk"
                             "event runs past the end of its chunk")))
             (next-byte ()
               (if (< i end)
                   (prog1 (aref buffer i) (incf i))
                   (run-out)))
             (data-byte ()
               (let ((b (next-byte)))
                 (if (< b #x80) b (fail (1- i) "data byte expected"))))
             (vlq ()
               (multiple-value-bind (value next)
                   (handler-case (decode-vlq buffer :offset i :end end)
                     (stack-binary:binary-unpack-error (c)
                       (let ((at (stack-binary:binary-error-offset c)))
                         (if (eql at end)
                             (run-out)
                             (fail at "variable-length quantity longer than 4 bytes")))))
                 (setf i next)
                 value))
             (take-bytes (count)
               (when (> count (- end i))
                 (run-out))
               (prog1 (subseq buffer i (+ i count))
                 (incf i count)))
             (channel-event (status delta)
               (destructuring-bind (type count)
                   (rest (assoc (ldb (byte 4 4) status) +channel-event-types+))
                 (let* ((channel (ldb (byte 4 0) status))
                        (a (data-byte))
                        (b (if (= count 2) (data-byte) 0)))
                   (ecase type
                     (note-off-event
                      (make-note-off-event :delta delta :channel channel :key a :velocity b))
                     (note-on-event
                      (make-note-on-event :delta delta :channel channel :key a :velocity b))
                     (poly-pressure-event
                      (make-poly-pressure-event :delta delta :channel channel :key a :pressure b))
                     (control-change-event
                      (make-control-change-event :delta delta :channel channel
                                                 :controller a :value b))
                     (program-change-event
                      (make-program-change-event :delta delta :channel channel :program a))
                     (channel-pressure-event
                      (make-channel-pressure-event :delta delta :channel channel :pressure a))
                     (pitch-bend-event
                      (make-pitch-bend-event :delta delta :channel channel
                                             :value (- (+ a (* 128 b)) 8192)))))))
             (meta-event (at delta)
               ;; Every byte of the event is checked to be there before its
               ;; type, length and data are validated.
               (let* ((type (next-byte))
                      (length (vlq))
                      (data-start i))
                 (when (> length (- end i))
                   (run-out))
                 (setf i (+ i length))
                 (multiple-value-bind (initargs problem)
                     (%decode-meta type buffer data-start length)
                   (if (null problem)
                       (apply (%event-constructor (or (cdr (assoc type +meta-event-types+))
                                                      'unknown-meta-event))
                              :delta delta initargs)
                       (restart-case
                           (error 'midi-parse-error :offset (+ base at) :track index
                                                    :reason problem)
                         (skip-event ()
                           :report "Drop this meta event; its delta moves to the next event."
                           (setf carry delta)
                           nil)
                         (use-value (value)
                           :report "Use another event, given the bad one's delta, in its place."
                           :interactive %read-replacement-event
                           (copy-event value :delta delta))))))))
      (loop
        (when (= i end)
          (when (< end declared-end)
            (run-out))
          (warn 'midi-format-warning :offset (+ base end)
                                     :reason "track chunk ends without an end-of-track event")
          (return))
        (let* ((delta-at i)
               (delta (+ (vlq) carry)))
          (when (> delta #x0FFFFFFF)
            (fail delta-at "delta time above #x0FFFFFFF after skipped events"))
          (setf carry 0)
          (let* ((at i)
                 (b (next-byte))
                 (event (cond ((< b #x80)
                               (unless running
                                 (fail at "data byte with no running status"))
                               (decf i)
                               (channel-event running delta))
                              ((< b #xF0)
                               (setf running b)
                               (channel-event b delta))
                              ((= b #xF0)
                               (make-sysex-event :delta delta :data (take-bytes (vlq))))
                              ((= b #xF7)
                               (make-escape-event :delta delta :data (take-bytes (vlq))))
                              ((= b #xFF)
                               (meta-event at delta))
                              (t
                               (fail at "system common or real-time message in a file")))))
            (when event
              (push event (%reader-events reader))
              (when (end-of-track-event-p event)
                (when (< i end)
                  (warn 'midi-format-warning :offset (+ base i)
                                             :reason "bytes after the end-of-track event"))
                (when (< end declared-end)
                  (run-out))
                (return)))))))))

;;; Entry point ----------------------------------------------------------------

(defun %input-stream-p (x)
  (and (streamp x) (input-stream-p x)))

(defun %read-smf (reader)
  (multiple-value-bind (file-format ntrks division) (%read-header reader)
    (restart-case
        (dotimes (index ntrks)
          (%read-track-chunk reader index))
      (use-partial-smf ()
        :report "Return an SMF with the tracks read before the error."
        (let ((events (%reader-events reader)))
          (unless (eq events :none)
            (push (reverse events) (%reader-tracks reader))))))
    (make-smf :format file-format :division division
              :tracks (reverse (%reader-tracks reader)))))

(defun read-smf (source)
  "Read a Standard MIDI File from SOURCE (an octet vector, a binary input
stream or a pathname designator) into an SMF. Signals MIDI-PARSE-ERROR with
restarts and warns MIDI-FORMAT-WARNING as the file header says."
  (etypecase source
    ((vector (unsigned-byte 8))
     (%read-smf (%make-reader (coerce source 'octet-vector) nil)))
    ((satisfies %input-stream-p)
     (%read-smf (%make-reader nil source)))
    ((or pathname string)
     (with-open-file (in source :element-type '(unsigned-byte 8))
       (%read-smf (%make-reader nil in))))))
