;;;; Conditions and restarts (CLHS 9).
;;;;
;;;; READ-SMF signals MIDI-PARSE-ERROR for input it can't read, and WARNs with a
;;;; MIDI-FORMAT-WARNING for oddities it reads past. WRITE-SMF signals
;;;; MIDI-ENCODE-ERROR. read.lisp states exactly where each one comes from and
;;;; which byte offset it carries.
;;;;
;;;; Restarts established by READ-SMF around a MIDI-PARSE-ERROR:
;;;;
;;;;   USE-PARTIAL-SMF  For every error after the header chunk was read. READ-SMF
;;;;                    returns an SMF with the header's format and division and
;;;;                    the tracks read so far (see read.lisp, "Partial result").
;;;;   SKIP-CHUNK       For every error inside a track chunk whose 8-byte chunk
;;;;                    header was read. The track is dropped and reading goes on
;;;;                    at the chunk's declared end.
;;;;   SKIP-EVENT       For a known meta event whose length or data is invalid.
;;;;                    The event is dropped and its delta is added to the next
;;;;                    event of the track, so absolute times don't move.
;;;;   USE-VALUE        For the same errors as SKIP-EVENT. The value, a
;;;;                    MIDI-EVENT, replaces the bad event; it is given the bad
;;;;                    event's delta (COPY-EVENT ... :DELTA).
;;;;
;;;; WARN establishes MUFFLE-WARNING around every MIDI-FORMAT-WARNING. No restart
;;;; changes what WRITE-SMF does: it checks the whole SMF before writing a byte.
;;;;
;;;; The functions SKIP-CHUNK, SKIP-EVENT and USE-PARTIAL-SMF invoke the restart
;;;; of that name and return NIL when there is none, as CONTINUE and USE-VALUE do.

(in-package #:midi-protocol)

(define-condition midi-error (error)
  ((reason :initarg :reason :initform nil :reader midi-error-reason
           :documentation "A short lower-case explanation, e.g. \"data byte expected\"."))
  (:documentation "Root of every error midi-protocol signals.")
  (:report (lambda (c s)
             (format s "MIDI error~@[: ~A~]" (midi-error-reason c)))))

(define-condition midi-parse-error (midi-error parse-error)
  ((offset :initarg :offset :initform nil :reader midi-parse-error-offset
           :documentation "Zero-based byte offset from the start of the source (for a
stream, from its position when READ-SMF was called). read.lisp lists which byte
each error points at.")
   (track :initarg :track :initform nil :reader midi-parse-error-track
          :documentation "Zero-based index of the track chunk being read (counting only
MTrk chunks), or NIL when the error is outside a track chunk."))
  (:documentation "READ-SMF met bytes that are not a Standard MIDI File it can read.")
  (:report (lambda (c s)
             (format s "MIDI parse error at byte ~D~@[ in track ~D~]~@[: ~A~]"
                     (midi-parse-error-offset c)
                     (midi-parse-error-track c)
                     (midi-error-reason c)))))

(define-condition midi-encode-error (midi-error)
  ((event :initarg :event :initform nil :reader midi-encode-error-event
          :documentation "The event (or other track element) that can't be written, or NIL
when the problem is the SMF itself (e.g. format 0 with two tracks) or a whole
track (one that isn't a list).")
   (track :initarg :track :initform nil :reader midi-encode-error-track
          :documentation "Zero-based index of the track holding EVENT, or NIL."))
  (:documentation "WRITE-SMF was given an SMF it can't write.")
  (:report (lambda (c s)
             (format s "MIDI encode error~@[ in track ~D~]~@[: ~A~]~@[ (~S)~]"
                     (midi-encode-error-track c)
                     (midi-error-reason c)
                     (midi-encode-error-event c)))))

(define-condition midi-format-warning (warning)
  ((offset :initarg :offset :initform nil :reader midi-format-warning-offset
           :documentation "Byte offset, counted as for MIDI-PARSE-ERROR-OFFSET.")
   (reason :initarg :reason :initform nil :reader midi-format-warning-reason
           :documentation "A short lower-case explanation."))
  (:documentation "READ-SMF read past something the SMF spec doesn't allow but that
loses no events (see read.lisp, \"Warnings\").")
  (:report (lambda (c s)
             (format s "MIDI format warning at byte ~D~@[: ~A~]"
                     (midi-format-warning-offset c)
                     (midi-format-warning-reason c)))))

(defun skip-chunk (&optional condition)
  "Invoke the SKIP-CHUNK restart (established by READ-SMF inside a track chunk).
Return NIL when there is no such restart."
  (let ((restart (find-restart 'skip-chunk condition)))
    (when restart (invoke-restart restart))))

(defun skip-event (&optional condition)
  "Invoke the SKIP-EVENT restart (established by READ-SMF for a bad known meta
event). Return NIL when there is no such restart."
  (let ((restart (find-restart 'skip-event condition)))
    (when restart (invoke-restart restart))))

(defun use-partial-smf (&optional condition)
  "Invoke the USE-PARTIAL-SMF restart (established by READ-SMF once the header
chunk was read). Return NIL when there is no such restart."
  (let ((restart (find-restart 'use-partial-smf condition)))
    (when restart (invoke-restart restart))))
