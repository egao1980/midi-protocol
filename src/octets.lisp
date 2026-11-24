;;;; Local byte helpers for what binary-protocol 0.1.0 lacks. Each one is named
;;;; and shaped as the binary-protocol issue that asks for it proposes, and
;;;; signals binary-protocol's own conditions as that issue specifies, so moving
;;;; it there is a rename (drop it here, call STACK-BINARY:<name>):
;;;;
;;;;   ENCODE-VLQ, DECODE-VLQ          egao1980/binary-protocol#2 (variable-length
;;;;                                   quantities). DECODE-VLQ takes an extra :END.
;;;;   PACK-INTEGER, UNPACK-INTEGER    egao1980/binary-protocol#3 (24-bit and other
;;;;                                   widths). Big-endian unsigned only.
;;;;
;;;; egao1980/binary-protocol#4: UNPACK-FROM (and UNPACK) checks every element of
;;;; the whole buffer on each call. read.lisp only ever gives STACK-BINARY:UNPACK
;;;; a fresh slice of the few bytes a field needs, so reading stays linear.
;;;;
;;;; Nothing here knows about MIDI: callers check MIDI ranges and translate a
;;;; BINARY-UNPACK-ERROR into a MIDI-PARSE-ERROR at the offset it carries.

(in-package #:midi-protocol)

(deftype octet-vector ()
  "What READ-SMF returns in DATA slots and WRITE-SMF returns for a NIL destination."
  '(simple-array (unsigned-byte 8) (*)))

(defun encode-vlq (integer)
  "INTEGER as a variable-length quantity (big-endian base-128, bit 7 set on
every byte but the last), shortest form, in a fresh octet vector: 0 -> #(0),
#x80 -> #(#x81 0). Signals BINARY-PACK-ERROR for anything but a non-negative
integer."
  (unless (typep integer '(integer 0))
    (error 'stack-binary:binary-pack-error
           :message (format nil "VLQ of ~S: expected a non-negative integer" integer)))
  (let* ((count (max 1 (ceiling (integer-length integer) 7)))
         (octets (make-array count :element-type '(unsigned-byte 8))))
    (dotimes (k count octets)
      (let ((group (ldb (byte 7 (* 7 (- count k 1))) integer)))
        (setf (aref octets k) (if (< k (1- count)) (logior group #x80) group))))))

(defun decode-vlq (octets &key (offset 0) (end (length octets)) (max-bytes 4))
  "Decode the variable-length quantity at OFFSET in OCTETS, reading no byte at
or after END. Returns (values integer next-offset). Signals BINARY-UNPACK-ERROR
with :OFFSET END when END comes before its last byte, and with :OFFSET OFFSET
(its first byte) when its first MAX-BYTES bytes all have bit 7 set."
  (let ((value 0))
    (loop for i from offset below (min end (+ offset max-bytes))
          for b = (aref octets i)
          do (setf value (logior (ash value 7) (logand b #x7F)))
             (unless (logbitp 7 b)
               (return-from decode-vlq (values value (1+ i)))))
    (if (>= (- end offset) max-bytes)
        (error 'stack-binary:binary-unpack-error
               :offset offset
               :message (format nil "variable-length quantity longer than ~D bytes" max-bytes))
        (error 'stack-binary:binary-unpack-error
               :offset end
               :message "buffer ends inside a variable-length quantity"))))

(defun pack-integer (value width)
  "VALUE as a big-endian unsigned integer of WIDTH bytes, in a fresh octet
vector. Signals BINARY-PACK-ERROR when it doesn't fit."
  (unless (typep value `(unsigned-byte ,(* 8 width)))
    (error 'stack-binary:binary-pack-error
           :message (format nil "~S is not an unsigned ~D-byte integer" value width)))
  (let ((octets (make-array width :element-type '(unsigned-byte 8))))
    (dotimes (k width octets)
      (setf (aref octets k) (ldb (byte 8 (* 8 (- width k 1))) value)))))

(defun unpack-integer (octets width &key (offset 0))
  "The big-endian unsigned integer in the WIDTH bytes of OCTETS at OFFSET.
Signals BINARY-UNPACK-ERROR when OCTETS ends before them."
  (unless (<= (+ offset width) (length octets))
    (error 'stack-binary:binary-unpack-error
           :offset offset :message "buffer too short"))
  (let ((value 0))
    (dotimes (k width value)
      (setf value (logior (ash value 8) (aref octets (+ offset k)))))))
