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

(defun read-smf (source)
  "Read a Standard MIDI File from SOURCE (an octet vector, a binary input
stream or a pathname designator) into an SMF. Signals MIDI-PARSE-ERROR with
restarts and warns MIDI-FORMAT-WARNING as the file header says."
  (declare (ignorable source))
  (%midi0-pending 'read-smf))
