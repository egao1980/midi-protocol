"""Generate the MIDI0 test fixtures with mido.

Run from the repo root:
    uv run --with mido==1.3.3 python -I tests/fixtures/generate-midi0.py

Writes the .mid files in FIXTURES with mido 1.3.3 (MIT), reads each one back
with mido, and records what mido read in midi0-files.sexp and mido's playback
times in midi0-seconds.sexp. type1-ensemble-no-rs.mid holds the messages of
type1-ensemble.mid without running status: it is mido's write_track with the
running-status branch removed, still using mido's own message encoders, and
mido must read it back to the same messages. See PROVENANCE.md.

With --dump it instead reads Standard MIDI Files concatenated on stdin and
prints, as one s-expression, what mido reads (the live cross-read test).
"""
import struct
import sys
from fractions import Fraction
from pathlib import Path

import mido
from mido import Message, MetaMessage, MidiFile, MidiTrack, UnknownMetaMessage
from mido.midifiles.meta import encode_variable_int
from mido.midifiles.tracks import fix_end_of_track

MIDO_VERSION = "1.3.3"
OUT = Path(__file__).resolve().parent

# --- s-expression form of what mido read ------------------------------------

TEXT_KINDS = {"text": ":text", "copyright": ":copyright", "track_name": ":track-name",
              "instrument_name": ":instrument-name", "lyrics": ":lyric",
              "marker": ":marker", "cue_marker": ":cue-point", "device_name": ":device-name"}
NAME_ATTR = ("track_name", "instrument_name", "device_name")


def lisp_text(s):
    """A Lisp string, or a list of character codes when S has control characters."""
    if all(0x20 <= ord(c) <= 0x7E or 0xA0 <= ord(c) <= 0xFF for c in s):
        return '"' + s.replace("\\", "\\\\").replace('"', '\\"') + '"'
    return "(" + " ".join(str(ord(c)) for c in s) + ")"


def octets(data):
    return "(" + " ".join(str(b) for b in data) + ")"


def event_sexp(msg):
    d, t = msg.time, msg.type
    if t in ("note_on", "note_off"):
        return f"(:{t.replace('_', '-')} {d} {msg.channel} {msg.note} {msg.velocity})"
    if t == "polytouch":
        return f"(:poly-pressure {d} {msg.channel} {msg.note} {msg.value})"
    if t == "control_change":
        return f"(:control-change {d} {msg.channel} {msg.control} {msg.value})"
    if t == "program_change":
        return f"(:program-change {d} {msg.channel} {msg.program})"
    if t == "aftertouch":
        return f"(:channel-pressure {d} {msg.channel} {msg.value})"
    if t == "pitchwheel":
        return f"(:pitch-bend {d} {msg.channel} {msg.pitch})"
    if t == "sysex":
        # mido drops the closing F7 when it reads; the model keeps the bytes verbatim.
        return f"(:sysex {d} {octets(list(msg.data) + [0xF7])})"
    if t == "sequence_number":
        return f"(:sequence-number {d} {msg.number})"
    if t in TEXT_KINDS:
        text = msg.name if t in NAME_ATTR else msg.text
        return f"({TEXT_KINDS[t]} {d} {lisp_text(text)})"
    if t == "channel_prefix":
        return f"(:channel-prefix {d} {msg.channel})"
    if t == "midi_port":
        return f"(:midi-port {d} {msg.port})"
    if t == "end_of_track":
        return f"(:end-of-track {d})"
    if t == "set_tempo":
        return f"(:tempo {d} {msg.tempo})"
    if t == "smpte_offset":
        rate = {24: 24, 25: 25, 29.97: 29, 30: 30}[msg.frame_rate]
        return (f"(:smpte-offset {d} {rate} {msg.hours} {msg.minutes} {msg.seconds} "
                f"{msg.frames} {msg.sub_frames})")
    if t == "time_signature":
        return (f"(:time-signature {d} {msg.numerator} {msg.denominator} "
                f"{msg.clocks_per_click} {msg.notated_32nd_notes_per_beat})")
    if t == "key_signature":
        sf, mi = msg.bytes()[3:5]          # FF 59 02 sf mi
        return f"(:key-signature {d} {sf - 256 if sf > 127 else sf} {'t' if mi else 'nil'})"
    if t == "sequencer_specific":
        return f"(:sequencer-specific {d} {octets(msg.data)})"
    if t == "unknown_meta":
        if msg.type_byte == 0x08:          # program name: mido has no class for it
            return f"(:program-name {d} {lisp_text(bytes(msg.data).decode('latin1'))})"
        return f"(:unknown-meta {d} {msg.type_byte} {octets(msg.data)})"
    raise ValueError(f"no s-expression form for {msg!r}")


def division_sexp(ticks_per_beat):
    if ticks_per_beat < 0:                 # mido keeps an SMPTE division as a negative short
        hi, lo = struct.pack(">h", ticks_per_beat)
        return f"(:smpte {256 - hi} {lo})"
    return str(ticks_per_beat)


def smf_sexp(mid):
    tracks = "\n   ".join("(" + "\n    ".join(event_sexp(m) for m in track) + ")"
                          for track in mid.tracks)
    return f"{mid.type} {division_sexp(mid.ticks_per_beat)}\n  ({tracks})"


# --- fixtures ---------------------------------------------------------------

def at(events):
    """A track from (tick, message) pairs, stable-sorted by tick."""
    track, now = MidiTrack(), 0
    for tick, msg in sorted(events, key=lambda e: e[0]):
        track.append(msg.copy(time=tick - now))
        now = tick
    return track


def melody():
    t = [(0, MetaMessage("track_name", name="Melody")),
         (0, MetaMessage("time_signature", numerator=3, denominator=4,
                         clocks_per_click=24, notated_32nd_notes_per_beat=8)),
         (0, MetaMessage("key_signature", key="F")),
         (0, MetaMessage("set_tempo", tempo=600000)),
         (0, Message("program_change", channel=0, program=73))]
    now = 0
    notes = [(65, 96, "One"), (67, 96, None), (69, 96, "two"), (70, 192, None),
             (72, 96, "three"), (70, 96, None), (69, 192, "four"), (67, 96, None),
             (65, 288, "five")]
    for i, (key, dur, syllable) in enumerate(notes):
        if syllable:
            t.append((now, MetaMessage("lyrics", text=syllable)))
        t.append((now, Message("note_on", channel=0, note=key, velocity=80 + i)))
        now += dur
        if i % 3 == 2:   # some note-offs are 8n, most are 9n with velocity 0
            t.append((now, Message("note_off", channel=0, note=key, velocity=64)))
        else:
            t.append((now, Message("note_on", channel=0, note=key, velocity=0)))
    t.append((now + 96, MetaMessage("end_of_track")))
    return MidiFile(type=0, ticks_per_beat=96, tracks=[at(t)])


def ensemble():
    conductor = at([
        (0, MetaMessage("sequence_number", number=1)),
        (0, MetaMessage("track_name", name="Conductor")),
        (0, MetaMessage("copyright", text="Test data, MIT")),
        (0, MetaMessage("smpte_offset", frame_rate=25, hours=1, minutes=2, seconds=3,
                        frames=4, sub_frames=5)),
        (0, MetaMessage("time_signature", numerator=4, denominator=4,
                        clocks_per_click=24, notated_32nd_notes_per_beat=8)),
        (0, MetaMessage("key_signature", key="C")),
        (0, MetaMessage("set_tempo", tempo=500000)),
        (0, MetaMessage("marker", text="A")),
        (1920, MetaMessage("time_signature", numerator=6, denominator=8,
                           clocks_per_click=36, notated_32nd_notes_per_beat=8)),
        (1920, MetaMessage("set_tempo", tempo=750000)),
        (1920, MetaMessage("marker", text="B")),
        (3360, MetaMessage("set_tempo", tempo=400000)),
        (3360, MetaMessage("set_tempo", tempo=410000)),   # same tick: the last one counts
        (3360, MetaMessage("key_signature", key="Cm")),
        (3840, MetaMessage("cue_marker", text="Cue 1")),
        (3840, MetaMessage("text", text="rit.")),
        (4800, MetaMessage("set_tempo", tempo=450000)),
        (5760, MetaMessage("end_of_track"))])

    voice = [(0, MetaMessage("track_name", name="Voice")),
             (0, MetaMessage("instrument_name", name="Lead")),
             (0, Message("program_change", channel=0, program=52)),
             (0, Message("control_change", channel=0, control=7, value=100)),
             (0, Message("control_change", channel=0, control=10, value=64))]
    for i, key in enumerate([60, 62, 64, 65, 67, 69, 71, 72]):
        start = 480 * i
        voice.append((start, MetaMessage("lyrics", text=f"syl{i}")))
        voice.append((start, Message("note_on", channel=0, note=key, velocity=90)))
        if i == 2:
            voice += [(start + 120, Message("pitchwheel", channel=0, pitch=-8192)),
                      (start + 240, Message("pitchwheel", channel=0, pitch=8191)),
                      (start + 360, Message("pitchwheel", channel=0, pitch=0))]
        if i == 4:
            voice.append((start + 120, Message("aftertouch", channel=0, value=90)))
        voice.append((start + 470, Message("note_off", channel=0, note=key, velocity=40)))
    voice += [(4000, Message("pitchwheel", channel=0, pitch=1234)),
              (5760, MetaMessage("end_of_track"))]

    bass = [(0, MetaMessage("track_name", name="Bass")),
            (0, MetaMessage("channel_prefix", channel=1)),
            (0, MetaMessage("midi_port", port=0)),
            (0, MetaMessage("device_name", name="Synth A")),
            (0, Message("program_change", channel=1, program=32)),
            (0, Message("control_change", channel=1, control=64, value=127))]
    for i, key in enumerate([36, 43, 41, 38, 36, 31]):
        start = 960 * i
        bass.append((start, Message("note_on", channel=1, note=key, velocity=100)))
        if i == 3:
            bass.append((start + 300, Message("polytouch", channel=1, note=key, value=70)))
        bass.append((start + 900, Message("note_on", channel=1, note=key, velocity=0)))
        if i == 1:
            bass.append((start + 940, Message("control_change", channel=1, control=64, value=0)))
    bass += [(5000, Message("control_change", channel=1, control=121, value=0)),
             (5760, MetaMessage("end_of_track"))]

    drums = [(0, MetaMessage("track_name", name="Drums")),
             (0, Message("sysex", data=[0x7E, 0x7F, 0x09, 0x01])),
             (0, UnknownMetaMessage(0x08, list(b"Kit 1"))),
             (0, MetaMessage("sequencer_specific", data=[0x00, 0x00, 0x41, 0x01, 0x02])),
             (0, UnknownMetaMessage(0x60, [1, 2, 3]))]
    for i in range(24):
        start = 240 * i
        drums += [(start, Message("note_on", channel=9, note=36, velocity=100)),
                  (start, Message("note_on", channel=9, note=42, velocity=80)),
                  (start + 120, Message("note_off", channel=9, note=36, velocity=0)),
                  (start + 120, Message("note_off", channel=9, note=42, velocity=0))]
    drums.append((5760, MetaMessage("end_of_track")))
    return MidiFile(type=1, ticks_per_beat=480,
                    tracks=[conductor, at(voice), at(bass), at(drums)])


def vlq_and_lengths():
    t, now = [], 0
    for i, delta in enumerate([0, 0x40, 0x7F, 0x80, 0x2000, 0x3FFF, 0x4000, 0x100000,
                               0x1FFFFF, 0x200000, 0x8000000, 0x0FFFFFFF]):
        now += delta
        t.append((now, Message("control_change", channel=0, control=1, value=i)))
    alphabet = "abcdefghijklmnopqrstuvwxyz"
    for text in ["", "x" * 127, "y" * 128, (alphabet * 12)[:300],
                 "Grüße · ça va · ½ · ÿ", "".join(chr(c) for c in range(256))]:
        t.append((now, MetaMessage("text", text=text)))
    t += [(now, Message("sysex", data=list(range(126)))),      # <len> 127: one byte
          (now, Message("sysex", data=list(range(127)))),      # <len> 128: two bytes
          (now, MetaMessage("sequencer_specific", data=list(range(200)))),
          (now + 1, MetaMessage("end_of_track"))]
    return MidiFile(type=0, ticks_per_beat=480, tracks=[at(t)])


def smpte_division():
    division = struct.unpack(">h", bytes([0xE7, 40]))[0]   # -25 fps, 40 ticks per frame
    t = [(0, MetaMessage("track_name", name="SMPTE")),
         (0, MetaMessage("smpte_offset", frame_rate=29.97, hours=0, minutes=0, seconds=0,
                         frames=0, sub_frames=0)),
         (0, Message("note_on", channel=2, note=69, velocity=64)),
         (1000, Message("note_on", channel=2, note=69, velocity=0)),
         (1000, MetaMessage("end_of_track"))]
    return MidiFile(type=1, ticks_per_beat=division, tracks=[at(t)])


def write_without_running_status(mid, path):
    """mido's MidiFile._save and write_track, minus the running-status branch."""
    out = bytearray(b"MThd" + struct.pack(">L", 6)
                    + struct.pack(">hhh", mid.type, len(mid.tracks), mid.ticks_per_beat))
    for track in mid.tracks:
        data = bytearray()
        for msg in fix_end_of_track(track):
            data.extend(encode_variable_int(msg.time))
            if msg.is_meta:
                data.extend(msg.bytes())
            elif msg.type == "sysex":
                data.append(0xF0)
                data.extend(encode_variable_int(len(msg.data) + 1))
                data.extend(msg.data)
                data.append(0xF7)
            else:
                data.extend(msg.bytes())
        out += b"MTrk" + struct.pack(">L", len(data)) + data
    path.write_bytes(bytes(out))


def same_messages(a, b):
    def norm(msg):
        return {k: list(v) if isinstance(v, (list, tuple)) else v for k, v in msg.dict().items()}
    return [[norm(m) for m in t] for t in a.tracks] == [[norm(m) for m in t] for t in b.tracks]


def seconds_rows(mid):
    """(tick seconds) for each message of mido's merged playback, seconds exact
    as the rational value of mido's float."""
    rows, tick, secs = [], 0, 0.0
    for by_tick, by_time in zip(mido.merge_tracks(mid.tracks), mid):
        tick += by_tick.time
        secs += by_time.time
        f = Fraction(secs)
        rows.append(f"({tick} {f.numerator}/{f.denominator})")
    return rows


def generate():
    ensemble_file = OUT / "type1-ensemble.mid"
    files = [("type0-melody.mid", melody()),
             ("type1-ensemble.mid", ensemble()),
             ("vlq-and-lengths.mid", vlq_and_lengths()),
             ("smpte-division.mid", smpte_division()),
             ("type0-empty-track.mid", MidiFile(type=0, ticks_per_beat=96,
                                               tracks=[MidiTrack([MetaMessage("end_of_track")])])),
             ("type1-no-tracks.mid", MidiFile(type=1, ticks_per_beat=96))]
    entries = []
    for name, mid in files:
        mid.save(OUT / name)
        reread = MidiFile(OUT / name)
        # mido drops the delta of meta types it doesn't know, so keep those at delta 0.
        assert same_messages(mid, reread), f"mido reads {name} differently from what it wrote"
        entries.append((name, reread))

    reread = MidiFile(ensemble_file)
    merged = MidiFile(type=0, ticks_per_beat=480, tracks=[mido.merge_tracks(reread.tracks)])
    merged.save(OUT / "type0-merged.mid")
    entries.append(("type0-merged.mid", MidiFile(OUT / "type0-merged.mid")))

    no_rs = OUT / "type1-ensemble-no-rs.mid"
    write_without_running_status(reread, no_rs)
    assert same_messages(MidiFile(no_rs), reread), "no-running-status file reads differently"
    assert no_rs.read_bytes() != ensemble_file.read_bytes()
    entries.append(("type1-ensemble-no-rs.mid", MidiFile(no_rs)))

    (OUT / "midi0-files.sexp").write_text(
        f";; (file format division tracks): what mido {MIDO_VERSION} reads from each fixture.\n"
        ";; Generated by generate-midi0.py; regenerate, don't edit. Division is ticks per\n"
        ";; quarter note or (:smpte fps ticks-per-frame). Events are (kind delta fields...);\n"
        ";; a text is a string or, with control characters, a list of character codes.\n("
        + "\n ".join(f'("{name}" {smf_sexp(mid)})' for name, mid in entries) + ")\n",
        encoding="utf-8")

    seconds = [(name, mid) for name, mid in entries if name in
               ("type0-melody.mid", "type1-ensemble.mid", "type0-merged.mid")]
    (OUT / "midi0-seconds.sexp").write_text(
        f";; (file (tick seconds)...): mido {MIDO_VERSION} playback time of each message of the\n"
        ";; merged tracks, seconds as the exact rational value of mido's float.\n("
        + "\n ".join(f'("{name}"\n  ' + "\n  ".join(seconds_rows(mid)) + ")"
                     for name, mid in seconds) + ")\n",
        encoding="utf-8")
    print(len(entries), "files:", ", ".join(name for name, _ in entries))


def dump():
    """Print (format division tracks) for each SMF concatenated on stdin."""
    stream, out = sys.stdin.buffer, []
    while stream.peek(1):
        out.append(f"({smf_sexp(MidiFile(file=stream))})")
    sys.stdout.write("(" + "\n ".join(out) + ")\n")


if __name__ == "__main__":
    dump() if sys.argv[1:] == ["--dump"] else generate()
