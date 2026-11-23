# midi-protocol

A MIDI 1.0 event model and a Standard MIDI File (SMF) reader and writer, at the event level: header, tracks, delta times, running status, channel messages, meta events and sysex passthrough. It has no notation semantics, and real-time ports are out of scope. Byte I/O builds on [binary-protocol](https://github.com/egao1980/binary-protocol).

**Status:** skeleton (0.0.1). The design is in the capability brief [cl-stack/docs/capabilities/score.md](https://github.com/egao1980/cl-stack/blob/main/docs/capabilities/score.md) (the `midi-protocol` rows and slice SC3). The API and tests land through contract branches and team-solve.

```sh
cl-repo test --system midi-protocol
```

## License

MIT, see [LICENSE](LICENSE).
