# plan: horn — the audio door, on both seats

**THE CLAIM: sound is a PORT, and once it is, the hosted side comes with it.** The tree has no
audio anywhere. It does have a port layer whose write contract is already the contract a sound
card wants — land what fits, answer 0 when the ring is full, let the scheduler park the writer
— and `ai_io_alloc` is core rather than host, so an fd is a port on inle exactly as it is under
Linux. So the question "can the host have this too" is not a second implementation; it is the
same door with a different fd under it.

⚠ **`sound` is TAKEN and it is not close.** `sound` is love's reader — one datum off text,
`love/p1.l`, and salt, dns, cli and bao all stand on it. There is a `sound0` nif beside it.
Naming the audio door `sound` would shadow the reader in every file that uses both. **`horn`**
is free and is what this plan spells; `reed`, `drum` and `chime` are free too if a better ear
than mine prefers one.

## what already exists, and it is most of it

* **the port vtable is the ring contract, verbatim.** `src/love.h`: *"writen: land up to n bytes
  in one motion: >0 landed, 0 no room now (caller keeps the residue), -1 the device is gone."*
  That is a DMA ring with backpressure, described without knowing it. A full audio buffer
  answers 0, the caller keeps the residue, and the scheduler's existing fd-park wakes it.
* ⚠ **it must be a HEAP port, and the vtable says why:** *"only a door whose port keeps a write
  run may refuse; the static ports cannot park."* So the horn is `ai_io_alloc`'s, never a boot
  row — and that is the same sentence on both seats.
* **an fd is a port on both seats already.** src/sock.c's whole method is "produce an OS fd,
  hand it to `ai_io_alloc`, and read and write come free"; doc/misc/inle.md says `ai_io_alloc`
  is core, not host. Neither seat needs a new mechanism, only a new device.
* **the PCI walk is written** (src/blk.c, CF8/CFC) and so is virtio-mmio on aarch64. The disk
  rung already paid for both transports.
* **the flow doors are written** — spout/drip, backpressure, parking, the reader-bootstrap
  arc's whole rung 9. PCM is a byte stream that must not be dropped, which is the one shape
  those doors were built for.

## the shape

One device, two faces, the way the framebuffer already has two:

* **the love face** is a port. `(horn ...)` opens it and answers a port; PCM goes out through
  the ordinary write path, and a full ring is backpressure rather than a dropped frame. Rate,
  channels and format are the ONE thing an fd cannot carry, so they ride the open.
* **the C face** is a direct call (`k_horn_write`), for a program linked into the image that
  has no love heap in hand — src/doom.c reaches `k_fb` the same way today.

⚠ **the door is a vtable, not an AC'97 shape.** qemu's aarch64 `virt` has no AC'97 (it offers
virtio-sound), and the hosted seats have neither. If the second device is a rewrite, the first
one was designed wrong.

## the ladder

Hosted first, and deliberately: it is where a mixer can be got right against real speakers with
no driver in the way, and where a gate can run on the dev box. inle is the rung that needs
hardware written; it should not also be the rung that debugs resampling.

### rung 0 — the name, the port, and a sink that discards

`horn` opens, answers a port, accepts PCM, throws it away. No hardware on either seat. This is
the rung that proves the *shape*: that a heap port refuses with 0 rather than dropping, that a
blocked writer parks and wakes, that the residue is kept.

*gate:* a love law that writes more frames than the ring holds and reads the backpressure —
`test/host/horn.l`, and the same file in the kernel corpus. **~1 day.**

### rung 1 — the BSD seat, which is nearly free

`/dev/dsp` is native on FreeBSD and NetBSD: open, three ioctls (`SNDCTL_DSP_SETFMT`, `SPEED`,
`CHANNELS`), then `write`. No library, no protocol, and the seed-universal arc already has boxes
on both (test_freebsd_arm64, test_netbsd_arm64).

⚠ **this is not the Linux lane.** This box has `/dev/snd/*` and no `/dev/dsp` at all — OSS is
gone from ordinary Linux, so anyone reading "just use /dev/dsp" will find nothing there.
**~1 day.**

### rung 2 — the Linux seat, by ioctl and no libasound

`/dev/snd/pcmC0D0p` with the ALSA ioctl protocol directly — `HW_REFINE`/`HW_PARAMS`,
`SW_PARAMS`, `PREPARE`, then `WRITEI_FRAMES`. It is a real protocol with a large parameter
struct, but it is **plain syscalls**, which is the only kind this tree may have: linking
libasound would put an ambient library under the artifact and the bare-cc door (rung 7 of
self-host-default) exists precisely so nothing does.

⚠ I have not written this handshake here — the size is an estimate off the interface, not off a
measurement. ⚠ device naming is a policy question, not a lookup: `pcmC0D0p` is not always the
one you want (this box's first playback node is `pcmC0D3p`). **~2-4 days**, and the widest error
bar on the ladder.

### rung 3 — inle on x86_64: AC'97

`-device AC97`, and it is the friendly one: **two I/O-port BARs**, so none of the 64-bit-MMIO
grief rung 5 hit under OVMF. A 32-entry buffer descriptor list of `kmallocw` buffers, the run
bit set, and the current-index register read from the write path — **polled, no interrupt**,
src/blk.c's own posture and for blk.c's own reason.

⚠ `pa = va - khhdm` holds for heap memory and NOT for image statics — blk.c's warning, and the
BDL and every sample buffer are subject to it. ⚠ every door's map stops at 4 GiB.
⚠ `asmops.h` has `k_inb`/`k_outb`/`k_inl`/`k_outl` and **no 16-bit pair**; AC'97's mixer
registers are 16-bit, so `k_inw`/`k_outw` are a prerequisite. **~2-3 days**, ~200 lines, a twin
of blk.c in shape.

### rung 4 — inle on aarch64: virtio-sound

qemu `virt` carries `virtio-sound-device`. The transport is already written (blk.c's virtio-mmio
half); what is new is the device's own control/stream queues. This is the rung that proves rung
0's vtable was a vtable. **~3-4 days.**

### rung 5 — doom hears itself

doomgeneric asks for one `sound_module_t`: Init, GetSfxLumpNum, Update, UpdateSoundParams,
StartSound, StopSound, SoundIsPlaying, CacheSounds. `DG_sound_module` is the whole contract.
The work is a mixer, not plumbing: DMX lumps are 8-bit unsigned mono at 11025 Hz behind an
8-byte header, and eight channels want mixing with volume and separation into 16-bit stereo at
the device's rate. ⚠ `i_sdlsound.c` is 1076 lines and that number will mislead you — most of it
is SDL_mixer, libsamplerate and caching that a 200-line mixer does not need.

⚠ **it rides the C face**, like src/doom.c's framebuffer: doom is linked into the image and has
no love heap in hand at `I_UpdateSound`. **~2 days on top of any one device rung.**

*gate:* the honest headless one — `-audiodev none` and assert the ring index advances, since a
gate cannot listen. Hearing it is a human's job, once.

## what is cheaper than it looks

* **backpressure is not new work.** The one thing that makes audio hard in a naive stack — what
  to do when the ring is full — is the port vtable's answer already, and the scheduler already
  parks and wakes on it.
* **the frame loop is a generous refill clock.** doom's `Update()` is called every frame and
  inle renders at ~260 fps, so a polled top-up has enormous margin. If the frame rate ever falls
  below the buffer duration the honest fix is a **bigger ring, not an interrupt**.
* **two of the four devices are already-paid transports.** PCI config space and virtio-mmio both
  exist.

## the expensive things nobody budgets

* **music is a synthesizer, and that is the whole cost.** `mus2mid.c` is vendored already, so
  MUS→MIDI is free — and then you owe a synth. Chocolate's answer is an OPL3 emulator of ~3-4k
  lines that doomgeneric does not ship. **Ship SFX and leave music silent**; revisit it as its
  own arc or not at all.
* **resampling policy.** 11025 → 48000 is not an integer ratio. Nearest-neighbour is audibly
  gritty on doom's samples and is what "it works" will sound like; linear is cheap and fine.
  Decide it once, in the mixer, not per device.
* **latency has no gate.** Ring depth trades underrun against lag and no test can hold it. Pick
  a depth, write down why, and expect to change it by ear.
* **capture is a different device.** The vtable has `readn` and `hda-duplex` exists, but nothing
  here needs a microphone and designing for one now would be inventing requirements.

## open

* **which name** — `horn` reads well beside quay and berth, but this is the one decision that is
  cheap now and expensive later.
* **where the rate/channels/format live.** They ride the open here. The alternative is a control
  door beside the port; that is what a second device with different constraints would want, and
  we do not have one yet.
* **whether the mixer is love or C.** doom needs the C face regardless. A love-side mixer would
  be the nicer artifact and would make `horn` useful to anything else in the tree; it is also
  the part where a per-sample loop in love has to be measured before it is believed.
