; crew/holo/copy.l -- objcopy's two output formats, ours: a linked elf flattened to
; the bytes a flash tool takes. `-O binary` is the image as it lies from the lowest
; load address, `-O ihex` that same image in Intel hex, which is what teensy_loader_cli
; and the Teensy Loader gui read. between them they were the last foreign tool on the
; thumb boards (port/{teensy41,nucleo446,rp2040}/Makefile); `kore objcopy` is the applet.
;
; a section's sh_addr is where it runs, and the flasher wants where it is stored --
; different numbers for a lane held in flash and copied into RAM at boot. so every
; section is carried through the PT_LOAD holding its file bytes and the segment's
; p_paddr answers: lma = p_paddr + (off - p_offset). matching by file offset rather
; than by address is what keeps two lanes sharing a vaddr apart.
;
; the section table sets the record breaks -- objcopy starts a fresh record at each
; section, so a hex file carries the section shape in where its short records fall.
; matching that is what lets test/gate/objcopy.sh be a byte comparison and not a
; reading of the result.
;
; loads after crew/holo/link.l, whose field table, ld-write and ld-zeros it reads.
(module 'holo
(use 'kore) ; the text/u floors (uread, usplit ..), scoped to this module's compile
(:
   ;; the pt_loads as (fileoff filesz paddr) -- all a flat image needs of them.
   (oc-segs fs s)
    (: n (ld-f fs s 0 'e-phnum) po (ld-f fs s 0 'e-phoff) e (ld-n fs 'ph-ent)
       (go i acc)
        (? (<= n i) (rev acc)
           (: h (po + (i * e))
              (go (i + 1) (? (ld-f fs s h 'ph-type = 1)
                             ([(ld-f fs s h 'ph-off) (ld-f fs s h 'ph-fsz)
                               (ld-f fs s h 'ph-pa)] >< acc)
                             acc))))
       (go 0 ()))
   (oc-lma id segs off)
    (: (go ps) (? !(two? ps) (scare 'copy-unmapped-section off)
                  (: p <ps
                     (? ((<p <= off) && (off < (<p + at p 1))) (at p 2 + (off - <p))
                        (go >ps))))
       (go segs))

   ;; -> ((lma fileoff size).. in lma order, entry). what a flat image is made of is
   ;; every section the loader allocates that has bytes to give -- .bss is allocated
   ;; and has none, which is right: a flasher writes what exists. the question is
   ;; has bytes, not is-progbits: a dynamic exe loads its .note, .dynsym, .gnu.hash
   ;; and .rela.dyn too, and each is a type of its own.
   (oc-read id s)
    (: _ (? ((s 0 = 127) && (s 1 = 69) && (s 2 = 76) && (s 3 = 70)) 0
            (scare 'copy-not-elf id))
       fs (? (s 4 = 2) elf64f (s 4 = 1) elf32f (scare 'copy-elf-class id))
       ; a table this file does not have is a table of stride 0, so ask after the
       ; count first or the entsize check answers for it and names the wrong field.
       ; both tables are load-bearing here and neither is optional in an executable:
       ; a bare image from elf.l's one-segment writer carries no section headers, and
       ; flattening it off the segment alone would be a guess. (gnu objcopy refuses it
       ; too; llvm-objcopy writes a zero-BYTE image and says nothing, which on a board
       ; is a brick that looks like a build.)
       shnum (ld-f fs s 0 'e-shnum) phnum (ld-f fs s 0 'e-phnum)
       _ (? (0 < shnum) 0 (scare 'copy-no-sections id))
       _ (? (0 < phnum) 0 (scare 'copy-no-segments id))
       _ (? (ld-f fs s 0 'e-shent = ld-n fs 'sh-ent) 0 (scare 'copy-shentsize id))
       _ (? (ld-f fs s 0 'e-phent = ld-n fs 'ph-ent) 0 (scare 'copy-phentsize id))
       segs (oc-segs fs s)
       _ (? (two? segs) 0 (scare 'copy-no-load id))
       shoff (ld-f fs s 0 'e-shoff)
       (go i acc)
        (? (<= shnum i) acc
           (: h (shoff + (i * ld-n fs 'sh-ent))
              ty (ld-f fs s h 'sh-type) fl (ld-f fs s h 'sh-flags)
              off (ld-f fs s h 'sh-off) sz (ld-f fs s h 'sh-size)
              keep (!(ty = 8) && (0 < (fl & 2)) && (0 < sz))
              (go (i + 1) (? keep ([(oc-lma id segs off) off sz] >< acc) acc))))
       secs (go 1 ())
       _ (? (two? secs) 0 (scare 'copy-nothing id))
       [(sortby (\ a b (<a <= <b)) secs) (ld-f fs s 0 'e-entry)])

   ;; -- the flat image, as a list of pieces for ld-write. a gap between two sections
   ;; is zeros: the file lays down from the lowest load address, so whatever is not a
   ;; section is what the part comes up holding there.
   (oc-bin s secs)
    (: (go l pos acc)
        (? !(two? l) (rev acc)
           (: c <l a <c o (at c 1) n (at c 2)
              _ (? (a < pos) (scare 'copy-overlap a) 0)
              (go >l (a + n)
                  (snip s o (o + n) >< (? (pos < a) (ld-zeros (a - pos) >< acc) acc)))))
       (go secs (at <secs 0) ()))

   ;; -- Intel hex: ":" ll AAAA tt dd.. CC, every byte two ASCII hex digits, CC the
   ;; two's complement of the sum of all before it. 01 is the end, and an entry of 0
   ;; writes no start record at all, which is objcopy's silence.
   ;; the address field is sixteen bits, so a base record carries the rest -- and there
   ;; are two of those, a 1976 one and a 1988 one. under 1 MB the base is an 8086 segment
   ;; (type 02, the base shifted right 4); at or above it is a linear upper half (type 04).
   ;; the two do not replace each other, they sum, so a run climbing past 1 MB has to
   ;; zero the segment before naming a linear base -- a bare 04 would land 0xf0000 high.
   ;; a data record stops at the 64K line rather than wrapping over it.
   ;; the line ending is crlf -- the format has been a serial-line format since 1973
   ;; and every writer still speaks it. a lone \n reads back fine and diffs wrong.
   oc-digits "0123456789ABCDEF"
   oc-pairs (foldl (\ a b (a + snip oc-digits (b >> 4) ((b >> 4) + 1)
                             + snip oc-digits (b & 15) ((b & 15) + 1)))
                   "" (jot 256))                  ; every byte's two digits, once
   (oc-x2 b) (snip oc-pairs (b + b) (b + b + 2))
   (oc-cat ls) (foldl (+) "" ls)
   (oc-rec ty addr bs)
    (: all ([(tally bs) ((addr >> 8) & 255) (addr & 255) ty] + bs)
       ck ((256 - (foldl (+) 0 all % 256)) % 256)
       (oc-cat (":" >< (map oc-x2 (all + [ck]) + '("\r\n")))))
   (oc-be4 v) [((v >> 24) & 255) ((v >> 16) & 255) ((v >> 8) & 255) (v & 255)]
   ;; the entry wears the same two ages as the base: type 03 is cs:ip under 1 MB, and
   ;; type 05 the flat address above it.
   (oc-start e)
    (? (0xFFFFF < e) (oc-rec 5 0 (oc-be4 e))
       (oc-rec 3 0 [(((e >> 16) & 15) << 4) 0 ((e >> 8) & 255) (e & 255)]))
   (oc-chunks secs)
    (: (go a o left acc)
        (? (left < 1) acc
           (: room (65536 - (a % 65536))
              m (? (16 < left) 16 left)
              n (? (room < m) room m)
              (go (a + n) (o + n) (left - n) ([a o n] >< acc))))
       (rev (foldl (\ acc c (go <c (at c 1) (at c 2) acc)) () secs)))
   (oc-b02 nb) (oc-rec 2 0 [((nb >> 12) & 255) ((nb >> 4) & 255)])
   (oc-b04 nb) (oc-rec 4 0 [((nb >> 24) & 255) ((nb >> 16) & 255)])
   ;; base is the 64K window in force, sg whether a segment base is standing in it
   (oc-ihex s secs entry)
    (: (go l base sg acc)
        (? !(two? l)
           (rev (link ":00000001FF\r\n"
                      (? (0 < entry) (oc-start entry >< acc) acc)))
           (: c <l a <c
              new? ((base + 65535) < a)
              lin? (0xFFFFF < a)
              nb (a - (a % 65536))
              acc2 (? !new? acc
                      !lin? (oc-b02 nb >< acc)
                      sg (oc-b04 nb >< (oc-b02 0 >< acc))
                      (oc-b04 nb >< acc))
              b2 (? new? nb base)
              (go >l b2 (? new? !lin? sg)
                  (oc-rec 0 (a - b2)
                          (map (\ k (s (at c 1 + k))) (jot (at c 2))) >< acc2))))
       (go (oc-chunks secs) 0 () ()))

   ;; -- the applet. GNU's objcopy names itself when it complains and exits 1, and a
   ;; makefile's error belongs where the binutils one went; `kore objcopy` dispatches
   ;; here, and so does the one-line cat a port build runs.
   (oc-die m) (: _ (say err ("objcopy: " + m)) _ (put err 10) (quit 1))
   (objcopy as)
    (? !(tally as = 4) (oc-die "usage: objcopy -O binary|ihex IN OUT")
       (: fmt (? (<as = "-O") <>as (oc-die ("unrecognized option " + <as)))
          src (at as 2) dst (at as 3)
          _ (? (member? fmt '("binary" "ihex")) 0 (oc-die ("unknown output format " + fmt)))
          r (uread src)
          _ (? (two? r) 0 (oc-die ("cannot open " + src)))
          s <>r
          w (oc-read src s)
          nw (ld-write dst (? (fmt = "ihex") (oc-ihex s <w <>w) (oc-bin s <w)))
          _ (? (nw < 0) (oc-die ("cannot write " + dst + " -- errno " + show (0 - nw))) 0)
          (quit 0))))
) ; module 'holo
