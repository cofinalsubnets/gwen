; bao -- the shell core: the read-eval-print loop, its multi-line line editor,
; the stream shell (read/reads), the floor handler (welp), and the host-only
; pty wrapper (wrap). bao is the BAKED shell core for all three frontends -- the
; host (out/host/love), the freestanding kernel (kmain.c), and the love0 bootstrap
; self-test -- loaded after the prel. (It was repl.l; the editor/repl internals
; used to LEAK as user-visible book globals because the corpus tested them by
; name. Now the editor + repl + the pty wrapper live inside one closure, so only
; the entry points -- shell, welp, edraw, edln, wrap, bao, plus the stream shell
; read/reads -- are book globals.)
;
; parsing is delegated to the C reader: the editor's flattened charlist is
; implicitly parenthesized and read as ONE expression via (tap ..) + read
; -- a line `x y z` is the application (x y z), and infix works at the
; prompt (factored at compile time by opfix; reading is structural).
; read's more-bit conditions route through the help continuation, whose
; default delivers the port itself on more (input ends inside an unfinished
; SHAPE -- an unclosed paren or string; a trailing operator is a complete
; datum that folds to a partial). that lets parseall distinguish "submit"
; from "keep editing" -- parseall is the lisp-visible face of the more bit;
; a global `help` takes over that dispatch.
;
; the editor buffer is a four-zipper threaded positionally through
; the editor functions: u (lines above the cursor, reversed), l (chars
; left of cursor on the current line, reversed), r (chars at/after
; cursor on the current line, in order), d (lines below). state is
; passed through helpers in CPS so no table is allocated per
; keystroke. on each tick the whole multi-line input is redrawn from
; the prompt's row via relative cursor motion; the loop tracks rows
; above the cursor from the previous render so terminal scrolling
; doesn't desync.
;
; command history is a parallel zipper (hu, hd) over previously-
; submitted buffer states. when the buffer cursor is on the top line,
; up navigates history toward older; mirrored for down. on first edit
; to a recalled entry, the pristine version is restored to its slot
; and the buffer becomes a new entry, so the original survives.

; read: chars -> top, the reverse shell of the compiler -- the lane every shell
; ultimately feeds into. its `char` lift + `char->chars` realizer are CLOSURE-PRIVATE
; here (off the book). a charm is a sat (a non-negative fixnum); a CHARS is a bare
; charlist (eager, ends at 0); the `char` colist is the LAZY form: (cap cs) is a charm,
; (cup cs) a top, the tail forced with the empty -- ((cup cs) ()) -- typing as colist + 0
; (the coproduct, 0 the end). consumers dispatch on KIND: two? is a cell, anything else
; the end. `char` is the canonical lift -- chars cell by cell, a port (the HOT lane)
; one byte per force (-1 ends it), a colist passing through, the end lifting to itself --
; and `char->chars` realizes a finite colist. read takes any of these as `s`, reads ONE
; form (structurally -- the reader knows no operators) and sends it through ev's core
; (opfix, feel, ana, cata): the full hull chars -> top in one call. nothing readable --
; empty input or an unfinished shape -- is nothing. (lit? is no use for the cell test:
; it names the top region -- map/fn/hot -- so a chain or string answers false anyway.)
; a REGISTERED MODULE with NO BRACKETS (the kanren pattern): the runtime
; supplies the layer -- a frontend registers this source (ai_lib_) and its boot
; says (use 'bao). bao is ALL surface -- the closure below already keeps the
; internals private -- and the splice serves the user verbs (read reads welp
; wrap) bare, while the frontend launchers reach the plumbing through the
; registry ((from 'bao 'bao) / (from 'bao 'shell)); (from 'bao 'keys) lists
; the shell core's whole public face.
(: (read s)
   (: (char s)
      (? (two? s) (: t >s
                     (? (two? t) (<s >< (\ u (char t)))          ; chars: lift, lazy tail
                        !t (<s >< (\ u ()))                      ; the last chars cell
                        s))                                      ; a top tail: char already
         (hot? s) (: c (see s)                                   ; a port is the hot char
                        (? (c = -1) () (c >< (\ u (char s)))))
         ())
      (char->chars s)
      (: cs (char s)
         (? (two? cs) (<cs >< (char->chars (<>cs ()))) ()))
      p (tap (char->chars s)) e (nom 0) r (sound p e)
      (? (id? r e) 0 (id? r p) 0 (ev r))))
; reads: the stream shell -- a port is the LAZY charms, and reads drinks it:
; read and eval every form until the stream ends (eof, or input ends inside
; an unfinished shape); the value is the last form's. ev late-binds through
; the book ((ev 'ev r)) so the one definition drives both of love0's passes
; (c0 first, the hatched ev' second); the host's stdin runner and the
; love0/kernel corpus runners are all this function now.
(: (reads p)
   ((: (g e z) (: r (sound p e)
       (? (id? r e) z (id? r p) z (g e (ev 'ev r)))))
    (nom 0) 0))

; --- the shell core: editor + repl + welp + the pty wrapper, all closure-private.
; the body of this (: ..) returns the public entry points as a list, defglob'd
; below; everything else (the ed* editor, parseall, do_eval, the history zipper,
; pump/waitfor, the raw-line decode) stays inside this scope, OFF the global book.
(: _                                ; the export-list holder: `_` never leaks to the book
 (: m (nom 0) eofsym (nom 0)

   ; the host nifs the shell wants -- raw (termios), the pty pair (mind gather),
   ; close -- are not on every frontend: the kernel pins none of them, love0
   ; lacks the ptys. bind the global when it exists, else a no-op stub; the
   ; mentions below read these locals, so bao compiles quiet everywhere and a
   ; call answers a clean () where the facility is not.
   (want n) (? (member? n (names ())) (ev n) (\ _ ()))
   raw (want 'raw) mind (want 'mind) gather (want 'gather) close (want 'close)

   (revcat a b) (foldl (flip link) b a)

   ; parse a charlist as ONE expression. read EVERY top-level datum off the line:
   ; a SINGLE datum is used as-is -- so a leading monadic like `+(-3)` keeps fusing
   ; (wrapping it in () would put the operator in HEAD position, where it never
   ; fuses -- the old `(+(-3))` -> `(+ -3)` bug); MULTIPLE datums become the
   ; application `(a b c)`, so `f x` applies and `1 = 1` factors infix. returns m if
   ; the line ends inside an unfinished shape, () for a blank line, else a one-form list.
   (rdall p acc) (: r (sound p eofsym)
                    (? (r = p)      m              ; unfinished shape -> more
                       (r = eofsym) (rev acc)      ; eof -> the datums, in order
                       (rdall p (r >< acc))))
   (parseall cl)
     (: ds (rdall (tap cl) ())
        (? (id? ds m)        m                     ; unfinished shape
           !(two? ds)        ()                    ; blank line: no datums
           !(two? >ds)       `(<ds)                ; ONE datum: as-is (leading monadic preserved)
           `(ds)))                                ; MANY: the application (a b c)

   ; --- editor ---

   ; intersperse a list of charlists with newlines, in order
   (joinln ls)
     (? (two? ls)
        (? (two? >ls)
           (<ls + (10 >< joinln >ls))
           <ls)
        ())

   ; flatten the editor state into one charlist suitable for parseall
   (flatten u l r d)
     (joinln (revcat u (revcat l r >< d)))

   ; walk xs forward taking n chars into acc (reversed); when done
   ; call k with (rev (take n xs)) and (drop n xs). stops cleanly at
   ; end-of-xs even if n exceeds the list length.
   (splitat k acc n xs)
     (? (two? xs && 0 < n) (splitat k (<xs >< acc) (n - 1) >xs)
        (k acc xs))

   ; map non-printables to space; otherwise pass through
   (prc c) (putc (? (32 <= c && c < 127) c 32))

   ; CPS editor primitives. each takes the loop as continuation k
   ; and forwards a new (u l r d) to it; helpers are no-ops at their
   ; boundary. left/right wrap across lines; bsp at column 0 merges
   ; the current line onto the previous; del at end-of-line splices
   ; the next line on. up/down preserve column, clamped to the
   ; destination line's length.
   (edleft k u l r d)
     (? (two? l) (k u >l (<l >< r) d)
        (two? u) (k >u (rev <u) () (revcat l r >< d))
        (k u l r d))
   (edright k u l r d)
     (? (two? r) (k u (<r >< l) >r d)
        (two? d) (k (revcat l r >< u) () <d >d)
        (k u l r d))
   (edbsp k u l r d)
     (? (two? l) (k u >l r d)
        (two? u) (k >u (rev <u) r d)
        (k u l r d))
   (eddel k u l r d)
     (? (two? r) (k u l >r d)
        (two? d) (k u l <d >d)
        (k u l r d))
   (edhome k u l r d) (k u () (revcat l r) d)
   (edend  k u l r d) (k u (revcat r l) () d)
   ; edtop / edbot jump across the whole buffer: cursor at the start of
   ; the topmost line / end of the bottommost line. on a single-line
   ; buffer they degenerate to edhome / edend.
   (edtop k u l r d)
     (? (two? u) (: ru (rev u)
                  (k () () <ru (>ru + (revcat l r >< d))))
        (k u () (revcat l r) d))
   (edbot k u l r d)
     (? (two? d) (: rd (rev d)
                  (k (>rd + (revcat l r >< u)) (revcat <rd ()) () ()))
        (k u (revcat r l) () d))
   (edup k u l r d)
     (? (two? u) (splitat (\ lz rr (k >u lz rr (revcat l r >< d)))
                          () (tally l) <u)
        (k u l r d))
   (eddown k u l r d)
     (? (two? d) (splitat (\ lz rr (k (revcat l r >< u) lz rr >d))
                          () (tally l) <d)
        (k u l r d))

   ; --- history zipper ---

   ; pack the buffer's four halves into one slot for history storage.
   (mkframe u l r d) (u >< l >< r >< d)

   ; if cur holds a pristine recall, restore it at its slot in hu;
   ; called before any mutating edit so the original survives.
   (detach hu cur) (? (two? cur) (cur >< hu) hu)

   ; predicates over the buffer used by the history navigation rules
   ; below: empty buffers don't get pushed during normal nav, and
   ; off-the-end nav only pushes when the buffer parses to a value.
   (emptybuf u l r d) (!u && !l && !r && !d)
   (parses u l r d)   (two? (parseall (flatten u l r d)))

   ; up/down through the history zipper. on a normal pop, push the
   ; current buffer onto the other side (skipping if empty); if the
   ; other side is empty (hu or hd) but we're on a real recall (cur
   ; non-nil) and the buffer parses, push and scroll into a fresh
   ; empty line.
   (uphist k u l r d hu hd cur)
     (? (two? hu)
        (: e <hu
           nhd (? (emptybuf u l r d) hd (mkframe u l r d >< hd))
           (k <e <>e <>>e >>>e >hu nhd e))
        (two? cur && parses u l r d)
        (: nhd (mkframe u l r d >< hd)
           (k () () () () hu nhd ()))
        (k u l r d hu hd cur))
   (downhist k u l r d hu hd cur)
     (? (two? hd)
        (: e <hd
           nhu (? (emptybuf u l r d) hu (mkframe u l r d >< hu))
           (k <e <>e <>>e >>>e nhu >hd e))
        (two? cur && parses u l r d)
        (: nhu (mkframe u l r d >< hu)
           (k () () () () nhu hd ()))
        (k u l r d hu hd cur))

   ; map a raw byte (or escape sequence) to an event code:
   ; printable >0; -1 left, -2 right, -3 bsp, -4 del, -5 home,
   ; -6 end, -7 quit, -8 up, -9 down, -10 buffer top, -11 buffer end;
   ; 10 = enter.
   ;
   ; after `ESC [ 1` there are two shapes: `~` is plain Home; `; M F`
   ; carries a modifier digit M (5=ctrl, 2=shift, ...) and a final byte
   ; F. modified Home/End jump to buffer top/end; modified arrows fold
   ; back to plain arrow events (we don't act on the modifier).
   ; the byte -> code tables, shared by both decoders (edesc/edev for the tty,
   ; besc/bev for a port). a compare costs ~9ns and a peep ~47ns flat in size,
   ; so a table pays past about five compares EXECUTED: evk earns it outright
   ; (a printable byte -- the common one -- falls through every one of its nine
   ; keys), the six-key arrow maps by a nose on the miss. 0 is the miss, read
   ; with id?: an event code nets RED, so truthiness cannot tell hit from miss.
   arrow    #(68 -1 67 -2 65 -8 66 -9 72 -5 70 -6)    ; ESC [ F -- the plain finals
   modarrow #(68 -1 67 -2 65 -8 66 -9 72 -10 70 -11)  ; ESC [ 1 ; M F -- modified home/end jump the buffer
   evk      #(-1 -7 4 -7 3 -12 13 10 10 10 8 -3 127 -3 1 -5 5 -6)   ; the raw control bytes
   (edesc1 c) (? (c = 126) -5
                 (c = 59)  (: _ (getc 0) (peep modarrow (getc 0) 0)))
   (edesc x) (? !(91 = getc 0) 0
     (: c (getc 0) e (peep arrow c 0)
        (? !(id? e 0) e
           (c = 49) (edesc1 (getc 0))
           (c = 55) (: _ (getc 0) -5)
           (c = 52 || c = 56) (: _ (getc 0) -6)
           (c = 51) (: _ (getc 0) -4) 0)))
   (edev x) (: c (getc 0) e (peep evk c 0)
     (? !(id? e 0) e
        (c = 27) (edesc 0)
        (32 <= c && c < 127 && c)))

   ; redraw the buffer in place. pra is "rows above the cursor from
   ; the previous render" -- we move up that many rows first so we
   ; land back on the prompt's row. all motion is relative: nothing
   ; depends on a saved absolute cursor, so the terminal scrolling
   ; (when the buffer reaches the last row) doesn't desync us. pl is
   ; the prompt's column; the topmost line shares it, subsequent
   ; lines start at column 0.
   (csi n f) (: _ (putc 27) _ (putc 91) _ (putn n) (putc f))   ; ESC [ n f -- the one cursor verb
   (edrender pl pra u l r d)
     (: _ (? (0 < pra) (csi pra 65) ())
        _ (putc 13)
        _ (? (0 < pl)  (csi pl 67) ())
        _ (putc 27) _ (putc 91) _ (putc 74) ; clear to end of screen
        _ (each (rev u) (\ ln (do (each ln prc) (putc 10))))
        _ (each (revcat l r) prc)
        _ (each d (\ ln (do (putc 10) (each ln prc))))
        ld (tally d)
        col ((? (two? u) 0 pl) + tally l)
        _ (? (0 < ld) (csi ld 65) ())
        _ (putc 13)
        _ (? (0 < col) (csi col 67) ())
        (flush out))

   ps1 "> "
   ; the helm glows in the crew's cyan (256-colour 117 ~ #7dcfff), reset after.
   ; SGR is zero-width to both the host terminal and the inle console (crew/quay/quay.c),
   ; so `pl` below stays (tally ps1) and the cursor math is undisturbed.
   ps1c ("" + 27 + "[38;5;117m" + ps1 + "" + 27 + "[39m")

   ; print the prompt, then dispatch events until ^D or until enter
   ; at end-of-buffer with the buffer fully parsed. enter always
   ; splits the current line at the cursor; only when the cursor is
   ; at end-of-buffer do we try parseall, which reads the buffer as
   ; one expression. returns eofsym on ^D, otherwise
   ; (X vs (X nhu nhd)) so the repl can thread history.
   ;
   ; the loop carries pra = rows above the cursor as of the previous
   ; render. edrender uses pra to move back up to the prompt's row
   ; before redrawing, so we never rely on a saved absolute cursor
   ; position -- terminal scrolling stays consistent. since nothing
   ; between renders moves the cursor (no putc except in the submit
   ; branch which exits), the next render's pra is always (tally u) of
   ; the CURRENT frame, regardless of how the state transitions; we
   ; cache it as npra and route every recursion through it.
   ;
   ; kloop wraps the CPS helpers for non-mutating motion; kedit does
   ; the same for mutating ops (backspace, delete) and applies detach
   ; on the way. khist is the continuation used by uphist/downhist
   ; for history navigation. cur is the pristine frame of the
   ; currently-recalled entry, or () if the buffer is a free edit.
   (edline hu hd)
     (: pr ps1
        _ (puts ps1c)
        pl (tally pr)
        (loop pra u l r d hu hd cur)
          (: _ (edrender pl pra u l r d)
             c (edev 0)
             npra (tally u)
             (kloop uu lz rr dd) (loop npra uu lz rr dd hu hd cur)
             (kedit uu lz rr dd) (loop npra uu lz rr dd (detach hu cur) hd ())
             (khist uu lz rr dd nhu nhd ncur) (loop npra uu lz rr dd nhu nhd ncur)
             ; the event dispatch stays a CHAIN, not a table: every arm runs a
             ; different continuation over the live buffer, so a table would have
             ; to hold closures built per keystroke -- and a keystroke is human-
             ; rate anyway. the tables above earn their keep because their arms
             ; are constants.
             (? (c = -7) eofsym
                ; enter CASHES the buffer wherever the cursor sits, as long as
                ; it parses complete; only an unfinished shape splits the line
                ; at the cursor and keeps editing. on submit the buffer goes to
                ; the END of history (the zipper re-merged in original order --
                ; an edited recall's pristine original was already restored by
                ; detach at the first edit), capped at 100 entries.
                (c = 10) (: vs (parseall (flatten u l r d))
                            (? (vs = m)
                               (loop npra (rev l >< u) () r d (detach hu cur) hd ())
                               (: _ (? (two? d) (csi (tally d) 66) ())
                                  _ (putc 10)
                                  nhu (? !vs (revcat hd (detach hu cur))
                                         (take 100 (mkframe u l r d >< revcat hd (detach hu cur))))
                                  `(vs nhu))))
                (0 < c)  (loop npra u (c >< l) r d (detach hu cur) hd ())
                (c = -1) (edleft  kloop u l r d)
                (c = -2) (edright kloop u l r d)
                (c = -3) (edbsp   kedit u l r d)
                (c = -4) (eddel   kedit u l r d)
                (c = -5) (edhome  kloop u l r d)
                (c = -6) (edend   kloop u l r d)
                (c = -8) (? (two? u) (edup    kloop u l r d)
                            (uphist   khist u l r d hu hd cur))
                (c = -9) (? (two? d) (eddown  kloop u l r d)
                            (downhist khist u l r d hu hd cur))
                (c = -10) (edtop kloop u l r d)
                (c = -11) (edbot kloop u l r d)
                (c = -12) (: _ (puts "^C") _ (putc 10)
                             (? (emptybuf u l r d) eofsym (edline hu hd)))
                (loop npra u l r d hu hd cur)))
        (loop 0 () () () () hu hd ()))

   ; evaluate a single parsed datum cooperatively. (ev v) is spawned as a
   ; worker; the main task polls done?/key? and yields. on Ctrl+C (byte 3)
   ; the worker is killed; any other byte is silently dropped (matches most
   ; shells). prints the result (or ^C) before returning -1; caller adds
   ; the trailing newline.
   ;
   ; this poll is the repl's control-flow seam under the help model: a
   ; worker that scares is terminated by the default help and done? flips,
   ; so the repl survives evaluation errors by construction. a global
   ; `help` function gets control at the scare (condition data on struct g)
   ; BEFORE the task dies; either way done?/key? is where the repl resumes.
   (do_eval v)
     (: pid (twirl ev v)
        (poll _)
          (? (back? pid) (: r (catch pid) _ (putx r) -1)   ; the answer
             (cue? 0)    (: c (getc 0)
                            (? (c = 3) (do (puts "^C") (freeze pid) -1)
                               (poll ())))
             (do (rest 0) (poll ())))
        (poll ()))

   ; the read-eval-print loop proper. the host and kernel frontends start the
   ; shell via (shell 0), which calls (repl 0 0).
   (repl hu hd)
     (: r (edline hu hd)
        (? (r = eofsym) 0
           (: vs  <r
              nhu <>r
              nhd >>r
              _ (each vs (\ v (: _ (do_eval v) (putc 10))))
              (repl nhu nhd))))

   ; welp -- the floor handler, help that has given up: keep the read protocol
   ; (incomplete -> the port back, eof -> the sentinel) and answer the zero
   ; point on a bare scare. this is the fromempty the introductions (scare,
   ; missing) never named: from any condition, a value -- and the value of the
   ; absorbed condition is the point, the held inhabitant of nothing, so
   ; (i love you) stays 1. any policy that runs out of cases delegates here:
   ; (\ s a b (? (mine? a) .. (welp s a b))). the point is captured HERE,
   ; helpless at load -- the body cannot say (:), since reading the missing
   ; head under an installed help would recurse.
   welp
     (: zp (:)
        (\ s a b (? (eof? s) b (more? s) a zp)))

   ; the shell's default help: a bare scare prints its condition -- ;; a b --
   ; with an honest face, then welps to the point so the session survives
   ; every raise (the answer must be a unit: a number would exponentiate under
   ; a numeral). every other status -- the more bits -- welps straight through,
   ; reproducing the read protocol exactly. (helpless, the C default is
   ; terminal and wears the oom banner.)
   shell-help
     (\ s a b
        (? (scare? s) (: _ (puts ";; ") _ (putx a) _ (puts " ") _ (putx b) _ (putc 10) (welp s a b))
           (welp s a b)))

   ; the interactive entry: install shell-help IF no help is present. the CHECK
   ; reads the book directly with a default -- (peep book 'help 0), peep's
   ; named-absence lane -- NOT a bare (ev 'help): a live read of an ABSENT help
   ; would trip the always-on `;; missing help` warning (the helpless missing-read
   ; path probes `help` itself to decide terminal-vs-raise, so reading help-while-
   ; absent reads as helpless). `book` folds to the book object at bake (captured
   ; like rng-state's), live past the image's surface-seal. the DEFINE still goes
   ; through ev -- a live top-level pin, so a later (: help 0) uninstall is seen.
   ; file mode never comes through here, so scripts keep the helpless (terminal) law.
   ; raw the tty BEFORE reading: the editor (edraw/edline) is the sole echo, so
   ; the kernel's cooked-mode echo must be off or every line doubles. main.c only
   ; raws on the no-arg path (replp = !argp); the bin/bao launch passes argv
   ; (love -l bao.l -e "(bao 0)"), so argp is set and that raw_mode never fires --
   ; the shell owns it here instead, covering the launcher AND a manual -e launch.
   ; the nif captures cooked once + restores via atexit on exit.
   (shell _)
     (: _ (? !(member? 'help (names ())) (ev `(': 'help shell-help)))
        _ (raw 1)
        _ (puts (";3 love " + love-version + "\n"))
        (repl 0 0))

   ; --- the host-only pty wrapper (wrap) ---
   ; reachable from the bao prompt to pty-wrap a host program -- (wrap (L
   ; "python3")). it uses the pty nifs (ptyrun, gather) which live in host/posix.c
   ; and are NOT present in love0 or the kernel -- there mind/gather are the
   ; want-stubs, so (wrap ..) answers a clean (). on the host they are the live
   ; nifs. NOTE: drives the real in/out tty fds, so it is exercised
   ; manually (the in-process gate covers the master read/write lanes; see
   ; boot/pty.l).

   ; pump bytes from port `src` to port `dst`, one at a time, until src hits EOF
   ; (see -> -1); flush after each byte keeps the far side prompt. (Same shape as
   ; ain's pump -- prel stays socket/pty-free, so each app carries its own.)
   (pump src dst) (: ch (see src) (? (ch = -1) 0 (: _ (put dst ch) _ (flush dst) (pump src dst))))

   ; poll a non-blocking (gather pid) to its exited PAIR. the pair is told from
   ; still-running (0) STRUCTURALLY (two?), never by net: the status pair (0)
   ; nets blue, so ! / nil? can't tell it from 0 -- only two? can.
   (waitfor pid) (: s (gather pid) (? (two? s) s (0 = s) (do (rest 2) (waitfor pid)) s))

   ; (wrap argv) -- run argv on a fresh pty and become a TRANSPARENT bidirectional
   ; pump: stdin -> the child, the child's output -> stdout, both live at once
   ; (the two pumps park on different fds and interleave through the cooperative
   ; scheduler, like ain). The teardown differs from ain's SYMMETRIC network
   ; pump, though: a wrapped PROCESS can exit while stdin is still open, so the
   ; child's death is the primary signal -- master->stdout runs HERE and ends on
   ; master EOF (the child gone); the stdin->master side runs in `up` and, on the
   ; user's own EOF, forwards VEOF (byte 4) so a canonical child exits. funnel
   ; through one death: when master EOF lands, join `up` -- wait if it already
   ; finished (user EOF first), else chill it (still parked on stdin; the child is
   ; dead so dropping pending input is correct, not ain's data-loss case).
   ; returns the child's exited status pair, or ptyrun's errno fixnum on hatch
   ; failure.
   (wrap argv)
     (: r (mind argv)
        (? (two? r)
           (: pid <r m >r
              up (twirl (\ _ (: _ (pump in m) (put m 4))) 0)   ; stdin -> child; VEOF on user EOF
              _ (pump m out)                                      ; child -> stdout; ends when the child dies
              _ (? (back? up) (catch up) (freeze up))                ; join the input pump through the one death
              _ (flush out)
              _ (close m)
              (waitfor pid))                                      ; gather the child
           r))

   ; --- the raw-line editor (bao's own line reader) ---
   ; repl's `edline` is wrong for wrapping a host program: it parses the buffer
   ; as love forms (submits only when it reads complete) and prints its own `> `. a
   ; wrapped program (bash, python) wants the RAW bytes, submitted on Enter no
   ; matter what. So bao reads its own line -- reusing the PURE zipper primitives
   ; (edleft/edright/edbsp/eddel/edhome/edend/revcat/edrender, all in/out-free for
   ; the logic), but with PORT-PARAMETERIZED event decode so the input is any
   ; port: `in` for the tty, a (tap ..) for tests.
   ;
   ; bev/besc/besc1 are edev/edesc/edesc1, reading (see ip) instead of the
   ; global getc -- event codes are identical (printable >0; -1 left, -2 right,
   ; -3 bsp, -4 del, -5 home, -6 end, -7 quit/EOF; 10 = enter; up/down/^C ignored
   ; by a single-line reader).
   (besc1 ip c) (? (c = 126) -5
                   (c = 59) (: _ (see ip) (peep arrow (see ip) 0)))  ; a line reader never jumps the buffer: the PLAIN finals
   (besc ip) (? !(91 = see ip) 0
     (: c (see ip) e (peep arrow c 0)
        (? !(id? e 0) e
           (c = 49) (besc1 ip (see ip))
           (c = 55) (: _ (see ip) -5)
           (c = 52 || c = 56) (: _ (see ip) -6)
           (c = 51) (: _ (see ip) -4) 0)))
   (bev ip) (: c (see ip) e (peep evk c 0)
     (? !(id? e 0) e
        (c = 27) (besc ip)
        (c = 9) -13                                   ; Tab -> the completion event (edlnc's; others ignore)
        (32 <= c && c < 127 && c)))

   ; (edraw ip pr) -- read ONE line off port `ip`, with in-line editing, prompt pr
   ; echoed to out. Enter submits the raw line (its bytes, in order); ^D / port EOF
   ; returns eofsym. single line, no history, no parseall -- the text goes straight
   ; back to the caller (bao's wrapper feeds it to a child). the buffer is the
   ; (l . r) halves of the zipper with the line count (u d) pinned empty, so the
   ; single-line primitives drive it unchanged.
   (edraw ip pr)
     (: _ (puts pr) pl (tally pr)
        (loop l r)
          (: _ (edrender pl 0 () l r ())
             c (bev ip)
             (km uu ll rr dd) (loop ll rr)         ; motion: edleft/.. give (k u l r d), keep l r
             (? (c = -7) eofsym                     ; ^D / EOF -> the sentinel
                (c = 10) (: _ (putc 10) (revcat l r)); Enter -> submit the raw line, in order
                (0 < c)  (loop (c >< l) r)           ; insert a printable
                (c = -1) (edleft  km () l r ())
                (c = -2) (edright km () l r ())
                (c = -3) (edbsp   km () l r ())
                (c = -4) (eddel   km () l r ())
                (c = -5) (edhome  km () l r ())
                (c = -6) (edend   km () l r ())
                (loop l r)))                         ; up/down/^C/unknown: ignore in a line reader
        (loop () ()))

   ; --- the shell line reader: edraw + command history -------------------------
   ; edraw is historyless; a command shell (lush, crew/lush/) wants up/down recall. edln
   ; keeps a flat history -- past lines as charlists, most-recent first -- and an
   ; index i into it: i = 0 is the live draft (saved in `draft` the instant up
   ; leaves it), i = k recalls the (k-1)th entry. up steps older, down newer;
   ; editing a recall just edits the buffer (i and hist untouched), so the original
   ; survives and the EDITED line is what Enter records on front. reuses the pure
   ; zipper primitives + edrender + bev EXACTLY like edraw -- only the loop state
   ; and the submit/recall policy differ (the edraw-vs-edline split, once more).
   ;
   ; (edln ip pr hist) -> 'eof at ^D / port EOF, else (line . nhist): the raw line
   ; (a charlist) and hist with the line pushed on front (blank lines dropped,
   ; capped at 100). thread nhist back in for recall across commands.
   ;
   ; (edlnc ip pr hist comp) is edln + Tab completion: comp takes the text LEFT
   ; of the cursor (a charlist, in order) and answers (insert . candidates) --
   ; insert lands at the cursor (a charlist), candidates (strings) print under
   ; the line when there is more than one way forward, then the renderer
   ; re-prompts fresh (pra 0: the prompt row is wherever we are). comp ()
   ; disables the key -- edln IS edlnc with no comp, so every old consumer
   ; rides unchanged.
   (edlnc ip pr hist comp)
     (: _ (puts pr) pl (tally pr) hn (tally hist)
        (hnth k) <(drop k hist)                       ; hist[k], 0-based (k < hn)
        (loop l r i draft)
          (: _ (edrender pl 0 () l r ())
             c (bev ip)
             (km uu ll rr dd) (loop ll rr i draft)     ; motion: index + draft kept
             (recall ni)                               ; jump to view index ni (0 = the draft)
               (: nd (? (i = 0) (revcat l r) draft)    ; save the live draft once, the instant up leaves it
                  ln (? (ni = 0) nd (hnth (ni - 1)))
                  (loop (rev ln) () ni nd))
             (? (c = -7) 'eof
                (c = 10) (: _ (putc 10)
                            ln (revcat l r)
                            nh (? !ln hist (take 100 (ln >< hist)))
                            (ln >< nh))
                (0 < c)  (loop (c >< l) r i draft)
                (c = -12) (: _ (puts "^C") _ (putc 10) (() >< hist))   ; ^C: abandon the line, hist untouched
                (c = -13)
                  (? !comp (loop l r i draft)
                     (: t (comp (revcat l ()))
                        (? !(two? t) (loop l r i draft)
                           (: l2 (foldl (\ a ch (ch >< a)) l <t)
                              cs >t
                              _ (? (two? cs && two? >cs)
                                   (: _ (putc 10)
                                      _ (puts (foldl (\ a s (? (tally a) (a + "  " + s) s)) "" cs))
                                      (putc 10))
                                   0)
                              (loop l2 r i draft)))))
                (c = -1) (edleft  km () l r ())
                (c = -2) (edright km () l r ())
                (c = -3) (edbsp   km () l r ())
                (c = -4) (eddel   km () l r ())
                (c = -5) (edhome  km () l r ())
                (c = -6) (edend   km () l r ())
                (c = -8) (recall (? (i < hn) (i + 1) i))   ; up: older
                (c = -9) (recall (? (0 < i) (i - 1) i))    ; down: newer
                (loop l r i draft)))
        (loop () () 0 ()))
   (edln ip pr hist) (edlnc ip pr hist ())

   ; (bao a) -- the interactive entry the host frontend launches. `(bao 0)` is
   ; exactly the baked shell (an arg is taken: love has no nullary calls).
   ; `wrap` is reachable from the bao prompt; the editor-as-line-reader (edraw)
   ; and the debug `help` are the later modes (crew/bao.md).
   ;
   ; `(bao 'parts)` is the TEST DOOR: an opaque accessor over the editor primitives,
   ; the same shape `from` hands back for a module -- (B 'edleft) reaches one,
   ; (B 'keys) lists them, anything else answers 0. test/host/baotest.l writes the
   ; battery against it, so the CHECKS live with the tests while the NAMES stay
   ; closure-private, off the book. The tablet is built ON THE CALL, so a shell that
   ; never asks never pays for it.
   (bao a) (? (a = 'parts)
              (: p #('edleft edleft 'edright edright 'edbsp edbsp 'eddel eddel
                     'edhome edhome 'edend edend 'edtop edtop 'edbot edbot
                     'edup edup 'eddown eddown 'uphist uphist 'downhist downhist
                     'detach detach 'emptybuf emptybuf 'mkframe mkframe
                     'parses parses 'joinln joinln 'flatten flatten
                     'splitat splitat 'parseall parseall)
                 (\ q (? (id? q 'keys) (keys p) (peep p q 0))))
              (shell ()))


   ; the body: the public entry points, defglob'd below. everything above stays
   ; closure-private (off the book), which is what stops the ed* editor + repl
   ; internals from leaking into (names ()).
   `(shell welp edraw wrap bao edln edlnc))

 ; defglob the public surface out of the export list (book is mopped post-egg, so
 ; a top-level body-less (: ..) is the only way to add a global; the privates above
 ; never reach it -- the battery in test/host/baotest.l reads them through the
 ; (bao 'parts) door instead). the holder is `_` (the previous line's value, by
 ; the sequence law), so it never leaks either.
 shell <_
 welp  <>_
 edraw <>>_
 wrap  <>>>_
 bao   <>>>>_
 edln  <>>>>>_
 edlnc <>>>>>>_)

; bao.l is DEFINE-ONLY -- loading it (egg-bake or `-l love/bao.l`) installs the shell
; core but does NOT launch. The frontend launches: host/main.c evals `(bao 0)` on a
; tty (so `love` on a terminal is the bao shell), a pipe stays the bare stdin runner
; `(reads in)`, and the kernel calls `(shell 0)`. Define-only is what makes bao
; loadable for tests (load the defs, read piped forms with the bare reader).
