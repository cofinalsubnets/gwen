; bao -- the shell core: the read-eval-print loop, its multi-line line editor,
; the stream shell (read/reads), the floor handler (welp), and the host-only
; pty wrapper (wrap). bao is the BAKED shell core for all three frontends -- the
; host (out/host/love), the freestanding kernel (kmain.c), and the love0 bootstrap
; self-test -- loaded after the prel.
;
; parsing is `sound`, the reader in love (love/p1.l): the editor's flattened
; charlist is implicitly parenthesized and read as ONE expression -- a line
; `x y z` is the application (x y z), and infix works at the prompt (factored at
; compile time by opfix; reading is structural). a line that ends mid-shape comes
; back TORN and the editor keeps taking input.
;
; the editor buffer is a four-zipper threaded positionally through
; the editor functions: u (lines above the cursor, reversed), l (chars
; left of cursor on the current line, reversed), r (chars at/after
; cursor on the current line, in order), d (lines below).

; read: chars -> top, the reverse shell of the compiler -- the lane every shell
; ultimately feeds into. `s` is a charlist: a bare one (a charm is a sat, a non-negative
; fixnum), a flowing one, or a PORT, which is lifted to the flowing kind and is the only
; case that needs saying. read takes any of them, reads ONE form (structurally -- the
; reader knows no operators) and sends it through ev's core (opfix, feel, ana, cata): the
; full hull chars -> top in one call. nothing readable -- empty input or an unfinished
; shape -- is nothing.
(: (read s)
   (: r (sound (? (hot? s) (flow s) s))
      (? (two? r) (ev <r) 0)))
; reads: the stream shell -- reads and evals every form of a port, the value the last
; form's; ev late-binds ((ev 'ev r)) so one definition drives both of love0's passes.
; ⚠ THE TWO ENDINGS DIFFER: a clean end comes BETWEEN forms (done), `torn` INSIDE one.
; and `torn` is no longer a maybe: the reader drank for itself and found the end, so
; there is nothing left to try and it is a scare.
(: (reads p)
   ((: (g cl z)
       (: r (sound cl)
          (? (two? r)       (g >r (ev 'ev <r))
             (id? r torn)   (scare 'unfinished-form p)
             z)))
    ; ⚠ GULP ONLY WHAT WE OWN. a file (cli.l's load1) and a tap (the baked corpus,
    ; host/main.c's runner) are ours alone -- nobody else reads them, so running
    ; ahead is free and worth it. `in` is SHARED: the forms we eval can read it,
    ; and a child can inherit its fd. so stdin trickles.
    (? (id? p in) (trickle p) (flow p)) 0))
; forms: every datum of a port, as DATA -- the data twin of `reads`, and the shape half
; the tools had each copied out by hand. it takes a PORT, like reads, so it needs neither
; open nor close (a fs-less frontend has neither, and a bare mention would raise at the
; define). ⚠ a torn tail SCARES: the other choice is handing back the forms that happened
; to land, which reads exactly like a whole file and is the bug every one of those had.
(: (forms p)
   ((: (g cl acc)
       (: r (sound cl)
          (? (two? r)       (g >r (<r >< acc))
             (id? r torn)   (scare 'unfinished-form p)
             (rev acc))))
    (flow p) ()))

; --- the shell core: editor + repl + welp + the pty wrapper, all closure-private.
; the body of this (: ..) returns the public entry points as a list, defglob'd
; below; everything else (the ed* editor, parseall, do_eval, the history zipper,
; pump/waitfor, the raw-line decode) stays inside this scope, OFF the global book.
(: _                                ; the export-list holder: `_` never leaks to the book
 (: m (nom 0) eofsym (nom 0)

   ; the host nifs the shell wants -- raw (termios), the pty pair (tether gather),
   ; close -- are not on every frontend: the kernel pins none of them, love0
   ; lacks the ptys. bind the global when it exists, else a no-op stub; the
   ; mentions below read these locals, so bao compiles quiet everywhere and a
   ; call answers a clean () where the facility is not.
   (want n) (? (member? n (names ())) (ev n) (\ _ ()))
   raw (want 'raw) tether (want 'tether) gather (want 'gather) close (want 'close)

   (revcat a b) (foldl (flip link) b a)

   ; parse a charlist as ONE expression. read EVERY top-level datum off the line:
   ; a SINGLE datum is used as-is -- so a leading monadic like `+(-3)` keeps fusing
   ; (wrapping it in () would put the operator in HEAD position, where it never
   ; fuses -- the old `(+(-3))` -> `(+ -3)` bug); MULTIPLE datums become the
   ; application `(a b c)`, so `f x` applies and `1 = 1` factors infix. returns m if
   ; the line ends inside an unfinished shape, () for a blank line, else a one-form list.
   (rdall cl acc) (: r (sound cl)
                    (? (two? r)     (rdall (cup r) (<r >< acc))
                       (id? r torn) m              ; unfinished shape -> more
                       (rev acc)))                 ; a clean end -> the datums, in order
   (parseall cl)
     (: ds (rdall cl ())
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
   ; besc/bev for a port). ⚠ 0 is the miss, read with id?: an event code nets
   ; RED, so truthiness cannot tell hit from miss.
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

   ; redraw the buffer in place. pra is rows above the cursor from the PREVIOUS
   ; render -- move up that many first, so we land back on the prompt's row. all
   ; motion is relative, so terminal scrolling never desyncs us. pl is the prompt's
   ; column; the topmost line shares it, later lines start at column 0.
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

   ; print the prompt, then dispatch events until ^D or a submitted buffer.
   ; returns eofsym on ^D, else (vs . nhu) so the repl can thread history.
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
             ; the dispatch stays a CHAIN, not a table: every arm runs a different
             ; continuation over the live buffer, so a table would hold closures
             ; built per keystroke. the tables above work because their arms are constants.
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
   ; the trailing newline. a worker that scares is terminated by the default
   ; help and landed? flips, so the repl survives evaluation errors by construction.
   (do_eval v)
     (: pid (twirl ev v)
        (poll _)
          (? (landed? pid) (: r (catch pid) _ (putx r) -1)   ; the answer
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

   ; welp -- the floor handler, help that has given up: answer the zero point,
   ; whatever the condition. (it kept the read protocol too, until the reader
   ; stopped raising one.) this is the fromempty the introductions (scare,
   ; missing) never named: from any condition, a value -- and the value of the
   ; absorbed condition is the point, the held inhabitant of nothing, so
   ; (i love you) stays 1. any policy that runs out of cases delegates here:
   ; (\ s a b (? (mine? a) .. (welp s a b))). the point is captured HERE,
   ; helpless at load -- the body cannot say (:), since reading the missing
   ; head under an installed help would recurse.
   welp
     (: zp (:)
        (\ s a b zp))

   ; the face a raise wears -- ;; a b -- ⚠ ON ERR: a DIAGNOSTIC, and a script's stdout is the
   ; script's (the C print this stands in for always went there). the two defaults share it and
   ; differ only in WHAT SURVIVES: both welp to the point (a unit -- a number would exponentiate)
   ; and pass the more bits through, but at the PROMPT every raise survives where in FILE MODE
   ; only a missing does. a deliberate scare stays terminal there; 239 sites lean on it.
   (hface a b) (: _ (say err ";; ") _ (print err a) _ (say err " ") _ (print err b) (put err 10))
   shell-help (\ s a b (: _ (? (scare? s) (hface a b)) (welp s a b)))
   file-help  (\ s a b (: _ (? (scare? s) (hface a b))
                          (? (? (scare? s) (nil? (id? a 'missing)) ()) (quit 1) (welp s a b))))

   ; INSTALLED AT LOAD, not at the prompt: file mode is where nobody was listening. an unbound
   ; global already surfaced from C, a `:` read before its pin did not, and both answer the zero
   ; point -- a typo reads () and (() x) is 1, a fake result wearing a real one's face.
   ; (heard ()) is the live read of the installed help (the hot_help slot), so the
   ; question is what it IS, not whether it is there -- and an already-heard help
   ; (a frontend's own) is respected, never clobbered.
   _ (? (nil? (heard ())) (hear file-help))
   ; ⚠ raw the tty BEFORE reading: the editor is the sole echo, so cooked-mode echo
   ; must be off or every line doubles.
   (shell _)
     (: _ (hear shell-help)             ; the prompt survives every raise; a script must not
        _ (raw 1)
        _ (puts (";3 love " + love-version + "\n"))
        (repl 0 0))

   ; --- the host-only pty wrapper (wrap) ---
   ; reachable from the bao prompt to pty-wrap a host program -- (wrap (L
   ; "python3")). it uses the pty nifs (ptyrun, gather) which live in host/posix.c
   ; and are NOT present in love0 or the kernel -- there tether/gather are the
   ; want-stubs, so (wrap ..) answers a clean (). it drives the real in/out tty
   ; fds, so it is exercised manually.

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
   ; scheduler). the teardown is ASYMMETRIC: a wrapped process can exit while stdin
   ; is still open, so the child's death is the primary signal -- master->stdout runs
   ; HERE and ends on master EOF; the stdin->master side runs in `up` and, on the
   ; user's own EOF, forwards VEOF (byte 4) so a canonical child exits. when master
   ; EOF lands, join `up` -- wait if it already finished, else chill it (the child is
   ; dead, so dropping pending input is right). returns the exited status pair, or
   ; the errno fixnum on hatch failure.
   (wrap argv)
     (: r (tether argv)
        (? (two? r)
           (: pid <r m >r
              up (twirl (\ _ (: _ (pump in m) (put m 4))) 0)   ; stdin -> child; VEOF on user EOF
              _ (pump m out)                                      ; child -> stdout; ends when the child dies
              _ (? (landed? up) (catch up) (freeze up))                ; join the input pump through the one death
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
   ; port: `in` for the tty, a (tap ..) for tests. bev/besc/besc1 are
   ; edev/edesc/edesc1 reading (see ip) instead of the global getc.
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
   ; survives and the EDITED line is what Enter records on front.
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
   ;
   ; `(bao 'parts)` is the TEST DOOR: an opaque accessor over the editor primitives,
   ; the same shape `from` hands back for a module. the battery writes against it, so
   ; the CHECKS live with the tests while the NAMES stay closure-private, off the
   ; book. the tablet is built ON THE CALL, so a shell that never asks never pays.
   (bao a) (? (a = 'parts)
              (: p #('edleft edleft 'edright edright 'edbsp edbsp 'eddel eddel
                     'edhome edhome 'edend edend 'edtop edtop 'edbot edbot
                     'edup edup 'eddown eddown 'uphist uphist 'downhist downhist
                     'detach detach 'emptybuf emptybuf 'mkframe mkframe
                     'parses parses 'joinln joinln 'flatten flatten
                     'splitat splitat 'parseall parseall)
                 (\ q (? (id? q 'keys) (keys p) (peep p q 0))))
              (shell ()))


   `(shell welp edraw wrap bao edln edlnc))

 ; defglob the public surface out of the export list: book is mopped post-egg, so a
 ; top-level body-less (: ..) is the only way to add a global. the holder is `_` (the
 ; previous line's value, by the sequence law), so it never leaks either.
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
