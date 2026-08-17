; crew/sb/up.l -- the up/down verbs + the verb table: the dist artifact's rail
; (doc/sb.md, the install answer: install = sync + cook build). this file rides
; the dist cat LAST (crew/build.mk distfiles) and module 'sb -- sb-sync/dirty
; are the module's own, herald the host nif -- and its tail pins into
; `verbs`, the table love/cli.l's verb rail walks. `verbs` is unbound everywhere
; else, so the rail is a property of what is BAKED, not of the language: only an
; artifact whose bake evaluated this file answers `love up`.
;
; up [URL]: sb-sync ~/.love/src from URL (an http .sb/ tree or a directory),
; then `self cook install` INSIDE it -- cook parses the repo Makefile and lands
; the ~/.love nest (rung 1's cook-native install). re-running up IS the update:
; sync pulls only what is new, cook rebuilds only what is stale. the URL default
; is `origin`, pinned ahead of this file by the dist cat when the build set one
; (make dist DIST_ORIGIN=...); unbound it reads the zero point and up asks for
; an explicit URL.
;
; down [-f]: the uninstall twin -- the nest whole, plus only those ~/.local
; links that resolve into it. unrecorded work in src REFUSES without -f (down
; eats the tree; svalbard knows whether it holds anything unsaved). ~/.vim is the
; user's; down leaves it alone.
(module 'sb
(: (oops m) (: _ (say err m) _ (put err 10) (quit 1))

   ; the nest paths, off HOME -- the seat walk's own convention, no env knobs
   (nestof _) (: h (getenv "HOME")
                 _ (? h 0 (oops "up: no HOME in the environment"))
                 (h + "/.love"))

   ; --- up ---------------------------------------------------------------
   (up-main as)
    (: url (? (two? as) <as origin)
       _ (? url 0 (: _ (say err "up: no origin baked in -- love up URL")
                     _ (put err 10) (quit 2)))
       nest (nestof ()) src (nest + "/src")
       _ (mkdir nest 493) _ (mkdir src 493)              ; EEXIST from a prior up is fine
       _ (? (charm? (chdir src)) (oops ("up: cannot enter " + src)) 0)
       _ (puts ("up: syncing " + src + " from " + url + "\n"))
       r (sb-sync url)
       _ (? r 0 (oops "up: the sync did not land"))
       exe (selfpath 0)
       ; the toolchain is US: unless the environment names a CC, the nest
       ; builds under this very binary's mooncc verb -- love0 compiles there,
       ; and love0 carries the graph on as always (mooncc0.image, the moon
       ; lane) -- so the download door needs no ambient compiler at all. an
       ; explicit CC (the git door's DDC leg) always wins. the embedding goods
       ; (liblove, love.h) are the cc lane's own: up skips them (EMBED=0).
       _ (? (getenv "CC") 0 (setenv "CC" (exe + " mooncc")))
       _ (? (getenv "EMBED") 0 (setenv "EMBED" "0"))
       _ (puts "up: cooking the nest (cook install)\n")
       s (herald (list exe "cook" "install"))
       st (? (two? s) <s s)
       _ (? (0 = st) 0 (oops ("up: cook install failed (exit " + show st + ")")))
       _ (puts ("up: the nest is " + nest + " -- love (and sb, cook, kore ..) on ~/.local/bin\n"))
       (quit 0))

   ; --- down -------------------------------------------------------------
   ; zap: rm -r that NEVER follows a link -- readlink answers the target string
   ; for a symlink and the REAL () otherwise, so the link test comes first and
   ; a linked directory loses its link, keeps its tree.
   (zap p)
    (? !(id? (readlink p) ()) !(unlink p)
       (dir? p) (: (go m ok) (? !(two? m) ok
                                (: k (zap (p + "/" + <m)) (go >m (? k ok 0))))
                   ok (go (readdir p) 1)
                   (ok && !(rmdir p)))
       !(unlink p))

   ; reap: unlink the links in d whose target leads with `nest` -- ours and only
   ; ours (a ~/.local/bin/cook pointing elsewhere is somebody else's cook)
   (reap d nest)
    (: (go m n) (? !(two? m) n
                   (: p (d + "/" + <m) t (readlink p)
                      hit (!(id? t ()) &&
                           (<= (tally nest) (tally t)) &&
                           (snip t 0 (tally nest) = nest))
                      _ (? hit (unlink p) 0)
                      (go >m (? hit (n + 1) n))))
       (go (readdir d) 0))

   (down-main as)
    (: f? (two? as && (<as = "-f"))
       nest (nestof ()) src (nest + "/src")
       here (cwd 0)
       d (? f? 0
            (? (two? (stat (src + "/.sb/snap")))     ; presence by stat, never by emptiness
               (: _ (chdir src) n (dirty ()) _ (chdir here) n)
               0))
       _ (? (0 < d)
            (oops ("down: " + show d + " unrecorded change(s) in " + src
                   + "  -- sb record them, or love down -f"))
            0)
       _ (? (two? (stat nest)) 0 (: _ (puts ("down: no nest at " + nest + "\n")) (quit 0)))
       _ (chdir (getenv "HOME"))                       ; never rm the tree we stand in
       _ (? (zap nest) 0 (oops ("down: could not remove all of " + nest)))
       nb (reap (getenv "HOME" + "/.local/bin") nest)
       nm (reap (getenv "HOME" + "/.local/share/man/man1") nest)
       _ (puts ("down: removed " + nest
                + (? (0 < (nb + nm)) (" and " + show (nb + nm) + " ~/.local link(s)") "")
                + "\n"))
       (quit 0)))

; up and down, pinned straight into the registry (love/verbs.l). the tablet is the real one
; -- a module IS its tablet -- so this is the whole registration: no table to hand over,
; nothing to rebind, and a frontend that never registered `verbs` fails LOUDLY here, which is
; what a bake wants. FN takes the args AFTER the name and ANSWERS a status charm; both rails
; quit with it, so a verb that returns needs no exit of its own.
; ⚠ ONLY these two. sb, cook, kore (and its whole applet table), kiosko, mooncc, sh and
; lush each pin their own name at their own foot now -- an app owns the word it answers to,
; and this file owning them meant the artifact was multi-call only because up.l happened to
; be catted last. up and down are up.l's own, and there is nothing else here to know.
(: T (from 'verbs 'tab)
   _ (pins T
      'up up-main
      'down down-main))

) ; module 'sb
