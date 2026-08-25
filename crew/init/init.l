; --- init: a service supervisor modeled in love (a runnable sketch) ---
; the EXECUTABLE companion to the "love as pid 1" idea -- the Tier-1 init (runit/s6/
; dinit class: own unit format, no D-Bus): a perceive->decide->act supervisor with a
; watchdog over checkpointable state, driving PROCESSES + signals over the host
; kernel's stack.
;
; like boot/tag.l this is a SKETCH -- it models the parts that STAY love so
; the shape runs and is probed on the host. there are now TWO lanes:
;   (demos 0) -- the MODELED supervisor: a deterministic tour where a "service" is a
;     green task and a crash is a `scare` the watchdog `help` welps to () (the SAME
;     shape recovery inle uses), so the dep graph / restart policy / checkpoint run
;     with no OS processes. (the oneshot lane here is already real via the `hark` nif.)
;   (real 0) -- the REAL supervisor: services are OS processes. src/posix.c adds the
;     two missing primitives -- (spawn argv) forks+execs and returns a pid to track,
;     (glean _) is the SIGCHLD core (waitpid(-1) -> (pid . status), restart per policy).
;     same supervisor shape, now over real children. (demos 0) ends by calling it.
; run either:
;   out/host/love -l init/init.l -e "(demos 0)"      ; or "(real 0)"
; (the file only DEFINES -- a plain load prints nothing; -l preloads it into the shared
; global scope so the -e call finds `demos`/`real`.)
;
; what an ACTUAL pid1 still needs is small and core-clean (src/posix.c via the AiNif
; glob, no love.c/love.h edit -- the same discipline as ain/bao). DONE: fork/execve +
; the non-blocking waitpid reaper (spawn/glean). REMAINING: signalfd (SIGCHLD/SIGTERM
; become perceive events, merged with timers by ai_wait_fds exactly like {nic, clock}),
; mount (early /proc, /sys, devtmpfs), and the cgroup-v2 writes. everything BELOW that
; is this file -- the dependency graph, the restart policy, the checkpoint -- already love.

(: (note s) (: _ (puts s) (puts "\n"))               ; the narrator: a line to the console

   ; --- a unit is an IMMUTABLE list (name kind cmd restart deps) --- show/read
   ; checkpoints it for free (the inle.md rule: state is a love value, persistence is
   ; show/read). `kind` is 'oneshot (a real argv for `hark`) or 'service (a thunk modeling
   ; the long-running process). `restart` is 'no | 'on-fail | 'always (systemd's Restart=).
   ; `deps` is the After= list -- names that must be up before this one starts.
   (nth l i) (? (= i 0) (cap l) (nth (cup l) (- i 1)))
   (unit name kind cmd restart deps) [name kind cmd restart deps]
   (u-name u) (nth u 0)   (u-kind u) (nth u 1)   (u-cmd u) (nth u 2)
   (u-restart u) (nth u 3) (u-deps u) (nth u 4)
   (find units name) (? !units ()                    ; the unit with this name, or ()
                        (? (id? (u-name (cap units)) name) (cap units)
                           (find (cup units) name)))
   (rev l) (foldl (\ a x (link x a)) () l)            ; prepend-fold = reverse

   ; --- the registry --- a live book name->status ('up | 'done | 'failed; absent =
   ; 'down). MUTABLE (a book), the one place state is shared while units come and go;
   ; `snap` reads it back to the honest list for the checkpoint. a unit is "started"
   ; once it is up (a service running) or done (a oneshot that completed).
   (up? reg name) (: s (peep reg name 'down) (? (id? s 'up) 1 (id? s 'done)))

   ; --- the watchdog: supervise one SERVICE (Restart=) --- twirl the thunk (the modeled
   ; process); if it faults, the `help` below welps the task to (), so `catch` comes back
   ; falsey and we RESTART per policy, counting attempts in `cnt`. clean exit (a truthy
   ; return) -> up. give up after `tries-cap`. on metal: fork+exec, and THIS loop is the
   ; SIGCHLD handler re-spawning the pid per its Restart= -- identical control flow.
   tries-cap 3
   (supervise name thunk policy cnt)
     (: p (twirl thunk 0)
        r (catch p)
        (? r 1                                        ; clean -> running
           (: n (+ 1 (peep cnt name 0)) _ (pin cnt name n)
              (? (? (id? policy 'no) 0 (< n tries-cap))
                 (: _ (note (+ "    " (+ (show name) (+ " crashed -> restart #" (show n)))))
                    (supervise name thunk policy cnt))
                 (: _ (note (+ "    " (+ (show name) " gave up"))) ())))))

   ; --- act: bring ONE unit up --- a oneshot really forks/execs/waits a child (the wire
   ; out to a real process: `hark` returns (exit-code . captured-stdout), so the REAL exit
   ; status drives done/failed). a service is supervised. returns the registry with this
   ; unit's status pinned.
   (start1 reg cnt u)
     (: name (u-name u)
        (? (id? (u-kind u) 'oneshot)
             (: res  (hark (u-cmd u))                  ; REAL child (src/main.c fork+exec+wait)
                code (cap res)  text (cup res)         ; (exit . stdout)
                _    (note (+ "  [oneshot] " (+ (show name) (+ " -> \"" (+ text (+ "\" (exit " (+ (show code) ")")))))))
                (pin reg name (? (= code 0) 'done 'failed)))
           (: ok (supervise name (u-cmd u) (u-restart u) cnt)
              _ (note (+ "  [service] " (+ (show name) (? ok " up" " FAILED"))))
              (pin reg name (? ok 'up 'failed)))))

   ; --- decide+act: dependency-ordered boot --- bring `name` up, but its After= deps
   ; FIRST (a DFS over the graph; up? dedups so a shared dep starts once). `boot` folds
   ; the whole unit list through it. this is the topological start systemd orders by
   ; Requires=/After=; a real init would also detect cycles (no progress -> raise).
   (ensure reg cnt units name)
     (? (up? reg name) reg
        (: u    (find units name)
           reg' (foldl (\ r d (ensure r cnt units d)) reg (u-deps u))  ; deps first (fresh
           (start1 reg' cnt u)))                                       ; name: a self-rebind reads missing)
   (boot units) (: cnt (tablet 0)
                   (foldl (\ reg u (ensure reg cnt units (u-name u))) (tablet 0) units))

   ; --- shutdown --- stop in REVERSE start order (dependents before their deps), the
   ; mirror of boot. modeled as a status flip; on metal: SIGTERM, wait, SIGKILL on a
   ; timeout, each in its cgroup.
   (stop1 reg name) (: _ (note (+ "  stop " (show name))) (pin reg name 'down))
   (shutdown reg names) (foldl stop1 reg names)

   ; --- checkpoint --- the live book -> an immutable (name status) list, show/read-able
   ; (a book does not round-trip; the list does). persistence is show/read, end to end.
   (snap reg names) (map (\ n [n (peep reg n 'down)]) names))

; --- install the watchdog as the global help (the floor that turns a service's fault
; into () so the supervisor sees a falsey `catch`) --- read control keeps the protocol;
; a real scare just welps (the supervisor narrates restarts itself; a true init would
; log the fault to the journal here). mirrors repl.l's shell-help, quieter.
(hear welp)

; --- the REAL supervisor (needs src/posix.c: spawn + glean; kill via bao's `still`) ---
; the same supervisor, now over OS PROCESSES. spawn each unit (a pid to track), then
; poll `glean` -- the SIGCHLD core -- mapping each dead pid back to its unit and
; restarting on a NONZERO exit per policy. on metal this poll is driven by signalfd
; through ai_wait_fds (perceive), exactly as {nic, clock} is in inle; here a rest-paced
; loop stands in. a real service's `cmd` is its argv (hark by spawn); `pids` maps
; pid->name, `tries` counts restarts. the dep graph / checkpoint above carry over
; unchanged -- only start1's "modeled thunk" becomes "a real fork+exec".
(: (rstart pids u)                                    ; spawn one unit, record pid->name
     (: pid (spawn (u-cmd u))
        _   (pin pids pid (u-name u))
        _   (note (+ "  spawn " (+ (show (u-name u)) (+ " pid=" (show pid)))))
        pid)
   ; the glean loop: poll until every service has SETTLED (live hits 0) or steps run
   ; out. each reaped (pid . status): a 0 exit (or a give-up) settles the unit; a
   ; nonzero exit under Restart=on-fail, within the cap, respawns it (live unchanged).
   (rloop units pids tries live steps)
     (? (? (< live 1) 1 (< steps 1)) 0                ; done: all settled / out of steps
        (: ev (glean 0)
           (? (two? ev)                               ; a child died: (pid . status)
                (: nm (peep pids (cap ev) 0)  st (cup ev)
                   u  (find units nm)
                   n  (+ 1 (peep tries nm 0))  _ (pin tries nm n)
                   _  (note (+ "  exit " (+ (show nm) (+ " status=" (show st)))))
                   (? (? (= st 0) 0 (? (id? (u-restart u) 'on-fail) (< n tries-cap) 0))
                        (: _ (note (+ "    restart " (+ (show nm) (+ " #" (show n)))))
                           _ (rstart pids u)
                           (rloop units pids tries live (- steps 1)))         ; still live
                        (rloop units pids tries (- live 1) (- steps 1))))     ; settled
              (: _ (rest 2) (rloop units pids tries live (- steps 1))))))      ; nothing yet: wait
   (real _)
     (: pids (tablet 0) tries (tablet 0)
        units [
         (unit 'worker 'service ["sh" "-c" "exit 0"] 'on-fail ())       ; exits 0 -> settles
         (unit 'flaky  'service ["sh" "-c" "exit 1"] 'on-fail ())]      ; exits 1 -> restart, then give up
        _ (note "-- REAL processes: spawn each, then poll glean (the SIGCHLD core) --")
        _ (each units (\ u (rstart pids u)))
        _ (rloop units pids tries (tally units) 500)
        (note "-- all real children reaped --")))

; --- the PERCEIVE lane (Linux: src/posix.c sigfd/sigtake + the core `await`) ---
; the supervisor's perceive becomes a real PARK on a signalfd, not a poll loop.
; (await sig) cooperatively yields until a signal lands; the scheduler folds the
; parked sigfd and a heartbeat TASK's timer into ONE ai_wait_fds -- the {nic, clock}
; merge, now {signals, clock}, so the heartbeat TICKS WHILE the supervisor sleeps on
; the fd. on SIGCHLD we drain glean (it COALESCES, so loop) and restart per policy; a
; SIGTERM (blocked -> queued to the fd: a graceful EVENT, not a kill) ends the loop.
; Linux signal numbers: SIGCHLD 17, SIGTERM 15. run: out/host/love -l init/init.l -e "(perceive 0)"
(: sig-chld 17  sig-term 15
   (heart n ms) (? (< n 1) 0 (: _ (rest ms) _ (note "  . beat") (heart (- n 1) ms)))

   ; drain every dead child after a SIGCHLD wake, restarting each per policy; return
   ; the new live count. SIGCHLD coalesces, so loop glean until it drains.
   (harvest units pids tries live)
     (: ev (glean 0)
        (? !(two? ev) live
           (: nm (peep pids (cap ev) 0)  st (cup ev)
              u  (find units nm)
              n  (+ 1 (peep tries nm 0))  _ (pin tries nm n)
              _  (note (+ "  glean " (+ (show nm) (+ " status=" (show st)))))
              (? (? (= st 0) 0 (? (id? (u-restart u) 'on-fail) (< n tries-cap) 0))
                   (: _ (note (+ "    restart " (show nm))) _ (rstart pids u)
                      (harvest units pids tries live))
                   (harvest units pids tries (- live 1))))))

   ; perceive: PARK on the sigfd, act on the signal, until all settle or a TERM.
   (watch sig units pids tries live)
     (? (< live 1) (note "  all services settled")
        (: _  (await sig)                                ; PARK -- cooperative; merges with the heartbeat
           ev (sigtake sig)
           (? (id? (cap ev) sig-term) (note "  SIGTERM: a queued EVENT, not a kill -> shut down")
              (id? (cap ev) sig-chld) (watch sig units pids tries (harvest units pids tries live))
              (watch sig units pids tries live))))

   (perceive _)
     (: sig (sigfd 0)
        (? !sig (note "  perceive: no signal source here")
           (: pids (tablet 0) tries (tablet 0)
              units [(unit 'slow 'service ["sh" "-c" "sleep 0.15; exit 0"] 'on-fail ())
                     (unit 'flap 'service ["sh" "-c" "exit 1"]             'on-fail ())]
              _ (note "-- PERCEIVE: park on a signalfd; a heartbeat task ticks alongside (the merge) --")
              hb (twirl (\ _ (heart 5 40)) 0)             ; heartbeat TASK: parks on the clock
              _ (each units (\ u (rstart pids u)))
              _ (watch sig units pids tries (tally units))
              _ (catch hb)                               ; join the heartbeat (the orphan law)
              0))))

; --- pid1 BRINGUP (Linux): mount the early filesystems + place a process in a cgroup --
; what a real pid1 does before spawning the console shell: mount proc/sysfs/devtmpfs,
; set up cgroup-v2, then (spawn (sh)). mounting needs privilege -- a real pid1 IS root,
; so it just calls (mount ...). here we DEMONSTRATE it unprivileged via (newns 0) (a
; private user+mount namespace, root-in-ns): tmpfs mounts cleanly; proc/sysfs additionally
; need a pid-ns, so they EPERM here but work as pid1. cgroup-v2 placement is no special
; nif -- mkdir the group, then write the pid to cgroup.procs (a plain file write via the
; `open` port), shown against the real tree (works under root / a delegated subtree).
(: (cg-write path s)                                     ; write s to a cgroup control file
     (: p (open path "w") (? (hot? p) (: _ (say p s) _ (flush p) (close p) 'ok) 'denied))
   (try-mount src tgt typ)
     (: _ (mkdir tgt 493)
        r (mount src tgt typ)                            ; () success, +errno failure
        (note (+ "  mount " (+ typ (+ " on " (+ tgt
                 (? r (+ " -> EPERM (errno " (+ (show r) ", needs root/pid-ns)")) " -> ok")))))))
   (boot-system _)
     (: e (newns 0)
        _ (note (? e "-- pid1 bringup: (no unpriv userns here; a real pid1 is root) --"
                     "-- pid1 bringup: private user+mount ns (root-in-ns) --"))
        _ (try-mount "tmpfs" "/tmp/love-run"  "tmpfs")       ; works unprivileged -> proves mount
        _ (try-mount "proc"  "/tmp/love-proc" "proc")        ; needs a pid-ns too -> EPERM here
        _ (note "-- cgroup-v2: mkdir a group, write a pid to cgroup.procs --")
        _ (: d "/sys/fs/cgroup/love.demo"
             _ (mkdir d 493)
             r (cg-write (+ d "/cgroup.procs") (+ (show (getpid 0)) "\n"))
             (note (+ "  place self in " (+ d (? (id? r 'ok) " -> ok" " -> needs root / a delegated subtree")))))
        0))

; --- BOOT AS PID 1 (a container) ------------------------------------------------
; the milestone the file is named for: love running as process 1. launch it in a
; private pid+user+mount namespace -- `make init-container`, or any runtime that
; makes the entrypoint pid 1 (docker/systemd-nspawn). then love IS init:
;   * getpid is 1.
;   * mount works -- we are root in a PID namespace, so proc mounts cleanly (the
;     EPERM in boot-system was the no-pid-ns case; here every mount is `ok`).
;   * love REAPS REPARENTED ORPHANS -- pid 1's defining duty. when a process's parent
;     dies the kernel reparents its children to pid 1, which must waitpid them or
;     they leak as zombies forever. (glean _) is waitpid(-1), so love harvests a pid it
;     NEVER SPAWNED. run NOT as pid 1, the orphan reparents elsewhere and never shows.
; (pid1 0) is the deterministic proof tour; (perceive 0) is the live supervisor
; (park on the signalfd, restart per policy, SIGTERM -> graceful shutdown).
(: (mount1 src tgt typ)                                  ; mount as pid1 (root in a pid-ns: it works)
     (: _ (mkdir tgt 493) r (mount src tgt typ)          ; () success, +errno failure
        (note (+ "  mount " (+ typ (+ " on " (+ tgt (? r (+ " -> errno " (show r)) " -> ok")))))))

   ; drain glean until `target` children are collected (or steps run out). classify
   ; each dead pid by `pids`: present -> a child WE started; absent -> a REPARENTED
   ; ORPHAN (only pid 1 sees those). a non-pair glean -- () none pending (now the real
   ; zero point) or -ECHILD none left -- just paces the loop.
   (glean-tour pids got target steps)
     (? (? (< steps 1) 1 (>= got target)) got
        (: ev (glean 0)
           (? (two? ev)
                (: pid (cap ev) st (cup ev) nm (peep pids pid ())
                   _ (note (+ (? nm (+ "  glean " (+ (show nm) " (a service we started)"))
                                     "  glean an ORPHAN (reparented to us -- pid 1's duty)")
                              (+ " pid=" (+ (show pid) (+ " status=" (show st))))))
                   (glean-tour pids (+ got 1) target (- steps 1)))
              (: _ (rest 5) (glean-tour pids got target (- steps 1))))))

   (pid1 _)
     (: me (getpid 0)
        _ (note (+ "-- love as PID " (+ (show me)
                  (? (= me 1) " -- a real init --" " (NOT pid 1: use `make init-container`) --"))))
        _ (note "-- mount the early filesystems (root in a pid-ns -> proc mounts) --")
        _ (mount1 "tmpfs" "/tmp/love-run"  "tmpfs")
        _ (mount1 "proc"  "/tmp/love-proc" "proc")
        pids (tablet 0)
        _ (note "-- spawn a child that orphans a grandchild, then glean (pid 1's duty) --")
        p (spawn ["sh" "-c" "(sleep 0.2; exit 7) & exit 0"])      ; parent exits 0; grandchild orphaned
        _ (pin pids p 'orphan-parent)
        n (glean-tour pids 0 2 800)                                ; parent settles fast; the orphan after its sleep
        _ (note (+ "-- reaped " (+ (show n) " children, one a reparented orphan -- love supervised pid 1 --")))
        0))

; --- the demos, as a CALLABLE function (not auto-run) ---
(: (demos _)
   (: cr (tablet 0)                                  ; per-name attempt counter for the flaky service
      ; a flaky service: crashes until its k-th start, then runs clean ('ok) -- models a
      ; process that exits nonzero until some dependency/race settles.
      svc    (\ name k (\ _ (: n (+ 1 (peep cr name 0)) _ (pin cr name n)
                               (? (< n k) (scare 'crash name) 'ok))))
      ok-svc (\ _ 'ok)                                ; a clean service: up on the first try
      units [
       (unit 'netd    'service ok-svc            'on-fail ())
       (unit 'logd    'service ok-svc            'on-fail ())
       (unit 'dbusd   'service ok-svc            'on-fail ['logd])              ; after logd
       (unit 'app     'service (svc 'app 3)      'on-fail ['netd 'dbusd])       ; crashes twice, then up
       (unit 'brittle 'service (svc 'brittle 9)  'no      ())                   ; crashes, Restart=no -> failed
       (unit 'hello   'oneshot ["echo" "-n" "init: a real child says hi"]
                                                 'no      ['app])]              ; a REAL forked child, after app
      _   (note "-- love-init: bring up the unit graph (deps first; services modeled, oneshot real) --")
      reg (boot units)
      _   (note "-- final state (the checkpoint -- a show/read-able value) --")
      _   (each (snap reg (map u-name units))
                (\ pr (note (+ "  " (+ (show (cap pr)) (+ " = " (show (caup pr))))))))
      _   (note "-- graceful shutdown (reverse start order) --")
      _   (shutdown reg (rev (map u-name units)))
      _   (note "")
      (real 0)))                                       ; ... and the same supervisor over REAL processes
