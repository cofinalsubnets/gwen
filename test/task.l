; tests for cooperative multitasking: hatch / yield / wait / sleep / done?.
; (twirl fn x)  -> pid (positive integer); starts a new task running (fn x).
;                  the main task has reserved pid 0.
; (rest 0)     -> nil; cooperatively suspends so other ready tasks can run.
;                  (yield) is just `yield` since (x)=x in l — call with an arg.
; (catch pid)    -> if pid is unknown / non-numeric / refers to the caller, 0.
;                  if the task is dormant (its fn has returned), collect it
;                  from the ring and return its return value.
;                  if the task is still running, yield until it becomes dormant.
; (rest n)     -> nil; blocks the current task for at least n g_clock ticks.
;                  n <= 0 returns immediately. Other tasks run during the wait.
; (landed? pid) -> 1 if (catch pid) would not yield (pid unknown, self, dormant),
;                  nil if pid refers to a runnable task.
; (scoop _)    -> (pid . retval) of one task that has finished, or () when none
;                  have. no pid needed and it never yields; the pair carries the
;                  presence, since a retval of () is a perfectly ordinary answer.

(assert
 ; hatch-wait round trip: fn is applied to x; the result flows back through wait.
 (= 42 (: p (twirl (\ x x) 42) (catch p)))
 (= 43 (: p (twirl (\ x (+ x 1)) 42) (catch p)))
 (= 49 (: p (twirl (\ x (* x x)) 7) (catch p)))

 ; hatch returns a positive integer pid (main reserves pid 0).
 (: p (twirl (\ _ 0) 0) _ (catch p) (&& (charm? p) (> p 0)))

 ; pids are monotonically increasing within a VM.
 (: a (twirl (\ x x) 0)
    b (twirl (\ x x) 0)
    _ (catch a) _ (catch b)
    (> b a))

 ; wait on an unknown pid returns 0 without blocking.
 (= 0 (catch 99999))
 ; non-numeric pid argument: also 0.
 (= 0 (catch ()))
 ; pid 0 (main) is treated as not-waitable from main itself.
 (= 0 (catch 0))

 ; collecting a dormant task removes it: a second wait on the same pid sees 0.
 (: p (twirl (\ x x) 99)
    (&& (= 99 (catch p)) (= 0 (catch p))))

 ; many tasks can be reaped in any order.
 (: a (twirl (\ x (* x 10)) 3)
    b (twirl (\ x (* x 10)) 4)
    c (twirl (\ x (* x 10)) 5)
    (&& (= 50 (catch c))
        (= 30 (catch a))
        (= 40 (catch b))))

 ; a task may yield mid-execution and still return its value through wait.
 (: p (twirl (\ x (do (rest 0) (rest 0) (rest 0) (+ x 1))) 41)
    (= 42 (catch p)))

 ; nested hatch: a worker can hatch and wait on its own subtask.
 (: p (twirl (\ _ (: q (twirl (\ x (+ x 1)) 10) (catch q))) 0)
    (= 11 (catch p)))

 ; tasks share the heap: a captured hash sees writes from the worker.
 (: t (tablet 0)
    p (twirl (\ k (pin t k 'done)) 'key)
    _ (catch p)
    (= 'done (peep t 'key 0)))

 ; waiting on a still-running task blocks until it finishes.
 ; here the worker only completes after several yields; main's wait yields
 ; alongside it and eventually returns the result.
 (: t (tablet 0)
    _ (pin t 'n 0)
    p (twirl (\ _ (: (loop _) (? (< (peep t 'n 0) 3)
                                (do (pin t 'n (+ 1 (peep t 'n 0)))
                                   (rest 0)
                                   (loop 0))
                                'done)
                    (loop 0))) 0)
    r (catch p)
    (&& (= 'done r) (= 3 (peep t 'n 0))))

 ; --- landed? predicate ---

 ; landed? on an unknown pid is true (catch would return 0 immediately).
 (= 1 (landed? 99999))
 ; landed? on a non-numeric pid is true.
 (= 1 (landed? ()))
 ; landed? on the caller's own pid (main = 0) is true — wait skips self.
 (= 1 (landed? 0))

 ; landed? on a freshly spawned task is false: the task hasn't completed.
 (: p (twirl (\ _ 42) 0) r (landed? p) _ (catch p) (nil? r))

 ; after yielding, the worker completes and landed? becomes true.
 (: p (twirl (\ _ 42) 0) _ (rest 0) r (landed? p) _ (catch p) (= 1 r))

 ; after wait collects the task, landed? is again true (pid is gone).
 (: p (twirl (\ _ 42) 0) _ (catch p) (= 1 (landed? p)))

 ; --- scoop: harvest one finished task, whichever it turns out to be ---
 ; `catch` names a pid and blocks until that one is done; `scoop` names none and
 ; never waits. it answers (pid . retval) for one task that has finished, or ()
 ; when none have -- the task-side twin of `glean`, which does this for a finished
 ; child process. a server twirling a task per client wants exactly this: without
 ; it, the only way to collect is to keep a roster of live pids and ask about each
 ; one, and since every ask is itself a ring search the two costs multiply.
 ; every law here drains first: the corpus runs concatenated in one scope, so a
 ; task some earlier section left dormant is a task scoop would rightly hand back.

 ; an empty ring answers the zero point.
 (: (drain _) (? (two? (scoop 0)) (drain ()) 0)
    _ (drain ())
    (nil? (scoop 0)))

 ; a finished task comes back whole: the pid in the car, its value in the cdr.
 (: (drain _) (? (two? (scoop 0)) (drain ()) 0)
    _ (drain ())
    p (twirl (\ x (x + 1)) 41)
    _ (rest 0)
    s (scoop 0)
    (&& (= p (cap s)) (= 42 (cup s))))

 ; presence rides the pair, never the net. a task run for its effect answers ()
 ; -- most of them do -- and it is still handed back as a pair. a drain that tested
 ; the value rather than the shape would stop dead on the first such task and leave
 ; the ring full while reporting it empty.
 (: (drain _) (? (two? (scoop 0)) (drain ()) 0)
    _ (drain ())
    p (twirl (\ _ ()) ())
    _ (rest 0)
    s (scoop 0)
    (&& (two? s) (&& (= p (cap s)) (nil? (cup s)))))

 ; scooping collects, exactly as catching does: the pid is gone afterwards.
 (: (drain _) (? (two? (scoop 0)) (drain ()) 0)
    _ (drain ())
    p (twirl (\ _ 42) ())
    _ (rest 0)
    _ (scoop 0)
    (= 0 (catch p)))

 ; a task still running is not harvested -- unfinished is the whole condition.
 (: (drain _) (? (two? (scoop 0)) (drain ()) 0)
    _ (drain ())
    p (twirl (\ _ (: _ (rest 0) 42)) ())
    s (scoop 0)
    _ (catch p)
    (nil? s))

 ; three finished tasks drain in three scoops, and the fourth answers nothing.
 (: (drain _) (? (two? (scoop 0)) (drain ()) 0)
    (count k) (? (two? (scoop 0)) (count (k + 1)) k)
    _ (drain ())
    _ (twirl (\ _ 1) ()) _ (twirl (\ _ 2) ()) _ (twirl (\ _ 3) ())
    _ (rest 0)
    (= 3 (count 0)))

 ; --- sleep ---

 ; sleep with non-positive n returns nil immediately without yielding.
 (nil? (rest 0))
 (nil? (rest -1))
 (nil? (rest ()))

 ; sleep takes at least n ticks (g_clock ms on this frontend).
 ; allow generous slack for slow ci.
 (: t0 (clock 0)
    _ (rest 30)
    elapsed (clock t0)
    (>= elapsed 30))

 ; sleeping tasks run concurrently: two parallel 30ms sleeps finish in well under 60ms.
 (: t0 (clock 0)
    a (twirl (\ _ (rest 30)) 0)
    b (twirl (\ _ (rest 30)) 0)
    _ (catch a) _ (catch b)
    elapsed (clock t0)
    (&& (>= elapsed 30) (< elapsed 60)))

 ; a peer waking mid-wait must not cut short main's longer sleep.
 ; (rest 0) forces peer to reach its (rest 30) before main starts its (rest 80),
 ; so main's yield_sw enters the deadline-coalesce wait with peer's 30 as min_wake.
 ; When peer wakes at ~30, main's pending wake_at must be preserved across the snapshot.
 (: t0 (clock 0)
    p (twirl (\ _ (rest 30)) 0)
    _ (rest 0)
    _ (rest 80)
    _ (catch p)
    elapsed (clock t0)
    (&& (>= elapsed 80) (< elapsed 120)))

 ; sleep cooperates: while one task sleeps, another finishes its work.
 (: t (tablet 0)
    _ (pin t 'flag 0)
    a (twirl (\ _ (rest 30)) 0)
    b (twirl (\ _ (pin t 'flag 1)) 0)
    _ (catch a) _ (catch b)
    (= 1 (peep t 'flag 0)))

 ; --- chill ---

 ; chill on an unknown pid: 0, no-op.
 (= 0 (freeze 99999))
 ; chill on a non-numeric pid: 0.
 (= 0 (freeze ()))
 ; chill on self (main = 0): 0 (self-chill impossible by construction).
 (= 0 (freeze 0))

 ; chill a running task: returns 1. The task is gone from the ring, so
 ; wait on its pid returns 0 (unknown).
 (: t (tablet 0)
    _ (pin t 'flag 0)
    p (twirl (\ _ (: (loop _) (do (rest 0) (pin t 'flag 1) (loop 0)) (loop 0))) 0)
    r (freeze p)
    (&& (= 1 r) (= 0 (catch p))))

 ; chill a dormant task (already finished): also returns 1, then wait sees 0.
 (: p (twirl (\ _ 42) 0)
    _ (rest 0)
    ; p is dormant now (landed? would be 1).
    (&& (= 1 (freeze p)) (= 0 (catch p))))

 ; double-chill: second chill is a no-op since the pid is gone from the ring.
 (: p (twirl (\ _ (: (loop _) (do (rest 0) (loop 0)) (loop 0))) 0)
    (&& (= 1 (freeze p)) (= 0 (freeze p))))

 ; killing one of several tasks leaves the others intact.
 (: t (tablet 0)
    a (twirl (\ _ (: (loop _) (do (rest 0) (loop 0)) (loop 0))) 0)
    b (twirl (\ x (* x 10)) 7)
    _ (freeze a)
    rb (catch b)
    (&& (= 0 (catch a)) (= 70 rb)))

 ; --- yieldable bignum multiply ---

 ; a big bignum multiply can be long-running yet yields cooperatively: it does a
 ; bounded slice of work per VM dispatch (a yield check per Karatsuba job for an
 ; equal-length product, per schoolbook chunk otherwise), so a peer task runs
 ; *during* the product. This is what lets the repl's Ctrl-C poller interrupt a
 ; hung eval like (3 3 3 3) (see g/repl.l do_eval). Here a worker squares a ~200k-bit
 ; number once; a ticker peer counts how often it gets scheduled mid-multiply.
 ; Before chunking, the worker held the CPU for the whole product and the ticker
 ; (capped so it can't wrap the 1-byte cask) never ran -- now it runs many times.
 (: big (200000 2)
    ticks (cask 1)
    w (twirl (\ _ (* big big)) 0)
    t (twirl (\ _ (: (go _) (? (landed? w) 0
                               (: c (peep ticks 0 0)
                                  _ (? (< c 250) (pin ticks 0 (+ c 1)) 0)
                                  _ (rest 0) (go 0)))
                     (go 0))) 0)
    _ (catch w) _ (catch t)
    (< 5 (peep ticks 0 0)))

 ; a peer can chill a task while it is mid-multiply, not only once it finishes:
 ; the worker parks at a yield inside the product, so chill finds it runnable.
 (: w (twirl (\ _ (* (200000 2) (200000 2))) 0)
    _ (rest 0)
    (&& (= 1 (freeze w)) (= 0 (catch w))))

 ; --- wake_at scheduler behavior ---

 ; wait on a sleeping task: main has to sleep deeply alongside it, then
 ; collect the result. Elapsed should be ~30ms, not 0 or much more.
 (: t0 (clock 0)
    p (twirl (\ _ (do (rest 30) 'done)) 0)
    r (catch p)
    elapsed (clock t0)
    (&& (= 'done r) (>= elapsed 30) (< elapsed 80)))

 ; tasks with different sleep durations wake in order: shorter sleep
 ; finishes first, longer still sleeping when we check, then completes.
 (: a (twirl (\ _ (do (rest 10) 1)) 0)
    b (twirl (\ _ (do (rest 80) 2)) 0)
    ra (catch a)               ; ~10ms; b still sleeping
    db (landed? b)              ; b should still be running
    rb (catch b)               ; ~70ms more
    (&& (= 1 ra) (nil? db) (= 2 rb)))

 ; chill a sleeping task: scheduler skips it via wake_at, but chill walks the
 ; ring directly and finds it regardless of state.
 (: p (twirl (\ _ (do (rest 10000) 'never)) 0)
    r (freeze p)
    (&& (= 1 r) (= 0 (catch p))))

 ; sleep tasks coexist with non-sleeping ones: a worker that yields ap
 ; gets time while another sleeps.
 (: t (tablet 0)
    _ (pin t 'n 0)
    a (twirl (\ _ (rest 30)) 0)
    b (twirl (\ _ (: (loop _) (? (< (peep t 'n 0) 5)
                                (do (pin t 'n (+ 1 (peep t 'n 0)))
                                   (rest 0)
                                   (loop 0))
                                'done)
                    (loop 0))) 0)
    _ (catch a) _ (catch b)
    (= 5 (peep t 'n 0)))

 ; --- what a task wears: in/out/err are names, and each task seats its own ---
 ; (wear (i o e)) re-seats them for the running task, (wear ()) hands the console
 ; back, (worn ()) answers what is worn. an element that is not a port keeps the
 ; console, so a lone stdout re-seat says (() o ()).

 ; a worn jug takes what the task says, and the console comes back after
 (: j (jug 0)
    _ (wear [() j ()])
    _ (puts "into the jug")
    _ (wear ())
    (&& ("into the jug" = (slurp j)) (nil? (worn ()))))

 ; a worn tap is what the task reads
 (: _ (wear [(tap '(104 105 33)) () ()])
    s (slurp in)
    _ (wear ())
    ("hi!" = s))

 ; per task, across a switch: two tasks wearing different jugs never cross, and
 ; the main task keeps the console through both
 (: a (jug 0) b (jug 0)
    p (twirl (\ _ (: _ (wear [() a ()]) _ (puts "a1") _ (rest 0) _ (puts "a2") 1)) 0)
    q (twirl (\ _ (: _ (wear [() b ()]) _ (puts "b1") _ (rest 0) _ (puts "b2") 2)) 0)
    _ (catch p) _ (catch q)
    (&& ("a1a2" = (slurp a)) ("b1b2" = (slurp b)) (nil? (worn ()))))

 ; inherited at spawn: a child says its piece into the jug its parent wears
 (: j (jug 0)
    _ (wear [() j ()])
    p (twirl (\ _ (: _ (puts "the child's") 0)) 0)
    _ (catch p)
    _ (wear ())
    ("the child's" = (slurp j)))

 ; a child that wears its own leaves the parent's alone
 (: a (jug 0) b (jug 0)
    _ (wear [() a ()])
    p (twirl (\ _ (: _ (wear [() b ()]) _ (puts "child") 0)) 0)
    _ (catch p)
    _ (puts "parent")
    _ (wear ())
    (&& ("parent" = (slurp a)) ("child" = (slurp b))))
)
