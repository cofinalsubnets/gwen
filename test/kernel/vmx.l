; --- test/kernel/vmx.l: the Intel twin of svm.l -- one guest, one
; exit, the other vendor. Guarded the same two ways: the nom is in the book only
; on x86_64, so presence is asked of (names ()); and the silicon under that seat
; may be amd's, so (vmx ()) answers for the CPU.
;
; The laws separate the same three successes svm.l does, plus one this vendor
; needs: a refused entry does not fault, it falls through with a number, so the
; fourth field is the VM-instruction error and it must be 0. When it is not, it
; is the only thing the machine has to say about why -- which is why the gate
; carries it rather than folding it into a yes or no.
;
; And since the guest runs under ept, the same laws now prove one thing more.
; The guest believes its code is at guest-physical 0; it is not, and host
; physical 0 holds the machine's own low memory. So a sentinel of 0x1234 coming
; back could only have been executed out of the page the ept pointed at -- an
; untranslated fetch would have run whatever lies at the bottom of RAM.
(: had (member? 'vmx (names ()))
   (? (! (? had (vmx ()) ()))
      (: _ (puts "vmx: no VT-x here, laws skipped") _ (putc 10) ())
      (: r (vmx-run ())
         _ (assert
            (4 = (tally r))          ; (reason rax rip err) -- a refusal answers ()
            (0 = <>>>r)              ; the VM-instruction error: the entry was taken
            (10 = <r)                ; exit_reason_cpuid
            (4660 = <>r)             ; 0x1234, the sentinel the guest loaded
            (5 = <>>r))              ; the cpuid's own address, one instruction in
         _ (puts "vmx: guest ran, cpuid exited") _ (putc 10)
         ())))
