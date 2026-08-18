; crew/kore/asbook.l -- make sure the assembler module is REGISTERED for the cat:
; the wrapped crew/holo files (backends, elf/obj/link, copy) REOPEN module 'holo,
; and holo's readers say (use 'holo) scoped or probe (from 'holo ..) -- nothing
; reads it off an ambient splice anymore. the load rides an anonymous module
; layer -- registered nowhere, so use's splice is popped and nothing lands in
; the cat's own walk -- enter/leave are mopped, module carries them as values,
; so this is the door that works off the book. kore's cat
; order: .. asbook.l, crew/holo/elf.l, ..; mooncc's: this, then the cross
; backends join the module (defbackend mutates its table), then elf/obj/link.
(module () (use 'holo))
