; svalbard, assembled: the u-floor it rides (kore text + diff, self-registering
; module 'kore), then the merge engine, the http client and the verbs -- each a
; (module 'sb form, so the parts register the one module and the loader keeps
; the file-made registration. this file loads the lot; the standalone `sb`
; script is the same files catted into a session instead, the wraps registering
; the same modules there.
(use "crew/kore/text.l")
(use "crew/kore/diff.l")
(use "crew/sb/merge.l")
(use "crew/sb/http.l")
(use "crew/sb/sb.l")
