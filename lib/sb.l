; svalbard, assembled: the u-floor it rides (kore text + diff), then the merge
; engine, the http client and the verbs -- all INCLUDED into this one layer
; (a slashed use evals into the current head), so (use 'sb) loads the lot
; and leave registers it whole. the standalone `sb` script is the same
; files catted into a session instead, nothing sealed, its seat reading bare.
(use 'dns)          ; http-get dials a NAME, and connect takes only numbers
(use 'sb/text)
(use 'sb/diff)
(use 'sb/merge)
(use 'sb/http)
(use 'sb/core)
