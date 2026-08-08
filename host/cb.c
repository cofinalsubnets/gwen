// host/cb.c -- quay's screen nifs, wired up for THIS seat and nothing more.
// The bodies are generic and live with the engine (crew/quay/nif.c over quay.c);
// what is host here is only the REGISTRATION -- the ai_nifs section glob is the
// host's trick, and another seat wires the same bodies its own way (the kernel a
// defs[] row, the playdate its own table). The quay sources ride along by unity
// include: they are not otherwise linked into the host binary.
#include "love.h"
#include "../crew/quay/quay.c"
#include "../crew/quay/nif.c"

AI_NIF("screen", nif_screen);
AI_NIF("scribe", nif_scribe);
AI_NIF("glass", nif_glass);
AI_NIF("gaze", nif_gaze);
AI_NIF("reply", nif_reply);
AI_NIF("unfold", nif_unfold);
AI_NIF("wet", nif_damage);
