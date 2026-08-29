/* per-TB guest insn counts, dumped as "pc n_insns exec_sum" lines at exit.
   inline adds into chunked scoreboards keep translation-speed overhead only. */
#include <qemu-plugin.h>
#include <glib.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
QEMU_PLUGIN_EXPORT int qemu_plugin_version = QEMU_PLUGIN_VERSION;

#define CHUNK 65536
typedef struct { uint64_t pc; uint32_t n; } TbRec;
static GPtrArray *chunks;   /* of struct qemu_plugin_scoreboard* */
static GArray *recs;        /* of TbRec, index = global slot */
static GMutex lock;
static char outpath[4096];

static void tb_trans(struct qemu_plugin_tb *tb, void *udata) {
    g_mutex_lock(&lock);
    size_t slot = recs->len;
    if (slot / CHUNK >= chunks->len)
        g_ptr_array_add(chunks, qemu_plugin_scoreboard_new(CHUNK * sizeof(uint64_t)));
    TbRec r = { qemu_plugin_tb_vaddr(tb), (uint32_t) qemu_plugin_tb_n_insns(tb) };
    g_array_append_val(recs, r);
    struct qemu_plugin_scoreboard *sb = g_ptr_array_index(chunks, slot / CHUNK);
    g_mutex_unlock(&lock);
    qemu_plugin_u64 ent = { sb, (slot % CHUNK) * sizeof(uint64_t) };
    qemu_plugin_register_vcpu_tb_exec_inline_per_vcpu(
        tb, QEMU_PLUGIN_INLINE_ADD_U64, ent, r.n);
}

static void at_exit(void *p) {
    FILE *f = fopen(outpath, "w");
    if (!f) { perror("insnpc out"); return; }
    fprintf(f, "# entry %" PRIx64 " start %" PRIx64 "\n",
            qemu_plugin_entry_code(), qemu_plugin_start_code());
    uint64_t total = 0;
    for (size_t i = 0; i < recs->len; i++) {
        TbRec *r = &g_array_index(recs, TbRec, i);
        struct qemu_plugin_scoreboard *sb = g_ptr_array_index(chunks, i / CHUNK);
        qemu_plugin_u64 ent = { sb, (i % CHUNK) * sizeof(uint64_t) };
        uint64_t c = qemu_plugin_u64_sum(ent);
        total += c * r->n;
        if (c) fprintf(f, "%" PRIx64 " %u %" PRIu64 "\n", r->pc, r->n, c);
    }
    fprintf(f, "# GUEST_INSNS %" PRIu64 "\n", total);
    fclose(f);
    fprintf(stderr, "GUEST_INSNS %" PRIu64 "\n", total);
}

QEMU_PLUGIN_EXPORT int qemu_plugin_install(qemu_plugin_id_t id,
        const qemu_info_t *info, int argc, char **argv) {
    snprintf(outpath, sizeof outpath, "insnpc.out");
    for (int i = 0; i < argc; i++)
        if (g_str_has_prefix(argv[i], "out="))
            snprintf(outpath, sizeof outpath, "%s", argv[i] + 4);
    chunks = g_ptr_array_new();
    recs = g_array_new(FALSE, FALSE, sizeof(TbRec));
    qemu_plugin_register_vcpu_tb_trans_cb(id, tb_trans, NULL);
    qemu_plugin_register_atexit_cb(id, at_exit, NULL);
    return 0;
}
