// port/inle/uefi/loader.c -- our own BOOTX64.EFI: the bring-up a bootloader
// used to do, in ~250 lines of mooncc-compiled C. firmware hands us ms_abi
// UEFI; mkefi.l's thunks carry the seam (efi_main in, efi_call out, efi_go
// the cr3-and-jump tail). the loader reads love.elf off its own volume,
// copies the PT_LOADs anywhere 2M-aligned, finds `kboot` in the kernel's
// symtab (our binaries carry one on purpose) and fills it: the UEFI memmap's
// conventional ranges, the GOP framebuffer, the hhdm. then ExitBootServices,
// our page tables (identity + hhdm + KVMA over wherever the kernel landed),
// and kmain. the kernel notices nothing: kboot is kboot, the limine request
// section answers NULL, the same ELF boots all three doors.
//
// INTEGER-ONLY on purpose: the entry thunk saves rsi/rdi around the sysv
// call and nothing else -- the ms_abi xmm6..15 stay untouched only as long
// as no float sneaks in here.

typedef unsigned long long u64;
typedef unsigned int u32;
typedef unsigned short u16;
typedef unsigned char u8;

extern u64 efi_call(void *fn, u64 a, u64 b, u64 c, u64 d, u64 e);
extern void efi_go(u64 pml4, u64 entry);

#define KVMA 0xffffffff80000000ull
#define HHDM 0xffff800000000000ull

static u8 lip_guid[16] = {0xa1,0x31,0x1b,0x5b,0x62,0x95,0xd2,0x11,0x8e,0x3f,0x00,0xa0,0xc9,0x69,0x72,0x3b};
static u8 sfs_guid[16] = {0x22,0x5b,0x4e,0x96,0x59,0x64,0xd2,0x11,0x8e,0x39,0x00,0xa0,0xc9,0x69,0x72,0x3b};
static u8 nfo_guid[16] = {0x92,0x6e,0x57,0x09,0x3f,0x6d,0xd2,0x11,0x8e,0x39,0x00,0xa0,0xc9,0x69,0x72,0x3b};
static u8 gop_guid[16] = {0xde,0xa9,0x42,0x90,0xdc,0x23,0x38,0x4a,0x96,0xfb,0x7a,0xde,0xd0,0x80,0x51,0x6a};

static void **sys, **bs;               // SystemTable / BootServices, u64-slot views

static void say(char *s) {
 u16 w[128];
 int i = 0;
 for (; s[i] && i < 126; i++) w[i] = (u16) s[i];
 w[i] = 0;
 efi_call(((void **) sys[8])[1], (u64) sys[8], (u64) w, 0, 0, 0); }

static u64 die(char *s) { say("uefi: "); say(s); say("\r\n"); return 1; }

// the k_boot shape (port/inle/k.h) -- keep the two in step by hand: this file
// compiles freestanding, before out/lib exists.
#define ram_max 64
struct k_boot {
 u32 ram_n;
 struct { u64 base, len; } ram[ram_max];
 u64 hhdm;
 struct { u64 base; u16 w, h; u32 pitch_px; } fb;
 u8 has_fb; };

static u8 mmap[32768];                 // the UEFI memory map, GetMemoryMap-filled
// the page tables: pml4 + 2 pdpt + 4 pd + the kernel pd. a page table's low 12
// bits are its flags, so the aligned(4096) IS the contract -- mooncc honors it.
static u64 pt[8 * 512] __attribute__((aligned(4096)));

u64 efi_main(void *handle, void *st) {
 sys = (void **) st;
 bs = (void **) sys[12];

 // our volume: LoadedImage->DeviceHandle -> SimpleFileSystem -> the root dir
 void *lip = 0, *sfs = 0, *root = 0, *f = 0;
 if (efi_call(bs[19], (u64) handle, (u64) lip_guid, (u64) &lip, 0, 0))
  return die("no LoadedImage protocol");
 if (efi_call(bs[19], (u64) ((void **) lip)[3], (u64) sfs_guid, (u64) &sfs, 0, 0))
  return die("no filesystem on the boot volume");
 if (efi_call(((void **) sfs)[1], (u64) sfs, (u64) &root, 0, 0, 0))
  return die("cannot open the boot volume");
 static u16 name[9] = {'l','o','v','e','.','e','l','f',0};
 if (efi_call(((void **) root)[1], (u64) root, (u64) &f, (u64) name, 1, 0))
  return die("no love.elf beside BOOTX64.EFI");

 // size it (FILE_INFO.FileSize at +8), take pages, read it whole
 static u8 nfo[512];
 u64 nsz = sizeof nfo;
 if (efi_call(((void **) f)[8], (u64) f, (u64) nfo_guid, (u64) &nsz, (u64) nfo, 0))
  return die("GetInfo failed on love.elf");
 u64 fsz = *(u64 *) (nfo + 8), buf = 0;
 if (efi_call(bs[5], 0, 2, (fsz + 4095) / 4096, (u64) &buf, 0))
  return die("no pages for love.elf");
 u64 rsz = fsz;
 if (efi_call(((void **) f)[4], (u64) f, (u64) &rsz, buf, 0, 0) || rsz != fsz)
  return die("short read on love.elf");

 // the ELF: copy every PT_LOAD into one fresh 2M-aligned span; our tables
 // will lay KVMA over it, so the link-time paddrs only fix RELATIVE places.
 u8 *e = (u8 *) buf;
 if (*(u32 *) e != 0x464c457f) return die("love.elf is not an ELF");
 u64 phoff = *(u64 *) (e + 32), entry = *(u64 *) (e + 24);
 u16 phn = *(u16 *) (e + 56), phsz = *(u16 *) (e + 54);
 u64 lo = ~0ull, hi = 0;
 for (u16 i = 0; i < phn; i++) {
  u8 *p = e + phoff + (u64) i * phsz;
  if (*(u32 *) p != 1) continue;                       // PT_LOAD
  u64 pa = *(u64 *) (p + 24), msz = *(u64 *) (p + 40);
  if (pa < lo) lo = pa;
  if (pa + msz > hi) hi = pa + msz; }
 if (hi <= lo) return die("love.elf carries no load segments");
 // floor lo to 2M: the kernel high map is laid in 2M pages, and a PDE's
 // physical address has to be 2M-aligned (the bits below are RESERVED, and a
 // reserved bit set is a #PF at the first instruction fetch -- how this bug
 // announced itself). the lowest load address is only PAGE-aligned (0x201000
 // after the ELF headers), so the 2M floor below it becomes the anchor: every
 // segment copies at its offset from THERE, and the map lines up exactly.
 lo &= ~0x1fffffull;
 u64 span = 0, npg = (hi - lo + 4095) / 4096 + 512;    // + 2M of alignment slack
 if (efi_call(bs[5], 0, 2, npg, (u64) &span, 0))
  return die("no pages for the kernel");
 u64 kbase = (span + 0x1fffff) & ~0x1fffffull;         // 2M-align inside the span
 for (u16 i = 0; i < phn; i++) {
  u8 *p = e + phoff + (u64) i * phsz;
  if (*(u32 *) p != 1) continue;
  u64 off = *(u64 *) (p + 8), pa = *(u64 *) (p + 24);
  u64 flz = *(u64 *) (p + 32), msz = *(u64 *) (p + 40);
  u8 *d = (u8 *) (kbase + (pa - lo));
  for (u64 j = 0; j < flz; j++) d[j] = e[off + j];
  for (u64 j = flz; j < msz; j++) d[j] = 0; }

 // kboot's home: the kernel symtab (sh_type 2), symbol "kboot"
 u64 shoff = *(u64 *) (e + 40);
 u16 shn = *(u16 *) (e + 60), shsz = *(u16 *) (e + 58);
 struct k_boot *kb = 0;
 for (u16 i = 0; i < shn && !kb; i++) {
  u8 *sh = e + shoff + (u64) i * shsz;
  if (*(u32 *) (sh + 4) != 2) continue;                // SHT_SYMTAB
  u8 *lnk = e + shoff + (u64) (*(u32 *) (sh + 40)) * shsz;
  u8 *str = e + *(u64 *) (lnk + 24);
  u64 so = *(u64 *) (sh + 24), sn = *(u64 *) (sh + 32) / 24;
  for (u64 j = 0; j < sn; j++) {
   u8 *sy = e + so + j * 24;
   char *nm = (char *) (str + *(u32 *) sy);
   if (nm[0] == 'k' && nm[1] == 'b' && nm[2] == 'o' && nm[3] == 'o'
       && nm[4] == 't' && !nm[5]) {
    kb = (struct k_boot *) (kbase + (*(u64 *) (sy + 8) - KVMA - lo));
    break; } } }
 if (!kb) return die("no kboot symbol in love.elf");
 kb->hhdm = HHDM;

 // the framebuffer, when GOP has one (the interactive door's console)
 void *gop = 0;
 if (!efi_call(bs[40], (u64) gop_guid, 0, (u64) &gop, 0, 0) && gop) {
  u8 *m = (u8 *) ((void **) gop)[3];
  u8 *info = (u8 *) *(u64 *) (m + 8);
  kb->fb.base = *(u64 *) (m + 24);
  kb->fb.w = (u16) *(u32 *) (info + 4);
  kb->fb.h = (u16) *(u32 *) (info + 8);
  kb->fb.pitch_px = *(u32 *) (info + 32);
  kb->has_fb = 1; }

 // page tables BEFORE ExitBootServices (say still works): identity + hhdm
 // over the low 4G in 2M pages, and KVMA -> the span the kernel landed in.
 for (u64 i = 0; i < 8 * 512; i++) pt[i] = 0;
 for (u64 i = 0; i < 2048; i++) pt[3 * 512 + i] = (i << 21) | 0x83;
 for (u64 i = 0; i < 4; i++) pt[512 + i] = (u64) &pt[(3 + i) * 512] | 3;
 // KVMA + V maps to kbase + (V - lo): the segments were copied at their
 // offsets FROM lo, and lo (the lowest link paddr, 2M-aligned by the script)
 // is where the kernel believes KVMA's image begins. slots below lo stay
 // unmapped -- nothing of ours lives there.
 u64 *kpd = pt + 7 * 512;              // the slack page after the seven above
 for (u64 i = lo >> 21; i < 512; i++) kpd[i] = (kbase + (i << 21) - lo) | 0x83;
 pt[2 * 512 + 510] = (u64) kpd | 3;
 pt[0] = (u64) &pt[512] | 3;
 // NX on the hhdm entry alone: one bit at the top of the walk covers every page
 // under it, and the hhdm is how the kernel reaches all of ram. the identity
 // window keeps X -- efi_go's own next instruction fetch is there.
 pt[256] = (u64) &pt[512] | 3 | (1ull << 63);
 pt[511] = (u64) &pt[2 * 512] | 3;

 // the memmap -> kboot.ram: CONVENTIONAL (7) only. loader/firmware-typed
 // memory stays out, so the kernel heap never eats this stack, the tables,
 // or the kernel span (LoaderData). then the ExitBootServices dance.
 u64 msz = sizeof mmap, key = 0, dsz = 0, dvr = 0;
 if (efi_call(bs[7], (u64) &msz, (u64) mmap, (u64) &key, (u64) &dsz, (u64) &dvr))
  return die("GetMemoryMap failed");
 if (efi_call(bs[29], (u64) handle, key, 0, 0, 0)) {
  msz = sizeof mmap;
  if (efi_call(bs[7], (u64) &msz, (u64) mmap, (u64) &key, (u64) &dsz, (u64) &dvr)
      || efi_call(bs[29], (u64) handle, key, 0, 0, 0))
   return die("ExitBootServices refused twice"); }
 for (u64 o = 0; o + dsz <= msz; o += dsz) {
  u8 *d = mmap + o;
  if (*(u32 *) d != 7) continue;
  u64 pa = *(u64 *) (d + 8), np = *(u64 *) (d + 24);
  if (pa < 0x100000) continue;                         // low memory stays the firmware's
  if (pa >= 0x100000000ull) continue;                  // above the mapped 4G
  if (kb->ram_n >= ram_max) break;
  kb->ram[kb->ram_n].base = pa;
  kb->ram[kb->ram_n].len = np * 4096;
  kb->ram_n++; }

 efi_go((u64) pt, entry);
 return 1; }
