/* v6-M varargs, gcc side (-O2 -mcpu=cortex-m0): the AAPCS32 base-ABI word
   walk crosses the gcc<->mooncc boundary in both directions. */
int vsum(int, ...); int vnth(int, ...); int vcall(void);
int ovnamed(int, int, int, int, int, ...);

int run(void){
 int ok = 0;
#define CK(x) do{ ok++; if(!(x)) return 100+ok; }while(0)
 CK(vsum(0) == 0);
 CK(vsum(1, 42) == 42);
 CK(vsum(6, 1,2,3,4,5,6) == 21);        /* anonymous words past r0-r3 */
 CK(vnth(0, 11, 22, 33) == 11);
 CK(vnth(2, 11, 22, 33) == 33);
 CK(vcall() == 2100 + 9);
 CK(ovnamed(1,2,3,4,5, 6, 7) == 1+20+300+4000+50000+600000+7000000);
 return 7;
}
