// java benchmark harness -- mirrors bench/bench.l and the py/js/jl harnesses.
// bench(name, work) auto-scales the repetition count (doubling until one timed
// batch clears MIN_MS), then prints one line matching the other harnesses:
//     <name> <lang> <reps> <ms> <checksum>
// work is a nullary LongSupplier returning a deterministic checksum. BENCH_LANG
// sets the label, default "java". Locale.ROOT keeps the decimal separator a dot.
import java.util.Locale;
import java.util.function.LongSupplier;

public class Bench {
    static final double MIN_MS = 200.0;
    static final String LANG;
    static {
        String l = System.getenv("BENCH_LANG");
        LANG = (l != null) ? l : "java";
    }

    public static void bench(String name, LongSupplier work) {
        long reps = 1;
        for (;;) {
            long t0 = System.nanoTime();
            long chk = 0;
            for (long i = 0; i < reps; i++) chk = work.getAsLong();
            double ms = (System.nanoTime() - t0) / 1e6;
            if (ms >= MIN_MS) {
                System.out.printf(Locale.ROOT, "%s %s %d %.3f %d%n",
                        name, LANG, reps, ms, chk);
                break;
            }
            reps *= 2;
        }
    }
}
