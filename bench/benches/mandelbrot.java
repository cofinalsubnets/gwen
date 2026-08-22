// mandelbrot escape counts over a 128x128 grid of c in [-2,1]x[-1.5,1.5] (IEEE
// double). Same op order as the reference (zr*zr - zi*zi + cr ; 2.0*zr*zi + ci),
// the grid step 3.0/W computed before the multiply.
class Main {
    static final int MAXIT = 128, W = 128, H = 128;

    static long pix(double cr, double ci) {
        double zr = 0.0, zi = 0.0;
        int it = 0;
        while (it < MAXIT && zr * zr + zi * zi <= 4.0) {
            double nzr = zr * zr - zi * zi + cr;
            zi = 2.0 * zr * zi + ci;
            zr = nzr;
            it += 1;
        }
        return it;
    }

    static long work() {
        long s = 0;
        for (int py = 0; py < H; py++) {
            double ci = -1.5 + py * (3.0 / H);
            for (int px = 0; px < W; px++) {
                double cr = -2.0 + px * (3.0 / W);
                s += pix(cr, ci);
            }
        }
        return s;
    }

    public static void main(String[] a) {
        Bench.bench("mandelbrot", Main::work);
    }
}
