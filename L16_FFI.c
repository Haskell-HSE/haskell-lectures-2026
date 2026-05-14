// quick & dirty (and, of course, incorrect) impl
double dot_product(double* a, double* b, int n) {
    double p = 0;
    for (int i = 0; i != n; ++i) p += a[i] * b[i];
    return p;
}
