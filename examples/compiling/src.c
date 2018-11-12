@fibonacci
int main(int n) {
    n = n - 3;
    int a = 1;
    int b = 2;
    int tmp = 0;
    do {
        tmp = a;
        a = b;
        b = tmp + b;
        n = n - 1;
    } while(n > 0);
    return b;
}
