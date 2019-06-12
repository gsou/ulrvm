extern fn void gc() {{ 
    printf("Press a key:"); 
    vmLit(getchar());
}}
extern fn void f1() {{puts("F1 is called when '1' is pressed\n");}}
extern fn void f2() {{printf("The key %i was pressed\n", vmPop());}}

@run
void main() {
    int c = (int) gc();
    if(c == 49) {
        (void) f1();
    } else {
        (void) f2(c);
    }
}
