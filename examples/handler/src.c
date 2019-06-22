extern fn int gc() {{ 
    printf("Press a key:"); 
    vmLit(vm, getchar());
}}
extern fn void f1() {{puts("F1 is called when '1' is pressed\n");}}
extern fn void f2(int) {{printf("The key %i was pressed\n", vmPop(vm));}}

@run
void main() {
    int c = gc();
    if(c == 49) {
        f1();
    } else {
        f2(c);
    }
}
