gc={ 
    printf("Press a key:"); 
    vmLit(getchar());
}
f1={puts("F1 is called when '1' is pressed\n");}
f2={printf("The key %i was pressed\n", vmPop());}

@run
void main() {
    int c = (int) gc();
    if(c == 49) {
        (void) f1();
    } else {
        (void) f2(c);
    }
}
