// We need a dynamic source to store variables
@run
void main() {
    @{ &gc nat dup }@ //Call getchar
    if ((int) @{ 49 eq }@) {
        @{ &f1 }@
    } else {
        @{ &f2 }@
    }
    @{nat end }@
    // Functions take two CELLS to store
    @{ :gc %0 %0 %0 %0 }@
    @{ :f1 %0 %0 %0 %0 }@
    @{ :f2 %0 %0 %0 %0 }@

}
