
extern fn struct { int x; int y; int z; } get_vector() {{
        vmLit(vm, 102); // z
        vmLit(vm, 203); // y
        vmLit(vm, 144); // x
    }}
extern fn void print(int) {{ printf("%i\n", vmPop(vm)); }}
extern fn void print32(int32) {{ printf("%i\n", vmPop32(vm)); }}

extern struct {
    int32 a;
    int32 b;
} vec2;

@run
void main() {
    // Define structure
    struct {int x; int y; int z; } v;
    v = get_vector();
    print(v.x);
    print(v.y);
    print(v.z);
    v.z = 133;
    print(v.x);
    print(v.y);
    print(v.z);

    print32(vec2.a);
    print32(vec2.b);

    int32 temp = vec2.a;
    vec2.a = vec2.b;
    vec2.b = temp;
    return;
}
