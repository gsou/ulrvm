
extern fn float __ADD__F_F(float, float) {{ vmLitFloat(vm, vmPopFloat(vm) + vmPopFloat(vm)); }}
extern fn float __SUB__F_F(float, float) {{ float sub = vmPopFloat(vm); vmLitFloat(vm,  vmPopFloat(vm) - sub); }}
extern fn float __SUB__F(float) {{ vmLitFloat(vm, - vmPopFloat(vm)); }}
extern fn float __MUL__F_F(float, float) {{ vmLitFloat(vm, vmPopFloat(vm) * vmPopFloat(vm)); }}
extern fn float __DIV__F_F(float, float) {{ float den = vmPopFloat(vm); vmLitFloat(vm, vmPopFloat(vm) / den); }}

extern fn float sqrt(float) {{ vmLitFloat(vm, sqrt(vmPopFloat(vm)));}}

extern fn void print(float) {{ printf("%f\n", vmPopFloat(vm));}}

@run
void main() {

    float a = 3.0;
    float b = 4.0;

    float c = sqrt( (a * a) + (b * b) );

    print(c);
    print(5.0 / 2.0);
    print(100.0 - 20.0);


    return;
}
