
extern fn float __ADD__F_F(float, float) {{ vmLitFloat(vm, vmPopFloat(vm) + vmPopFloat(vm)); }}
extern fn float __SUB__F_F(float, float) {{ float sub = vmPopFloat(vm); vmLitFloat(vm,  vmPopFloat(vm) - sub); }}
extern fn float __SUB__F(float) {{ vmLitFloat(vm, - vmPopFloat(vm)); }}
extern fn float __MUL__F_F(float, float) {{ vmLitFloat(vm, vmPopFloat(vm) * vmPopFloat(vm)); }}
extern fn float __DIV__F_F(float, float) {{ float den = vmPopFloat(vm); vmLitFloat(vm, vmPopFloat(vm) / den); }}

extern fn float sqrt(float) {{ vmLitFloat(vm, sqrt(vmPopFloat(vm)));}}

extern fn void print(float) {{ printf("%f\n", vmPopFloat(vm));}}

@run
float gethypo(float a, float b) {
	return sqrt( (a*a) + (b*b) );
}
void main() {
    print(gethypo(3.0, 4.0));
    print(5.0 / 2.0);
    print(100.0 - 20.0);
    return;
}
