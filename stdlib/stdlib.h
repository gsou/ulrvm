
extern fn int abs (int) {{ CELL c = vmPop(); vmLit(c > 0 ? c: -c);  }}
extern fn int32 labs (int32) {{ int32_t c = vmPop32(); vmLit32(c > 0 ? c: -c);  }}

extern fn int32 add32(int32, int32) {{int32_t num1 = vmPop32(); int32_t num2 = vmPop32(); vmLit32(num1+num2);}}
extern fn int32 sub32(int32, int32) {{int32_t num1 = vmPop32(); int32_t num2 = vmPop32(); vmLit32(num1-num2);}}
extern fn int32 mul32(int32, int32) {{int32_t num1 = vmPop32(); int32_t num2 = vmPop32(); vmLit32(num1*num2);}}
extern fn int32 div32(int32, int32) {{int32_t num1 = vmPop32(); int32_t num2 = vmPop32(); vmLit32(num1/num2);}}
extern fn int32 mod32(int32, int32) {{int32_t num1 = vmPop32(); int32_t num2 = vmPop32(); vmLit32(num1%num2);}}


