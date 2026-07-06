%.slice = type {ptr, i64}
declare void @llvm.trap() nounwind
%.ctor = type { i32, ptr, ptr }
@llvm.global_ctors = appending global [1 x %.ctor] [%.ctor { i32 65535, ptr @.ctor, ptr null }]
@.doubleFmtString = global [3 x i8] c"%f\00" align 1
declare i32 @snprintf(ptr, i64, ptr, ...)
define void @.doubleToStr(ptr %out, i64 %len, double %arg) {
  %res = call i32 (ptr, i64, ptr, ...) @snprintf(ptr %out, i64 %len, ptr @.doubleFmtString, double %arg)
  ret void
}
%.struct.0 = type {float, float, float, float}
declare {<2 x float>, <2 x float>} @"ci_MulQ"(<2 x float>, <2 x float>, <2 x float>, <2 x float>)
define void @"main"() {
L1:
%2 = alloca %.struct.0, align 4
%3 = alloca %.struct.0, align 4
store %.struct.0 zeroinitializer, ptr %3
%4 = getelementptr inbounds %.struct.0, ptr %3, i32 0, i32 0
store float 0x0000000000000000, ptr %4
%5 = getelementptr inbounds %.struct.0, ptr %3, i32 0, i32 1
store float 0x0000000000000000, ptr %5
%6 = getelementptr inbounds %.struct.0, ptr %3, i32 0, i32 2
store float 0x0000000000000000, ptr %6
%7 = getelementptr inbounds %.struct.0, ptr %3, i32 0, i32 3
store float 0x3FF0000000000000, ptr %7
%8 = load %.struct.0, ptr %3
store %.struct.0 %8, ptr %2
%9 = alloca %.struct.0, align 4
%10 = load %.struct.0, ptr %2
%11 = load %.struct.0, ptr %2
%12 = extractvalue %.struct.0 %10, 0
%13 = insertelement <2 x float> undef, float %12, i32 0
%14 = extractvalue %.struct.0 %10, 1
%15 = insertelement <2 x float> %13, float %14, i32 1
%16 = extractvalue %.struct.0 %10, 2
%17 = insertelement <2 x float> undef, float %16, i32 0
%18 = extractvalue %.struct.0 %10, 3
%19 = insertelement <2 x float> %17, float %18, i32 1
%20 = extractvalue %.struct.0 %11, 0
%21 = insertelement <2 x float> undef, float %20, i32 0
%22 = extractvalue %.struct.0 %11, 1
%23 = insertelement <2 x float> %21, float %22, i32 1
%24 = extractvalue %.struct.0 %11, 2
%25 = insertelement <2 x float> undef, float %24, i32 0
%26 = extractvalue %.struct.0 %11, 3
%27 = insertelement <2 x float> %25, float %26, i32 1
%28 = call {<2 x float>, <2 x float>} @"ci_MulQ"(<2 x float> %15, <2 x float> %19, <2 x float> %23, <2 x float> %27)
%29 = alloca {<2 x float>, <2 x float>}, align 4
store {<2 x float>, <2 x float>} %28, ptr %29
%30 = load %.struct.0, ptr %29
store %.struct.0 %30, ptr %9
ret void
}


define void @.ctor() {
ret void
}