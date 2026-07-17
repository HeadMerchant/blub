%.slice = type {ptr, i64}
declare void @llvm.trap() nounwind
%.ctor = type { i32, ptr, ptr }
@llvm.global_ctors = appending global [1 x %.ctor] [%.ctor { i32 65535, ptr @.ctor, ptr null }]
@.doubleFmtString = global [3 x i8] c"%f\00" align 1
declare i32 @snprintf(ptr, i64, ptr, ...)
define i32 @.doubleToStr(ptr %out, i64 %len, double %arg) {
  %res = call i32 (ptr, i64, ptr, ...) @snprintf(ptr %out, i64 %len, ptr @.doubleFmtString, double %arg)
  ret i32 %res
}
declare void @"write"(i32, ptr, i64)
define void @"printFloat_6"(float) {
L2:
%x = alloca float, align 4
store float %0, ptr %x
%3 = alloca [32 x i8], align 1
store [32 x i8] zeroinitializer, ptr %3
%4 = alloca i32, align 4
%5 = load float, ptr %x
%6 = fpext float %5 to double
%7 = call i32 @".doubleToStr"(ptr %3, i64 32, double %6)
store i32 %7, ptr %4
%8 = load i32, ptr %4
%9 = icmp ugt i32 %8, 0
br i1 %9, label %L10, label %L24
L10:
%11 = load i32, ptr %4
%12 = zext i32 %11 to i64
%13 = icmp ult i64 32, %12
br i1 %13, label %L14, label %L15
L14:
call void @llvm.trap()
unreachable
L15:
%16 = sub i64 %11, 0
%17 = icmp slt i64 0, 0
br i1 %17, label %L18, label %L19
L18:
call void @llvm.trap()
unreachable
L19:
%20 = load i8, ptr %3
%21 = getelementptr i8, ptr %3, i64 0
%22 = insertvalue %.slice undef, ptr %21, 0
%23 = insertvalue %.slice %22, i64 %16, 1
call void @"print_0"(%.slice %23)
br label %L24
L24:
ret void
}


define void @"printHex_5"(i32) {
L2:
%x = alloca i32, align 4
store i32 %0, ptr %x
%3 = alloca [8 x i8], align 1
store [8 x i8] zeroinitializer, ptr %3
%4 = alloca i32, align 4
store i32 0, ptr %4
%5 = alloca i32, align 4
%6 = load i32, ptr %x
store i32 %6, ptr %5
br label %L7
L7:
%8 = load i32, ptr %4
%9 = icmp ult i32 %8, 8
br i1 %9, label %L10, label %L34
L10:
%11 = alloca i32, align 4
%12 = load i32, ptr %5
%13 = urem i32 %12, 16
store i32 %13, ptr %11
%14 = load i32, ptr %11
%15 = icmp ult i32 %14, 10
br i1 %15, label %L16, label %L17
L16:
br label %L18
L17:
br label %L18
L18:
%19  = phi i32 [48, %L16], [55, %L17]
%20 = load i32, ptr %11
%21 = add i32 %20, %19
%22 = trunc i32 %21 to i8
%23 = load i32, ptr %4
%24 = sub i32 7, %23
%25 = zext i32 %24 to i64
%26 = icmp ule i64 8, %25
br i1 %26, label %L27, label %L28
L27:
call void @llvm.trap()
unreachable
L28:
%29 = getelementptr i8, ptr %3, i32 %24
store i8 %22, ptr %29
%30 = load i32, ptr %5
%31 = udiv i32 %30, 16
store i32 %31, ptr %5
%32 = load i32, ptr %4
%33 = add i32 %32, 1
store i32 %33, ptr %4
br label %L7
L34:
%35 = sub i64 8, 0
%36 = icmp slt i64 0, 0
br i1 %36, label %L37, label %L38
L37:
call void @llvm.trap()
unreachable
L38:
%39 = load i8, ptr %3
%40 = getelementptr i8, ptr %3, i64 0
%41 = insertvalue %.slice undef, ptr %40, 0
%42 = insertvalue %.slice %41, i64 %35, 1
call void @"println_1"(%.slice %42)
ret void
}


define void @"printBin_4"(i32) {
L2:
%x = alloca i32, align 4
store i32 %0, ptr %x
%3 = alloca [32 x i8], align 1
store [32 x i8] zeroinitializer, ptr %3
%4 = alloca i32, align 4
store i32 0, ptr %4
%5 = alloca i32, align 4
%6 = load i32, ptr %x
store i32 %6, ptr %5
br label %L7
L7:
%8 = load i32, ptr %4
%9 = icmp ult i32 %8, 32
br i1 %9, label %L10, label %L28
L10:
%11 = alloca i32, align 4
%12 = load i32, ptr %5
%13 = urem i32 %12, 2
store i32 %13, ptr %11
%14 = load i32, ptr %11
%15 = add i32 48, %14
%16 = trunc i32 %15 to i8
%17 = load i32, ptr %4
%18 = sub i32 31, %17
%19 = zext i32 %18 to i64
%20 = icmp ule i64 32, %19
br i1 %20, label %L21, label %L22
L21:
call void @llvm.trap()
unreachable
L22:
%23 = getelementptr i8, ptr %3, i32 %18
store i8 %16, ptr %23
%24 = load i32, ptr %5
%25 = udiv i32 %24, 2
store i32 %25, ptr %5
%26 = load i32, ptr %4
%27 = add i32 %26, 1
store i32 %27, ptr %4
br label %L7
L28:
%29 = sub i64 32, 0
%30 = icmp slt i64 0, 0
br i1 %30, label %L31, label %L32
L31:
call void @llvm.trap()
unreachable
L32:
%33 = load i8, ptr %3
%34 = getelementptr i8, ptr %3, i64 0
%35 = insertvalue %.slice undef, ptr %34, 0
%36 = insertvalue %.slice %35, i64 %29, 1
call void @"print_0"(%.slice %36)
ret void
}


define void @"print_s32_3"(i32) {
L2:
%x = alloca i32, align 4
store i32 %0, ptr %x
%3 = alloca [11 x i8], align 1
store [11 x i8] zeroinitializer, ptr %3
%4 = load i32, ptr %x
%5 = icmp eq i32 %4, 0
br i1 %5, label %L6, label %L19
L6:
%7 = icmp ule i64 11, 10
br i1 %7, label %L8, label %L9
L8:
call void @llvm.trap()
unreachable
L9:
%10 = getelementptr i8, ptr %3, i32 10
store i8 48, ptr %10
%11 = sub i64 11, 10
%12 = icmp slt i64 10, 0
br i1 %12, label %L13, label %L14
L13:
call void @llvm.trap()
unreachable
L14:
%15 = load i8, ptr %3
%16 = getelementptr i8, ptr %3, i64 10
%17 = insertvalue %.slice undef, ptr %16, 0
%18 = insertvalue %.slice %17, i64 %11, 1
call void @"print_0"(%.slice %18)
br label %L19
L19:
%20 = alloca i1, align 1
%21 = load i32, ptr %x
%22 = icmp slt i32 %21, 0
store i1 %22, ptr %20
%23 = alloca i32, align 4
store i32 11, ptr %23
%24 = alloca i32, align 4
%25 = load i1, ptr %20
br i1 %25, label %L26, label %L29
L26:
%27 = load i32, ptr %x
%28 = sub i32 0, %27
br label %L31
L29:
%30 = load i32, ptr %x
br label %L31
L31:
%32  = phi i32 [%28, %L26], [%30, %L29]
store i32 %32, ptr %24
br label %L33
L33:
%34 = load i32, ptr %24
%35 = icmp sgt i32 %34, 0
br i1 %35, label %L36, label %L53
L36:
%37 = load i32, ptr %23
%38 = sub i32 %37, 1
store i32 %38, ptr %23
%39 = alloca i32, align 4
%40 = load i32, ptr %24
%41 = srem i32 %40, 10
store i32 %41, ptr %39
%42 = load i32, ptr %24
%43 = sdiv i32 %42, 10
store i32 %43, ptr %24
%44 = load i32, ptr %39
%45 = trunc i32 %44 to i8
%46 = add i8 48, %45
%47 = load i32, ptr %23
%48 = zext i32 %47 to i64
%49 = icmp ule i64 11, %48
br i1 %49, label %L50, label %L51
L50:
call void @llvm.trap()
unreachable
L51:
%52 = getelementptr i8, ptr %3, i32 %47
store i8 %46, ptr %52
br label %L33
L53:
%54 = load i1, ptr %20
br i1 %54, label %L55, label %L64
L55:
%56 = load i32, ptr %23
%57 = sub i32 %56, 1
store i32 %57, ptr %23
%58 = load i32, ptr %23
%59 = zext i32 %58 to i64
%60 = icmp ule i64 11, %59
br i1 %60, label %L61, label %L62
L61:
call void @llvm.trap()
unreachable
L62:
%63 = getelementptr i8, ptr %3, i32 %58
store i8 45, ptr %63
br label %L64
L64:
%65 = load i32, ptr %23
%66 = zext i32 %65 to i64
%67 = sub i64 11, %66
%68 = icmp slt i64 %66, 0
br i1 %68, label %L69, label %L70
L69:
call void @llvm.trap()
unreachable
L70:
%71 = load i8, ptr %3
%72 = getelementptr i8, ptr %3, i64 %66
%73 = insertvalue %.slice undef, ptr %72, 0
%74 = insertvalue %.slice %73, i64 %67, 1
call void @"print_0"(%.slice %74)
ret void
}


define void @"print_u32_2"(i32) {
L2:
%x = alloca i32, align 4
store i32 %0, ptr %x
%3 = alloca [10 x i8], align 1
store [10 x i8] zeroinitializer, ptr %3
%4 = load i32, ptr %x
%5 = icmp eq i32 %4, 0
br i1 %5, label %L6, label %L19
L6:
%7 = icmp ule i64 10, 9
br i1 %7, label %L8, label %L9
L8:
call void @llvm.trap()
unreachable
L9:
%10 = getelementptr i8, ptr %3, i32 9
store i8 48, ptr %10
%11 = sub i64 10, 9
%12 = icmp slt i64 9, 0
br i1 %12, label %L13, label %L14
L13:
call void @llvm.trap()
unreachable
L14:
%15 = load i8, ptr %3
%16 = getelementptr i8, ptr %3, i64 9
%17 = insertvalue %.slice undef, ptr %16, 0
%18 = insertvalue %.slice %17, i64 %11, 1
call void @"print_0"(%.slice %18)
br label %L19
L19:
%20 = alloca i32, align 4
store i32 10, ptr %20
%21 = alloca i32, align 4
%22 = load i32, ptr %x
store i32 %22, ptr %21
br label %L23
L23:
%24 = load i32, ptr %21
%25 = icmp ugt i32 %24, 0
br i1 %25, label %L26, label %L43
L26:
%27 = load i32, ptr %20
%28 = sub i32 %27, 1
store i32 %28, ptr %20
%29 = alloca i32, align 4
%30 = load i32, ptr %21
%31 = urem i32 %30, 10
store i32 %31, ptr %29
%32 = load i32, ptr %21
%33 = udiv i32 %32, 10
store i32 %33, ptr %21
%34 = load i32, ptr %29
%35 = trunc i32 %34 to i8
%36 = add i8 48, %35
%37 = load i32, ptr %20
%38 = zext i32 %37 to i64
%39 = icmp ule i64 10, %38
br i1 %39, label %L40, label %L41
L40:
call void @llvm.trap()
unreachable
L41:
%42 = getelementptr i8, ptr %3, i32 %37
store i8 %36, ptr %42
br label %L23
L43:
%44 = load i32, ptr %20
%45 = zext i32 %44 to i64
%46 = sub i64 10, %45
%47 = icmp slt i64 %45, 0
br i1 %47, label %L48, label %L49
L48:
call void @llvm.trap()
unreachable
L49:
%50 = load i8, ptr %3
%51 = getelementptr i8, ptr %3, i64 %45
%52 = insertvalue %.slice undef, ptr %51, 0
%53 = insertvalue %.slice %52, i64 %46, 1
call void @"print_0"(%.slice %53)
ret void
}


@7 = global [1 x i8] c"\0A" align 1

define void @"println_1"(%.slice) {
L2:
%buf = alloca %.slice, align 8
store %.slice %0, ptr %buf
%3 = load %.slice, ptr %buf
call void @"print_0"(%.slice %3)
%4 = insertvalue %.slice undef, ptr @7, 0
%5 = insertvalue %.slice %4, i64 1, 1
call void @"print_0"(%.slice %5)
ret void
}


define void @"print_0"(%.slice) {
L2:
%buf = alloca %.slice, align 8
store %.slice %0, ptr %buf
%3 = getelementptr inbounds %.slice, ptr %buf, i32 0, i32 0
%4 = load ptr, ptr %3
%5 = getelementptr inbounds %.slice, ptr %buf, i32 0, i32 1
%6 = load i64, ptr %5
call void @"write"(i32 1, ptr %4, i64 %6)
ret void
}


@8 = global [14 x i8] c"hello from fmt" align 1

define void @"main"() {
L1:
%2 = insertvalue %.slice undef, ptr @8, 0
%3 = insertvalue %.slice %2, i64 14, 1
call void @"print_0"(%.slice %3)
ret void
}


define void @.ctor() {
ret void
}