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
declare void @"write"(i32, ptr, i64)
define void @"6"(float) {
%x = alloca float, align 4
store float %0, ptr %x
%2 = alloca [10 x i8], align 1
%3 = insertvalue [10 x i8] zeroinitializer, i8 0, 0
%4 = insertvalue [10 x i8] %3, i8 0, 1
%5 = insertvalue [10 x i8] %4, i8 0, 2
%6 = insertvalue [10 x i8] %5, i8 0, 3
%7 = insertvalue [10 x i8] %6, i8 0, 4
%8 = insertvalue [10 x i8] %7, i8 0, 5
%9 = insertvalue [10 x i8] %8, i8 0, 6
%10 = insertvalue [10 x i8] %9, i8 0, 7
%11 = insertvalue [10 x i8] %10, i8 0, 8
%12 = insertvalue [10 x i8] %11, i8 0, 9
store [10 x i8] %12, ptr %2
%13 = load float, ptr %x
%14 = fpext float %13 to double
call void @".doubleToStr"(ptr %2, i64 10, double %14)
%15 = insertvalue %.slice undef, ptr %2, 0
%16 = insertvalue %.slice %15, i64 10, 1
call void @"0"(%.slice %16)
ret void
}


define void @"5"(i32) {
%x = alloca i32, align 4
store i32 %0, ptr %x
%2 = alloca [8 x i8], align 1
store [8 x i8] zeroinitializer, ptr %2
%3 = alloca i32, align 4
store i32 0, ptr %3
%4 = alloca i32, align 4
%5 = load i32, ptr %x
store i32 %5, ptr %4
br label %6
6:
%7 = load i32, ptr %3
%8 = icmp ult i32 %7, 8
br i1 %8, label %9, label %33
9:
%10 = alloca i32, align 4
%11 = load i32, ptr %4
%12 = urem i32 %11, 16
store i32 %12, ptr %10
%13 = load i32, ptr %10
%14 = icmp ult i32 %13, 10
br i1 %14, label %15, label %16
15:
br label %17
16:
br label %17
17:
%18  = phi i32 [48, %15], [55, %16]
%19 = load i32, ptr %10
%20 = add i32 %19, %18
%21 = trunc i32 %20 to i8
%22 = load i32, ptr %3
%23 = sub i32 7, %22
%24 = zext i32 %23 to i64
%25 = icmp ule i64 8, %24
br i1 %25, label %26, label %27
26:
call void @llvm.trap()
unreachable
27:
%28 = getelementptr i8, ptr %2, i32 %23
store i8 %21, ptr %28
%29 = load i32, ptr %4
%30 = udiv i32 %29, 16
store i32 %30, ptr %4
%31 = load i32, ptr %3
%32 = add i32 %31, 1
store i32 %32, ptr %3
br label %6
33:
%34 = sub i64 8, 0
%35 = icmp slt i64 0, 0
br i1 %35, label %36, label %37
36:
call void @llvm.trap()
unreachable
37:
%38 = getelementptr i8, ptr %2, i64 0
%39 = insertvalue %.slice undef, ptr %38, 0
%40 = insertvalue %.slice %39, i64 %34, 1
call void @"1"(%.slice %40)
ret void
}


define void @"4"(i32) {
%x = alloca i32, align 4
store i32 %0, ptr %x
%2 = alloca [32 x i8], align 1
store [32 x i8] zeroinitializer, ptr %2
%3 = alloca i32, align 4
store i32 0, ptr %3
%4 = alloca i32, align 4
%5 = load i32, ptr %x
store i32 %5, ptr %4
br label %6
6:
%7 = load i32, ptr %3
%8 = icmp ult i32 %7, 32
br i1 %8, label %9, label %27
9:
%10 = alloca i32, align 4
%11 = load i32, ptr %4
%12 = urem i32 %11, 2
store i32 %12, ptr %10
%13 = load i32, ptr %10
%14 = add i32 48, %13
%15 = trunc i32 %14 to i8
%16 = load i32, ptr %3
%17 = sub i32 31, %16
%18 = zext i32 %17 to i64
%19 = icmp ule i64 32, %18
br i1 %19, label %20, label %21
20:
call void @llvm.trap()
unreachable
21:
%22 = getelementptr i8, ptr %2, i32 %17
store i8 %15, ptr %22
%23 = load i32, ptr %4
%24 = udiv i32 %23, 2
store i32 %24, ptr %4
%25 = load i32, ptr %3
%26 = add i32 %25, 1
store i32 %26, ptr %3
br label %6
27:
%28 = sub i64 32, 0
%29 = icmp slt i64 0, 0
br i1 %29, label %30, label %31
30:
call void @llvm.trap()
unreachable
31:
%32 = getelementptr i8, ptr %2, i64 0
%33 = insertvalue %.slice undef, ptr %32, 0
%34 = insertvalue %.slice %33, i64 %28, 1
call void @"0"(%.slice %34)
ret void
}


define void @"3"(i32) {
%x = alloca i32, align 4
store i32 %0, ptr %x
%2 = alloca [11 x i8], align 1
store [11 x i8] zeroinitializer, ptr %2
%3 = load i32, ptr %x
%4 = icmp eq i32 %3, 0
br i1 %4, label %5, label %17
5:
%6 = icmp ule i64 11, 10
br i1 %6, label %7, label %8
7:
call void @llvm.trap()
unreachable
8:
%9 = getelementptr i8, ptr %2, i32 10
store i8 48, ptr %9
%10 = sub i64 11, 10
%11 = icmp slt i64 10, 0
br i1 %11, label %12, label %13
12:
call void @llvm.trap()
unreachable
13:
%14 = getelementptr i8, ptr %2, i64 10
%15 = insertvalue %.slice undef, ptr %14, 0
%16 = insertvalue %.slice %15, i64 %10, 1
call void @"0"(%.slice %16)
br label %17
17:
%18 = alloca i1, align 1
%19 = load i32, ptr %x
%20 = icmp slt i32 %19, 0
store i1 %20, ptr %18
%21 = alloca i32, align 4
store i32 11, ptr %21
%22 = alloca i32, align 4
%23 = load i1, ptr %18
br i1 %23, label %24, label %27
24:
%25 = load i32, ptr %x
%26 = sub i32 0, %25
br label %29
27:
%28 = load i32, ptr %x
br label %29
29:
%30  = phi i32 [%26, %24], [%28, %27]
store i32 %30, ptr %22
br label %31
31:
%32 = load i32, ptr %22
%33 = icmp sgt i32 %32, 0
br i1 %33, label %34, label %51
34:
%35 = load i32, ptr %21
%36 = sub i32 %35, 1
store i32 %36, ptr %21
%37 = alloca i32, align 4
%38 = load i32, ptr %22
%39 = srem i32 %38, 10
store i32 %39, ptr %37
%40 = load i32, ptr %22
%41 = sdiv i32 %40, 10
store i32 %41, ptr %22
%42 = load i32, ptr %37
%43 = trunc i32 %42 to i8
%44 = add i8 48, %43
%45 = load i32, ptr %21
%46 = zext i32 %45 to i64
%47 = icmp ule i64 11, %46
br i1 %47, label %48, label %49
48:
call void @llvm.trap()
unreachable
49:
%50 = getelementptr i8, ptr %2, i32 %45
store i8 %44, ptr %50
br label %31
51:
%52 = load i1, ptr %18
br i1 %52, label %53, label %62
53:
%54 = load i32, ptr %21
%55 = sub i32 %54, 1
store i32 %55, ptr %21
%56 = load i32, ptr %21
%57 = zext i32 %56 to i64
%58 = icmp ule i64 11, %57
br i1 %58, label %59, label %60
59:
call void @llvm.trap()
unreachable
60:
%61 = getelementptr i8, ptr %2, i32 %56
store i8 45, ptr %61
br label %62
62:
%63 = load i32, ptr %21
%64 = zext i32 %63 to i64
%65 = sub i64 11, %64
%66 = icmp slt i64 %64, 0
br i1 %66, label %67, label %68
67:
call void @llvm.trap()
unreachable
68:
%69 = getelementptr i8, ptr %2, i64 %64
%70 = insertvalue %.slice undef, ptr %69, 0
%71 = insertvalue %.slice %70, i64 %65, 1
call void @"0"(%.slice %71)
ret void
}


define void @"2"(i32) {
%x = alloca i32, align 4
store i32 %0, ptr %x
%2 = alloca [10 x i8], align 1
store [10 x i8] zeroinitializer, ptr %2
%3 = load i32, ptr %x
%4 = icmp eq i32 %3, 0
br i1 %4, label %5, label %17
5:
%6 = icmp ule i64 10, 9
br i1 %6, label %7, label %8
7:
call void @llvm.trap()
unreachable
8:
%9 = getelementptr i8, ptr %2, i32 9
store i8 48, ptr %9
%10 = sub i64 10, 9
%11 = icmp slt i64 9, 0
br i1 %11, label %12, label %13
12:
call void @llvm.trap()
unreachable
13:
%14 = getelementptr i8, ptr %2, i64 9
%15 = insertvalue %.slice undef, ptr %14, 0
%16 = insertvalue %.slice %15, i64 %10, 1
call void @"0"(%.slice %16)
br label %17
17:
%18 = alloca i32, align 4
store i32 10, ptr %18
%19 = alloca i32, align 4
%20 = load i32, ptr %x
store i32 %20, ptr %19
br label %21
21:
%22 = load i32, ptr %19
%23 = icmp ugt i32 %22, 0
br i1 %23, label %24, label %41
24:
%25 = load i32, ptr %18
%26 = sub i32 %25, 1
store i32 %26, ptr %18
%27 = alloca i32, align 4
%28 = load i32, ptr %19
%29 = urem i32 %28, 10
store i32 %29, ptr %27
%30 = load i32, ptr %19
%31 = udiv i32 %30, 10
store i32 %31, ptr %19
%32 = load i32, ptr %27
%33 = trunc i32 %32 to i8
%34 = add i8 48, %33
%35 = load i32, ptr %18
%36 = zext i32 %35 to i64
%37 = icmp ule i64 10, %36
br i1 %37, label %38, label %39
38:
call void @llvm.trap()
unreachable
39:
%40 = getelementptr i8, ptr %2, i32 %35
store i8 %34, ptr %40
br label %21
41:
%42 = load i32, ptr %18
%43 = zext i32 %42 to i64
%44 = sub i64 10, %43
%45 = icmp slt i64 %43, 0
br i1 %45, label %46, label %47
46:
call void @llvm.trap()
unreachable
47:
%48 = getelementptr i8, ptr %2, i64 %43
%49 = insertvalue %.slice undef, ptr %48, 0
%50 = insertvalue %.slice %49, i64 %44, 1
call void @"0"(%.slice %50)
ret void
}


@7 = global [1 x i8] c"\0A" align 1

define void @"1"(%.slice) {
%buf = alloca %.slice, align 8
store %.slice %0, ptr %buf
%2 = load %.slice, ptr %buf
call void @"0"(%.slice %2)
%3 = insertvalue %.slice undef, ptr @7, 0
%4 = insertvalue %.slice %3, i64 1, 1
call void @"0"(%.slice %4)
ret void
}


define void @"0"(%.slice) {
%buf = alloca %.slice, align 8
store %.slice %0, ptr %buf
%2 = getelementptr inbounds %.slice, ptr %buf, i32 0, i32 0
%3 = load ptr, ptr %2
%4 = getelementptr inbounds %.slice, ptr %buf, i32 0, i32 1
%5 = load i64, ptr %4
call void @"write"(i32 1, ptr %3, i64 %5)
ret void
}


%.struct.0 = type {i32}
define void @"main"() {
%1 = alloca %.struct.0, align 4
%2 = alloca %.struct.0, align 4
store %.struct.0 zeroinitializer, ptr %2
%3 = getelementptr inbounds %.struct.0, ptr %2, i32 0, i32 0
store i32 1, ptr %3
%4 = load %.struct.0, ptr %2
store %.struct.0 %4, ptr %1
%5 = load %.struct.0, ptr %1
call void @"9"(%.struct.0 %5)
ret void
}


define void @"9"(%.struct.0) {
%self = alloca %.struct.0, align 4
store %.struct.0 %0, ptr %self
%2 = load %.struct.0, ptr %self
call void @"8"(%.struct.0 %2)
ret void
}


@10 = global [2 x i8] c"ok" align 1

define void @"8"(%.struct.0) {
%self = alloca %.struct.0, align 4
store %.struct.0 %0, ptr %self
%2 = insertvalue %.slice undef, ptr @10, 0
%3 = insertvalue %.slice %2, i64 2, 1
call void @"0"(%.slice %3)
ret void
}


define void @.ctor() {
ret void
}