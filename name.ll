declare i32 @"printf"(ptr noalias captures(none), ...)
@.str = private unnamed_addr constant [16 x i8] c"hello world!!!\0A\00"
declare i32 @puts(ptr captures(none)) nounwind

define i32 @"main"() {
  %1 = add i8 1, 2
  call void @"print<i32,i32>"()
  ret i32 0
}

define void @"print<i32,i32>"() {
  call i32 @puts(ptr @.str)
  ret void
}
