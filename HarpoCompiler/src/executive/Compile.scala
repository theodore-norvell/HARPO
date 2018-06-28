package executive

object Compile extends App {
  println("start")
  var compile = new HarpoToCCompiler()
  compile.addFile("0_MathClass_3.0", " (class Math public proc divide(in a,b: real32,out c:real32) (thread (*t0*) (while true (accept divide(in a,b:real32,out c:real32) c:=a/b; accept) while) thread) class)")
//  compile.addFile("0_pre-and-postcondition_3.0", " (class Math public proc divide(in a,b: real32,out c:real32) pre b !=0 post c'=a/b (thread (*t0*) (while true (accept divide(in a,b:real32,out c:real32) c:=a/b; accept) while) thread) class)")
  compile.runCompiler()
  var output:String = compile.getCOutput()
  println(output)
  println("end")
}