package executive

object Compile extends App {
  println("Compiler start")
  var compile = new HarpoToCCompiler()
  compile.addFile("mathClass","(class Math obj c:int32 :=0; thread (*t0*) claim c@1.0 assert 4=4; thread) class)")
  compile.runCompiler()
  var output:String = compile.getCOutput()
  println(output)
  println("Compiler end")
  
  
  println("Verifier start")
  var verify = new HarpoToBoogieTranslator()
  verify.addFile("mathClass.harpo","(class Math obj c:int32 :=0; thread (*t0*) claim c@1.0 assert 4=4; thread) class)")
  verify.runTranslator()
  var boogieScript:String = verify.getBoogieOutput()
  println(boogieScript)
  println("Verifier end")
  
}