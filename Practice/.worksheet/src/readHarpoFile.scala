object readHarpoFile {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(66); 
  println("Welcome to the Scala worksheet")

  case class HarpoToken(harpoLine: Array[String]);$skip(159); 

  def parseHarpoLine(string: String): HarpoToken = {
    val p = string.split(" ")
    HarpoToken(p)
  };System.out.println("""parseHarpoLine: (string: String)readHarpoFile.HarpoToken""");$skip(42); val res$0 = 
  new java.io.File(".").getAbsolutePath();System.out.println("""res0: String = """ + $show(res$0));$skip(76); 

  val harpoSource = io.Source.fromFile("0_pre-and-postcondition_3.0.txt");System.out.println("""harpoSource  : scala.io.BufferedSource = """ + $show(harpoSource ));$skip(56); 
  val prelude = io.Source.fromFile("boogiePrelude.txt");System.out.println("""prelude  : scala.io.BufferedSource = """ + $show(prelude ));$skip(49); 

  var lines = harpoSource.getLines.map(_.trim);System.out.println("""lines  : Iterator[String] = """ + $show(lines ));$skip(50); 

  var stringline: String = lines.mkString("\n");System.out.println("""stringline  : String = """ + $show(stringline ));$skip(22); 

  harpoSource.close;$skip(731); 
 val pf: PartialFunction[String, String] =
    {
      case "class" => "const unique 'name' : ClassName" // how to deal with Math
      case "procedure" => "procedure"
      case "invariant" => "invariant"
      case "pre" => "requires"
      case "post" => "ensures"
      case "ghost" => "var"
      case "assert" => "assert"
      case "assume" => "assume"
      case "int8" => "int"
      case "int16" => "int"
      case "int32" => "int"
      case "int64" => "int"
      case "int" => "int"
      case "real16 " => "real"
      case "real32" => "real"
      case "real64" => "real"
      case "*" => "*"
      case "/" => "/"
      case "!=" => "~="
      case "==" => "=="
      case ":=" => ":="
      case _ => " "

    };System.out.println("""pf  : PartialFunction[String,String] = """ + $show(pf ));$skip(43); 
  var stringline2 = stringline.split("\n");System.out.println("""stringline2  : Array[String] = """ + $show(stringline2 ));$skip(36); 
  var string3= stringline2.mkString;System.out.println("""string3  : String = """ + $show(string3 ));$skip(35); 
  val tokens1 = string3.split(" ");System.out.println("""tokens1  : Array[String] = """ + $show(tokens1 ));$skip(24); 
  
  
  var i: Int = 0;System.out.println("""i  : Int = """ + $show(i ));$skip(71); ;
  val boogieBuffer = scala.collection.mutable.ListBuffer.empty[String];System.out.println("""boogieBuffer  : scala.collection.mutable.ListBuffer[String] = """ + $show(boogieBuffer ));$skip(22); val res$1 = 
   stringline2.length;System.out.println("""res1: Int = """ + $show(res$1));$skip(28); 
   var boogieString = Array;System.out.println("""boogieString  : Array.type = """ + $show(boogieString ));$skip(136); 
   while(i<tokens1.length)
   {
   //println(stringline2(i));
   var temp: String = pf(tokens1(i))
   boogieBuffer += temp
   i+=1
   };$skip(23); val res$2 = 
   boogieBuffer.toList;System.out.println("""res2: List[String] = """ + $show(res$2))}

 
}
