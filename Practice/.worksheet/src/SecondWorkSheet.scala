object SecondWorkSheet {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(68); 
  println("Welcome to the Scala worksheet");$skip(13); 
var x:Int=5;System.out.println("""x  : Int = """ + $show(x ));$skip(13); ;
var y:Int=6;System.out.println("""y  : Int = """ + $show(y ));$skip(30); ;

def square(x:Int): Int = x*x;System.out.println("""square: (x: Int)Int""");$skip(31); 

def square1(y:Int): Int = y*y;System.out.println("""square1: (y: Int)Int""");$skip(11); val res$0 = 

square(3);System.out.println("""res0: Int = """ + $show(res$0));$skip(26); 
var f:Double = square1(5);System.out.println("""f  : Double = """ + $show(f ));$skip(70); 

def printString(name: String): Any = {
println(s"Name is : $name")
};System.out.println("""printString: (name: String)Any""");$skip(22); val res$1 = 

printString("Inaam");System.out.println("""res1: Any = """ + $show(res$1))}
}
