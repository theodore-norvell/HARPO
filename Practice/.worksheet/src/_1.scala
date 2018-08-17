object _1 {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(53); 
println("Welcome to the Scala worksheet");$skip(10); 

var a=9;System.out.println("""a  : Int = """ + $show(a ));$skip(37); ;
val next = if (a%2 == 0) a/2 else ();System.out.println("""next  : AnyVal = """ + $show(next ));$skip(23); val res$0 = 

if (a<8) "hi" else ();System.out.println("""res0: Any = """ + $show(res$0));$skip(21); val res$1 = 
if(a>8) "hi" else 16;System.out.println("""res1: Any = """ + $show(res$1));$skip(6); 

a=2;$skip(74); val res$2 = ;

a match{
case 0 => "zero"
case 1 => "one"
case _ => "Something Bigger"
};System.out.println("""res2: String = """ + $show(res$2))}

}
