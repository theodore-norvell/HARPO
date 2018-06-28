object FirstWorksheet
{;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(41); val res$0 = 
  "5.8".toDouble;System.out.println("""res0: Double = """ + $show(res$0));$skip(13); ;
	
	val a=87;System.out.println("""a  : Int = """ + $show(a ));$skip(22); ;
	
	val b:Double=55.0;System.out.println("""b  : Double = """ + $show(b ));$skip(18); ;
	
	var x,y,z = 9;System.out.println("""x  : Int = """ + $show(x ));System.out.println("""y  : Int = """ + $show(y ));System.out.println("""z  : Int = """ + $show(z ));$skip(18); ;
	var (c,d)=(4,5);System.out.println("""c  : Int = """ + $show(c ));System.out.println("""d  : Int = """ + $show(d ));$skip(22); ;
 	var (t,w)=("t",30);System.out.println("""t  : String = """ + $show(t ));System.out.println("""w  : Int = """ + $show(w ));$skip(17); val res$1 = ;
 	(4,5,"Hello");System.out.println("""res1: (Int, Int, String) = """ + $show(res$1));$skip(10); ;
 
  x=20;$skip(8); ;
  y=55;$skip(45); ;
  
  if(x>90) println("Hi") else println(y);$skip(41); ;

 	var thisIsChoice = if(y>30) x else ();System.out.println("""thisIsChoice  : AnyVal = """ + $show(thisIsChoice ));$skip(112); 

	var r=a*1 match
	{
	case 0 => println(1)
	case 1 => println(2)
	case _ => println(3)
	case 87=> println(4)
	};System.out.println("""r  : Unit = """ + $show(r ))}

}
