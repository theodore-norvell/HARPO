object HarpoPrograms {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(46); 
	println("Hello Scala");$skip(17); 
	
	var x:Int =90;System.out.println("""x  : Int = """ + $show(x ));$skip(18); 
	val y:Float= 100;System.out.println("""y  : Float = """ + $show(y ));$skip(14); 
	println(x+y)}

}
