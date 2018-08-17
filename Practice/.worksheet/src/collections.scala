object collections {;import org.scalaide.worksheet.runtime.library.WorksheetSupport._; def main(args: Array[String])=$execute{;$skip(64); 
  println("Welcome to the Scala worksheet");$skip(17); val res$0 = 
  Array(1, 2, 3);System.out.println("""res0: Array[Int] = """ + $show(res$0));$skip(28); val res$1 = 
  List(5, 2, 7, 9, true, 1);System.out.println("""res1: List[AnyVal] = """ + $show(res$1));$skip(36); val res$2 = 
  List(5, 2, 7, 9, true, "Name", 1);System.out.println("""res2: List[Any] = """ + $show(res$2));$skip(27); 
  val arr = Array(1, 2, 3);System.out.println("""arr  : Array[Int] = """ + $show(arr ));$skip(35); 
  var _f = List(6, 3, 2, 7, 94, 2);System.out.println("""_f  : List[Int] = """ + $show(_f ));$skip(17); val res$3 = 
  _f(0) + arr(2);System.out.println("""res3: Int = """ + $show(res$3));$skip(15); 

  arr(0) = 6;$skip(17); 
  _f = 900 :: _f;$skip(8); val res$4 = 

  arr;System.out.println("""res4: Array[Int] = """ + $show(res$4));$skip(36); 
  
  val arr2 = new Array[Int](10);System.out.println("""arr2  : Array[Int] = """ + $show(arr2 ));$skip(39); 
  
  var arr3 = new Array[String] (20);System.out.println("""arr3  : Array[String] = """ + $show(arr3 ));$skip(33); 
  
  val arr4= Array.fill(20)(9);System.out.println("""arr4  : Array[Int] = """ + $show(arr4 ));$skip(64); 
  var arr5Random = Array.fill(5)(math.random);System.out.println("""arr5Random  : Array[Double] = """ + $show(arr5Random ));$skip(46);  // passed by name
  
  
  val arr6 = Array.tabulate(10)(i => i);System.out.println("""arr6  : Array[Int] = """ + $show(arr6 ));$skip(93); 
                                                  
  var arr7 = Array.tabulate(10)(i=>i*i*i);System.out.println("""arr7  : Array[Int] = """ + $show(arr7 ))}

}
