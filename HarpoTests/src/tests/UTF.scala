package tests

object UTF extends App {
    def show( str : String ) {
      for( ch <- str ) { printf("%d, %x", ch.toInt, ch.toInt) ; println  }
      println  
    }
    printf("UTF-8") ; println
    show("≤")
    show("\u2264")
}