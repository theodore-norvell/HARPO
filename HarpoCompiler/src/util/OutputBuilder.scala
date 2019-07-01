package util

import contracts.Contracts
import frontEnd.AST.Coord
import scala.collection.mutable.StringBuilder 
import scala.collection.mutable.ArrayBuffer ;

/** Create an output text to be sent to a verifier (for example)

 */
class OutputBuilder extends Contracts {
    // This is not pretty, but it seems to work.
    // We use a doubly linked list with a sentinel at either end.
    // Lines go through 3 stages of life.
    //  * Not started (and not done)
    //  * Started and not done
    //  * Started and done
    // Lines that are not started, don't contribute to the output.
  
    class Line {
        protected[OutputBuilder] var indentLevel : Int = 0
        protected[OutputBuilder] var text = "" 
        protected[OutputBuilder] var error : Tuple2[String, Coord] = null ;
        protected[OutputBuilder] var prev : Line = null 
        protected[OutputBuilder] var next : Line = null 
        protected[OutputBuilder] var started = false
        protected[OutputBuilder] var done = false
        protected[OutputBuilder] var errorSetOnThisLine = false 
    }
    
    class MyIterator extends Iterator[String] {
        var cursor = head.next ;
        override def hasNext : Boolean = {
            while( !cursor.started && cursor.next != null ) cursor = cursor.next 
            cursor.next != null ; }
        override def next() : String = pre( hasNext ).in{
            val v = "    " * cursor.indentLevel + cursor.text
            cursor = cursor.next ;
            v }
    }
    
    
    protected val head = new Line() ;
    protected var current = new Line() ;
    protected val tail = new Line() ;
    // Invariant. There is a sequence of distinct Line objects
    // [ l(0), l(1), ..., l(n) ]
    // such that n > 1 and l(0) == head and l(n) == tail
    // and head.prev == null and tail.next == null
    // and for all i, such that 0 < i _< n
    //    l(i-1).next == l(i) and l(i).prev == l(l-1).next
    // also head.started and head.done 
    //  and ! tail.started and ! tail.done
    // and there exists an i in {1,..,n-1} such that current == l(i)
    head.started = true
    head.done = true
    head.next = current
    current.next = tail
    tail.prev = current 
    current.prev = head
    

    
    /** Adds a string to the output.
     *  
     *  If the string contains embedded newlines, returns
     *  return-newline pairs, these are treated
     *  as calls to <code>.newLine</code>.
     *  
     *  Strings at the start of a line will be indented with spaces
     *  to the current indentation level.
     *  
     */
    def put( string : String ) : Unit =
    {
        var str = string 
        val endOfLineRE = """\r\n|\n|\r""".r
        var m = endOfLineRE.findFirstMatchIn( str )
        while( !m.isEmpty  ) {
            val matchData = m.get
            
            addToLine( matchData.before.toString )
            newLine
            str = matchData.after.toString
            m = endOfLineRE.findFirstMatchIn( str )
        }
        addToLine( str ) 
    }
    
    protected def startNewLine : Unit = {
        val newLine = new Line() ;
        newLine.indentLevel = current.indentLevel 
        newLine.error = current.error ;
        newLine.prev = current
        newLine.next = current.next
        current.next = newLine
        newLine.next.prev = newLine 
        current = newLine 
    }
    
    protected def addToLine( str : String ) : Unit = {
        if( str.length() > 0 ) {
            current.text = current.text + str ;
            current.started = true ;
        }
    }
    
    /** Add a new line to the output.
     */
    def newLine {
        if( ! current.started ) current.started = true ;
        current.done = true ;
        startNewLine 
    }
    
    def saveLine() : Line = { current ; }
    
    def goToAfter( savedLine : Line ) : Unit = {
        current = savedLine ;
        if( current.done ) newLine
    }
    
    def goToBefore( savedLine : Line ) : Unit = {
        current = savedLine.prev ;
        if( current.done ) newLine
    }
    
    def goToEnd() : Unit = {
        current = tail.prev ;
        if( current.done ) newLine
    }
    
    /** Set the current error message.
     *  
     *  The actual association happens on the newLine. I.e. when a
     *  newLine is added to the output, the line that it ends gets
     *  associated with the current error message if there is one.
     *  
     *  The same message will be associated with future lines until
     *  there is a call to <code>clearError</code>.
     *  
     *  It is forbidden to set the error message but not subsequently
     *  use it for at least one line.
     *  
     *  Examples: These examples assume no newlines are embedded in the puts.
     *  <ul><li> <code>setError(m, c), put(a), newLine put(b) newLine</code>
     *           Both lines will be associated with the same error message.
     *      <li> <code>setError(ma, ca), put(a), newLine, setError(mb, cb), put(b), newLine</code>
     *           Each line will have a separate error message.
     *      <li> <code>setError(ma, ca), put(a), newLine, clearError, put(b), newLine, setError(mc, cc), put(c), newLine</code>
     *           The first line is associated with the first message, the second line
     *           with no error message, and the third line with another error message.
     *      <li> <code>put(a), setError(ma, ca), newLine, put(b), clearError, newLine, put(c), setError(mc, cc), newLine</code>
     *           Same as previous example.
     *      <li> <code>setError(ma, ca), put(a), setError(mb, cb)</code>
     *           Forbidden. First error is not used.
     *      <li> <code>setError(ma, ca), put(a), clearError</code>
     *           Forbidden. Error is not used.
     */
    def setError( mess : String, coord : Coord ) : Unit = 
    pre( coord != null && mess != null)
    .pre( ! current.errorSetOnThisLine, "Two errors on one line" )
    .in {
        current.error = ( mess, coord )
        current.errorSetOnThisLine = true
    }
    
    /** Clear the error message associated with a line
     *  
     *  It is forbidden to clear an error message that has been set
     *  and not associated with at least one line.
     */
    def clearError : Unit = 
    pre( ! current.errorSetOnThisLine )
    .in {
        current.error = null
    }
    
    /** Get the error (if any) associated with a given line of output
     *  @param outputLineNumber The line number of the output counted from 1
     *  @result The error message and coordinate (wrapped in a Some) associated
     *          with the line number if any. Otherwise None.
     *   */
    def getError( outputLineNumber : Int ) : Option[Tuple2[String, Coord]] = {
        // This isn't very efficient. But really, who cares?
        var i = 1 ;
        var line = head.next ;
        def loop : Unit = {
            while( !line.started && line != tail ) line = line.next 
            if( line != tail && i != outputLineNumber ) {
                line = line.next
                i += 1 ;
                loop ; } }
        loop
        if( line == tail ) None
        else if( line.error == null ) None 
        else Some( line.error ) 
    }
    
    /** Increase the amount of indentation at the start of the next line.
     *  
     *  This call affects the current line if nothing has been output on it yet.
     *  E.g. <code>put(a) newLine indent put(b) put(c) newLine </code> will
     *  indent the second line. But <code>put(a) newLine put(b) indent put(c) newLine </code>
     *  will not indent the second line unless b is an empty string.
     */
    def indent {
        current.indentLevel += 1 
    }
    
    /** Decrease the amount of indentation at the start of the next line. */
    def dedent {
        if( current.indentLevel > 0 ) current.indentLevel -= 1 
    }
    
    /** Obtain the string built so far. */
    def resultAsString() : String = {
        result.mkString( "\n" ) }
    
    /** Obtain the string built so far. */
    def result() : Iterator[String] ={ new MyIterator() ; }
}
