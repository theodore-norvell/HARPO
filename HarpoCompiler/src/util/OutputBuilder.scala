package util

import contracts.Contracts
import frontEnd.AST.Coord
import scala.collection.mutable.StringBuilder 
import scala.collection.mutable.ArrayBuffer ;

/** Create an output text to be sent to a verifier (for example)
 */
class OutputBuilder extends Contracts {
    protected val builder = new StringBuilder
    
    protected var indentationLevel = 0 ;
    protected val indentationString = "    " ;
    protected var atNewLine = true ;
    
    protected val sourceMap = new ArrayBuffer[ Tuple2[String, Coord] ] ;
    protected var currentPair : Tuple2[String, Coord] = null ;
    protected var messageHasBeenSetOnThisLine = false ;
    
    /** Adds a string to the output.
     *  
     *  If the string contains embedded newlines, these are treated
     *  as calls to <code>.newLine</code>.
     *  
     *  Strings at the start of a line will be indented with spaces
     *  to the current indentation level.
     *  
     *  Strings must not contain any linefeed or formfeed characters.
     */
    def put( string : String ) {
        var str = string 
        var i = str.indexOf( '\n' ) ; 
        while( i != -1  ) {
            addToLine( str.substring(0, i) )
            str = str.substring( i+1 )
            i = str.indexOf( '\n' )
            newLine
        }
        addToLine( str ) ;
    }
    
    protected def addToLine( str : String ) : Unit =
    pre( str.indexOf( '\n' ) == -1 )
    .pre( str.indexOf( '\r' ) == -1 )
    .pre( str.indexOf( '\f' ) == -1 )
    .in {
        if( str.length() == 0 ) return 
        if( atNewLine ) { 
            for( i <- 0 until indentationLevel ) 
                builder.append( indentationString ) 
            atNewLine = false ;
        }
        builder.append( str ) 
    }
    
    /** Add a newLine to the output.
     */
    def newLine {
        sourceMap += currentPair 
        messageHasBeenSetOnThisLine = false
        builder.append( "\n" )
        atNewLine = true ;
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
    .pre(! messageHasBeenSetOnThisLine, "Two errors on one line" )
    .in {
        currentPair = ( mess, coord ) 
        messageHasBeenSetOnThisLine = true
    }
    
    /** Clear the error message associated with a line
     *  
     *  It is forbidden to clear an error message that has been set
     *  and not associated with at least one line.
     */
    def clearError : Unit = 
    pre(! messageHasBeenSetOnThisLine)
    .in {
        currentPair = null
    }
    
    /** Get the error (if any) associated with a given line of output */
    def getError( outputLineNumber : Int ) : Option[Tuple2[String, Coord]] = {
        if( outputLineNumber < 0 ) return None
        if( outputLineNumber >= sourceMap.length ) return None
        val pair = sourceMap( outputLineNumber ) 
        if( pair == null ) None else Some( pair ) 
    }
    
    /** Increase the amount of indentation at the start of the next line.
     *  
     *  This call affects the current line if nothing has been output on it yet.
     *  E.g. <code>put(a) newLine indent put(b) put(c) newLine </code> will
     *  indent the second line. But <code>put(a) newLine put(b) indent put(c) newLine </code>
     *  will not indent the second line unless b is an empty string.
     */
    def indent {
        indentationLevel += 1 
    }
    
    /** Decrease the amount of indentation at the start of the next line. */
    def dedent {
        if( indentationLevel > 0 ) indentationLevel -= 1 
    }
    
    /** Obtain the string built so far. */
    def result() : String = builder.result() 
}