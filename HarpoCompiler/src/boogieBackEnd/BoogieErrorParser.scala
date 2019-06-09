package boogieBackEnd
import scala.util.matching.Regex

/**  Translate the output of the Boogie verifier to a VerificationReport
  * @author theo
  */
class BoogieErrorParser {
    import BoogieErrorParser._
    def parseBoogieOutput( boogieOutput : String ) : VerificationReport = {
        return parseBoogieOutput( boogieOutput.split( "\n" ) )
    }
    
    def parseBoogieOutput( boogieOutput : Seq[String] ) : VerificationReport = {
        val vr : VerificationReport = new VerificationReport ;
        for(  line <- boogieOutput ) {
            line match {
                case syntaxErrorRE( line, col, message )  => vr.putOtherError(line.toInt, message)
                case verificationErrorStrRE( line, col, message )  => vr.putVerificationError(line.toInt, message)
                case relatedLocationRE( line, col, message )  => vr.addAssociatedLineNumber( line.toInt )
                case _ => {}
            }
        }
        return vr
    }
}

object BoogieErrorParser {
    val fileNameStr = ".*[.]bpl"
    val lineAndColStr = "[(]([0-9]+),([0-9]+)[)]"
    
    val syntaxErrorStr = "^" + fileNameStr + lineAndColStr + ": error:(.*)$"
    val syntaxErrorRE = syntaxErrorStr.r
    
    val verificationErrorStr = "^" + fileNameStr + lineAndColStr + ": Error BP[0-9]+:(.*)$"
    val verificationErrorStrRE = verificationErrorStr.r
    
    val relatedLocationStr = "^" + fileNameStr + lineAndColStr + ": Related location:(.*)$"
    val relatedLocationRE = relatedLocationStr.r
    
}
