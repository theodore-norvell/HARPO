package util
import scala.text.Document;
import java.io.Writer;
import java.io.StringWriter;

trait Format {
  
  def pp : Document = Document.text( this.toString() )
    
	def format(width: Int, writer: Writer) { this.pp.format( width, writer) }
    
  def format( width : Int ) : String = {
        val writer = new StringWriter() 
        this.format( width, writer )
        writer.toString
    }
  
  implicit protected def toDocument( seq : Seq[Pretty]) = 
        Pretty.prettyList( seq )
    
  implicit protected def toDocument( str : String ) = Document.text(str)
  
  implicit protected def toDocument( p : Pretty ) = p.pp
}

object Format {
  
   def func0( f : String  ) = {
       "\n" + f
   }
   
   def func1( f : String  ) = {
     "\n\t" + f
   }
      
   def func2( f : String  ) = {
   "\n\t\t" + f
   }
         
   def func3( f : String  ) = {
     "\n\t\t\t" + f
   }
            
   def func4( f : String  ) = {
     "\n\t\t\t\t" + f
   }
               
   def func5( f : String  ) = {
     "\n\t\t\t\t\t" + f
   }
     
}