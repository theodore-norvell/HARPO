package util

import scala.text.Document
import java.io.Writer
import java.io.StringWriter

trait Pretty {
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

object Pretty {
    
    def stack( seq : Seq[Document]) =
        (seq.foldLeft( Document.empty.asInstanceOf[Document] )( (x,y)=> x :/: y) )

    def list( seq : Seq[Document]) = 
        if( seq.isEmpty ) Document.text("()")
        else Document.group(
                Document.text("[ ") :: seq(0) ::
                Document.nest(2, seq.drop(1).foldLeft(Document.empty.asInstanceOf[Document])((doc,a)=> doc ::"," :/: a)) ::
                Document.text(" ]") )
               
    /** Function to turn a list of pretty things into something a document */
    def prettyList( seq: Seq[Pretty] ) : Document =
        Pretty.list( seq map {x => x.pp} )
    
    def func( f : Document  ) = {
        f :: Document.text("( )")
    }
    
    def func( f : Document, arg : Document  ) = {
        Document.group( f :: Document.nest(3, "(" :/: arg :: Document.text(" )") ) )
    }
    
    def func( f : Document, arg0 : Document, arg1 : Document  ) = {
        Document.group( f :: Document.nest(3, "(" :/: arg0 :: "," :/: arg1 :: Document.text(" )") ) )
    }
    
    def func( f : Document, arg0 : Document, arg1 : Document, arg2 : Document  ) = {
        Document.group( f :: Document.nest(3, "(" :/: arg0 :: "," :/: arg1 :: ","
                                                  :/: arg2 :: Document.text(" )") ) )
    }
    
    def func( f : Document, arg0 : Document, arg1 : Document, arg2 : Document, arg3 : Document  ) = {
        Document.group( f :: Document.nest(3, "(" :/: arg0 :: "," :/: arg1 :: ","
                                                  :/: arg2 :: "," :/: arg3 :: Document.text(" )") ) )
    }
    
    def func( f : Document, arg0 : Document, arg1 : Document, arg2 : Document, arg3 : Document , arg4 : Document ) = {
        Document.group( f :: Document.nest(3, "(" :/: arg0 :: "," :/: arg1 :: ","
                                                  :/: arg2 :: "," :/: arg3 :: ","
                                                  :/: arg4 :: Document.text(" )") ) )
    }
    
    def func( f : Document, arg0 : Document, arg1 : Document, arg2 : Document, arg3 : Document , arg4 : Document, arg5 : Document ) = {
        Document.group( f :: Document.nest(3, "(" :/: arg0 :: "," :/: arg1 :: ","
                                                  :/: arg2 :: "," :/: arg3 :: ","
                                                  :/: arg4 :: "," :/: arg5 :: Document.text(" )") ) )
    }
}