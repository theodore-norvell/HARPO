package frontEnd

/** A qualified name is a relative or absolute path through the name space.
 *  Note that the list  is stored so that item 0 is the most specific name.
 *  I.e. a.b.c is stored as List("c","b","a") and can be constructed as either
 *  QN("c","b","a") or as QN( List("c","b","a") )
 */
sealed abstract class QN( names : List[String])  {

  def isFullyQualified : Boolean ;

	def apply( i : Int ) = names.apply( i ) 

	def length() = names.length 

	def last = names.head 

	def isEmpty = names.isEmpty

}

/** Fully qualified names.  These are relative to the root of the program */
case class FQN( names : List[String]) extends QN( names ) {

  def this( params : String* ) { this( List(params : _* )) }
  
  override def isFullyQualified = true ;
  
  override def toString() : String = {
      "::" ++ names.reduce((a,b) => b ++ "::" ++ a)
  }
  
  def prefix = new FQN( names.tail )
  
  def append( rqn : RQN) = new FQN( rqn.names ++ this.names )
  
  def append( name : String ) = new FQN( name :: this.names )
}

case class RQN( names : List[String]) extends QN( names ) {

  def this( params : String* ) { this( List(params : _* )) }
  
  override def toString() : String = {
      names.reduce((a,b) => b ++ "::" ++ a)
  }
  
  override def isFullyQualified = false ;
  
  def prefix = new RQN( names.tail )
}