package contracts

/** Contracts supports a number of utilities for checking preconditions, postconditions, and other assertions.
 * 
 * A typical example of using contracts in a method specification.
 * <pre> 
 *     int root( i : Int ) : Int =
 *         pre( i >= 0 )
 *         .pre( i < 1000000 )
 *         .post( x => x > 0 )
 *         .post( x => x*x <= i )
 *         .post( x => (x+1)*(x+1) > i )
 *         .in {
 *             ...
 *         }
 * </pre>
 *  
 *  
 */

trait Contracts {
    class PostResult[T]( postcond : T => Boolean, message : String = null ) { 
        var postList = List((postcond, message))
        
        def post( postcond : T => Boolean, message : String = null ) = {
            postList =  ( postcond, message) :: postList ;
            this
        }
        
    	def in( body : => T ) : T =  {
    		val result : T = body ;
    		for( (postcond,message) <- postList reverse) {
    			if( ! postcond(result) )
    			    if( message == null ) throw new AssertionError("Postcondition failed") 
    			    else throw new AssertionError("Postcondition failed: "+message) }
    		result }
    }
    
    class PostUnit( postcond : => Boolean, message : String = null ) { 
        var postList = List((() => postcond, message))
        
        def post( postcond : => Boolean, message : String = null ) = {
            postList =  ( () => postcond, message) :: postList ;
            this
        }
        
        def in( body : => Unit ) : Unit = {
			body ;	
    		for( (postcond,message) <- postList reverse ) {
    			if( ! postcond() )
    			    if( message == null ) throw new AssertionError("Postcondition failed") 
    			    else throw new AssertionError("Postcondition failed: "+message) }
		}
    }
    
	class Pre[T] {
	    def pre[T]( precond : => Boolean, message : String = null )  = 
	    	if( ! precond )
	    		if( message == null ) throw new AssertionError("precondition failed")
	    		else throw new AssertionError("precondition failed: "+message)
	    	else
	    		this
	    		
		def post( postcond : T => Boolean )  =
			new PostResult( postcond ) 
	    
		def post( postcond : => Boolean )  =
			new PostUnit( postcond )
        
        def in( body : => T ) : T =  {body }
	}
    
    protected def pre[T]( precond : => Boolean, message : String = null )  = 
        if( ! precond )
            if( message == null ) throw new AssertionError("precondition failed")
	    	else throw new AssertionError("precondition failed: "+message)
        else
            new Pre[T]
    
    protected def toDo(message : String = "") : Nothing =
        Contracts.toDo( message )
    
    protected def unreachable(message : String = "") : Nothing = 
        Contracts.unreachable( message )
    
    protected def check( b : Boolean, message : String = "Asertion failed" ) : Unit =
        Contracts.check( b, message )
}

object Contracts {
    def toDo(message : String = "") : Nothing =
        throw new AssertionError("Internal error. Unfinished code reached. "+ message)
            
    def unreachable(message : String = "") : Nothing =
        throw new AssertionError("Internal error. Unreachable code reached. "+ message)
        
    def check( b : Boolean, message : String = "Asertion failed" ) : Unit =
        if( ! b )
            throw new AssertionError( message )
} 