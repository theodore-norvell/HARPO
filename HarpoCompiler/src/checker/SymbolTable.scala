package checker

import frontEnd.AST._
import frontEnd.{RQN,FQN}
import frontEnd.ErrorRecorder
import CheckerTypes._
import contracts.Contracts


/**
* A class mapping fully qualified names to declarations .
*/
class SymbolTable( private val errorRecorder : ErrorRecorder,
                   private val map : Map[ FQN, DeclNd ],
                   private val allMembersMap : Map[ FQN, Map[String, DeclNd]])
 extends Contracts {
    
    private def lookUpMember( baseFQN : FQN, name : String )
    : Option[DeclNd] = {
        // pre: the baseFQN should be for a class or an interface at some level.
        var classLikeQN = baseFQN
        while( ! baseFQN.isEmpty && ! allMembersMap.keySet.contains( classLikeQN )  )
            classLikeQN = classLikeQN.prefix
        check( ! classLikeQN.isEmpty )
        allMembersMap(classLikeQN).get( name ) }
    
    /** Return the decl for a member of a class or interface. 
     *  <p>Pre: Some prefix of baseFQN should be the fqn of a class.
     *  That is baseFQN is ::a::b::c , then one of ::a::b::c, ::a::b or ::a
     *  should be the FQN of a class.
     *  <p>The relevant class is the class whose fqn is the longest prefix.
     *  <p>Post: If the relevant class has no member (direct or inherited)
     *           whose name is given by the nameNd,
     *           then the result is null
     *  <p>Post: If the relevant class has a member  (direct or inherited)
     *           whose name is given by the nameNd,
     *           then the result is the declaration of that member
     */
    def lookUpMember( baseFQN : FQN, name : String, coord : Coord )
    : Option[DeclNd] =
        // pre: the baseFQN should be for a class or an interface at some level.
        lookUpMember( baseFQN, name ) match {
            case Some( decl ) => Some(decl)
            case None => errorRecorder.reportFatal("Could not find " +name, coord)
                         None
        }
    
	  /** Return the decl that corresponds to a fully qualified name.
     *  <p>Pre: the fqn is not empty.
     *  <p>Post: If there is a declaration whose fqn is this one, the result is Some of that declaration.
     *  <p>Post: If there is no declaration whose fqn is this one, the result is None and an error is reported.
     */
    private def lookupAbsolute( fqn : FQN, coord : Coord ) : Option[DeclNd] = { 
            if( map.contains( fqn ) ) Some(map.apply( fqn ))
            else {errorRecorder.reportFatal("Could not find " +fqn.toString, coord) ;
                  None }
    }
                 
    def lookUpRelative( baseFQN : FQN, qn : RQN ) : Option[DeclNd] = {
        // Pre: qn has length 1.
        // Try ever shorter prefixes of baseFQN until one succeeds or we hit the top scope.
        if( baseFQN.isEmpty ) {
            val name = baseFQN.append(qn)
            if( map.contains( name ) ) Some( map.apply( name ) )
            else None }
        else {
            val name = qn.last
            val optBaseDecl = map.get( baseFQN )
            if( optBaseDecl == None ) // TODO remove this 
                check( optBaseDecl != None )
            val base = optBaseDecl.get 
            base match {
                case base : ClassLike =>
                    val result = lookUpMember( baseFQN, qn.last )
                    if( result == None ) lookUpRelative( baseFQN.prefix, qn)
                    else result
                case otherwise =>
                    val key : FQN =  baseFQN.append(qn)
                    if( map.contains( key ) )
                        Some( map.apply( key ) )
                    else lookUpRelative( baseFQN.prefix, qn)
            }
        }
    }

  	/** Return the decl that a nameNd in a context given by an fqn.
     *  <p>Pre: the nameNd, qn is either absolute or of length 1
     *  <
     *  <p>Post: If the name node is absolute, result is the declaration that 
     *           the name refers to (the one whose fqn is the qn of the nameNd)
     *           or, if no such declaration exists, the result is null
     *  <p>Post: If the name node is not absolute, the result is the result of the following process.
     *  <ul>  <li>First we look for name in the context of the baseFQN. 
     *        <li>Should that fail, the baseFQN is shortened by one and the process is repeated.
     *        <li>If the baseQFN is empty and the name is not found in the global space, null is returned.
     *            As a side effect an error is reported.
     *  </ul>
     *  <p>At each level, if the baseFQN refers to a class or an interface, then inherited members are
     *  also eligible to be returned.
     */
    def lookUp( baseFQN : FQN, nameNd : NameNd ) : Option[DeclNd] = 
        pre( nameNd.qn.length() == 1 || nameNd.qn.isFullyQualified ) in
    {
        nameNd.qn match { 
        case fqn : FQN =>
            lookupAbsolute( fqn, nameNd.coord )
        case rqn : RQN =>  
    			  lookUpRelative( baseFQN, rqn ) match {
    			      case Some(decl) => Some(decl)
    			      case None =>
                          errorRecorder.reportFatal("Could not find " +nameNd.toString, nameNd.coord) ;
    			          None  } }
    }
    
    def dump = 
        for( k <- map.keys ) println( k.toString+  " +-> " +map(k) )
    
}
