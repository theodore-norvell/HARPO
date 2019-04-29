package boogieBackEnd

/** Always set the Translation Context before passing any ExpNd to buid(ExpNd, TransContext) for building Boogie expressions.
 *  Created by: Inaam Ahmed

 */

class TransContext (Heap: String, ObjRef: String) {
   
    var heap: String = Heap;
    var objRef: String = ObjRef
  
  
   def set(Heap: String, ObjRef: String)
   {
      heap = Heap;
      objRef = ObjRef;
   }
    
   def getHeap() : String = {
     heap
   }
   
   def getObjRef() : String = {
     objRef
   }
}