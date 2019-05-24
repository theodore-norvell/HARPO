package boogieBackEnd

/** Always set the Translation Context before passing any ExpNd to buid(ExpNd, TransContext) for building Boogie expressions.
 *  Created by: Inaam Ahmed
 *  Whenever Set is called Translation Context will also need to be reset for next translation
 */

class TransContext (Heap: String, ObjRef: String) {
   
    var initHeap: String = Heap;
    var initObjRef : String = ObjRef;
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
   
  def setHeap(Heap: String) {
     heap = Heap;
   }
   
   def setObjRef(ObjRef : String) {
     objRef = ObjRef;
   }
   
   def reset() {
     heap = initHeap;
     objRef = initObjRef;
     
   }
}