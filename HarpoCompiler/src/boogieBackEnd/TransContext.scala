package boogieBackEnd

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