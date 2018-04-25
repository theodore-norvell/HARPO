package tests

import org.scalatest.FlatSpec
import org.scalatest.junit.JUnitRunner
import org.scalatest.FunSuite
import java.io._
import org.junit.runner.RunWith
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import parser.HarpoParser
import frontEnd.StandardErrorRecorder
import frontEnd.AST
import frontEnd.AST.DeclNd
import frontEnd.AST.ThreadDeclNd
import frontEnd.AST.CommandNd
import cBackEnd.CFGBuilder
import cBackEnd.CFGFactory
import frontEnd.AST.ClassDeclNd
import frontEnd.AST.ClassLike
import cBackEnd._
import cBackEnd.CFGDivider
import cBackEnd.CFG.CFGNd
import cBackEnd.CFG.StartCfgNd

@RunWith(classOf[JUnitRunner])
class BackEndTests extends  TestsBase {
  val out = new OutputStreamWriter(System.out)
  

  //  (thread ((u,v):=( u<=v<v ,4) (u,v):=4,8) thread)
// proc deposit(in value : int)
// proc fetch(out value : int)
  
  behavior of "The C back end" ;
  
  it should "generate code for Accept statement" in {
    val str = """(class FIFO  (in capacity : int)
 obj a : int:=0
 obj front :int := 0
 obj size :int:= 0
(thread
(while true do
 a:=0
(accept
deposit( in value : int ) when size < capacity
a  := value
size := size + 1
|
fetch( out value : int ) when size > 0
value := a
front := (front + 1) / capacity
size := size - 1
accept)
       a:=1
while)
thread)
class)"""
    tryWithCBackEnd(str)
  }

      it should "generate code for IF statement" in {
        val str = """(class Client{ type a, type b extends B}
                                (in x : X, obj y : Y)
                         obj u:FIFO := new FIFO()  obj a: int :=0
           
          (thread a:=0  u.fetch()  a:=1  thread)
    
                     class)"""
        tryWithCBackEnd(str)
      }
    
//      it should "generate code for while statement" in {
//        val str = """(class Fred{ type a, type b extends B}
//                                (in x : X, obj y : Y)
//                         obj u:int := 0   obj v : int8 := 0    obj w : bool := false
//           
//          (thread (while u<v<u do w := true while) thread)
//    
//                     class)"""
//        tryWithCBackEnd(str)
//      }
//
//        it should "parse a class with declarations" in {
//      val str = """(class Fred{ type a, type b extends B}
//                              (in x : X, obj y : Y)
//                       obj u:int := 0   obj v : int := 0    obj w : bool := false
//         
//        (thread w:= u<v<v thread)
//  
//                   class)""" 
//      tryWithCBackEnd(str)  
//    }
}