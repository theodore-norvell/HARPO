/*
						case ConstDeclNd()=>{ // To Do

						}

						case ParamDeclNd() => { // To Do
						}

						case MethodDeclNd() => { // To Do
						}

						case ThreadDeclNd() => {
						}

						case LocalDeclNd() => { // To Do
						}
						case GenericParamDeclNd() => { // To Do
						}

						// PrimitiveTypeDeclNd is used in the symbol table so that primitive types map to something.
						case PrimitiveTypeDeclNd() => { // TO Do
						}

						// The variable in a for loop, co loop, or array initialization.
						// Invariant: The parent of a ForVarDecl must be a ForDecl.
						case ForVarDecl()=>{// To Do
						}

						// A for loop, co loop, or array initialization.
						// These get their own FQN and so have their own "dummy" declaration
						// The name is some string that can not conflict with a user defined name
						case ForDecl() => {
						}

						// Method implementations are kind of odd as they are both declarations
						// and references to other declarations.
						case MethodImplementationDeclNd() => {

						}
						/****************/
						/** Statements **/
						/****************/

						case SkipCmdNd() => {

						}

						case SeqCommandNd() => {

						}

						case LocalDeclCmdNd () => {

						}

						case AssignmentCmdNd() => {
						}

						case CallCmdNd() =>{

						}

						case IfCmdNd() => {
						}

						case WhileCmdNd() => {

						}

						case ForCmdNd() => {
						}

						case CoForCmdNd() => {
						}

						case CoCmdNd() => {
						}

						case AcceptCmdNd() => {
						}

						case WithCmdNd() => {
						}
						// Making commands for BoogieBackend
						case AssertCmdNd() => {
						}
						case AssumeCmdNd() => {

						}

						/************/
						/** Types **/
						/**********/
						// TODO override toString for all types.

						case NoTypeNd() => {
						}

						case TopTypeNd() => {
						}

						case NamedTypeNd() => {

						}

						case ArrayTypeNd() =>{

						}

						/*******************/
						/** Expressions  **/
						/*****************/

						case NoExpNd() => {

						}

						case IntLiteralExpNd() => {

						}

						case FloatLiteralExpNd() => {

						}

						case NameExpNd() => {

						}

						case BinaryOpExpNd() => {
						}

						case UnaryOpExpNd() => {
						}

						case AsExpNd() => {
						}

						case MemberExpNd() => {
						}

						case ChainExpNd() => {
						}

						case FetchExpNd() => {
						}

						/*********************************/
						/** Initialization Expressions  **/
						/*********************************/

						case ValueInitExpNd() => {
						}

						case NewInitExpNd() => {
						}

						case ArrayInitExpNd() => {
						}

						case IfInitExpNd() => {
						}

						case WidenInitExpNd() => {
						}
						/************/
						/** Names  **/
						/************/

						case NameNd() => {
						}

						/*********************/
						/** Accessibility   **/
						/*********************/


						/**************************/
						/** Parameter Categories **/
						/**************************/


						/*******************/
						/** Operators    **/
						/*****************/
						}
*/
package boogieBackEnd
import java.net.URL
import scala.io.Source
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import frontEnd.ErrorRecorder
import frontEnd.AST
import frontEnd.AST._
import checker.CheckerTypes._
import scala.io.Source
import checker.CheckerTypes.PrimitiveType;
import java.io.File
import util.Format
import scala.text.Document
import util.OutputBuilder;

class BoogieBackEnd {

  protected var outputBuilder = new OutputBuilder;

  def getOutputBuilder: OutputBuilder = this.outputBuilder;

  def genBoogieCode(masterDeclList: frontEnd.AST.DeclList): OutputBuilder = {
    genBoogiePrelude("/boogieBackEnd/BoogiePrelude.txt")
    genBoogieTranslation(masterDeclList)
    this.outputBuilder
  } 
  private def genBoogiePrelude(fileName: String): Unit = {
    val preludeUrl: URL = this.getClass().getResource(fileName)
    val prelude = new File(preludeUrl.toURI())
    val contents = Source.fromFile(prelude)
    try {
      for (line <- contents.getLines) {
        outputBuilder.put(line)
        outputBuilder.newLine
      }
    } finally contents.close()
  }

  private def genBoogieTranslation(dl: DeclList): Unit = {
    outputBuilder.newLine
    //Iterate the top level declaration nodes
    for (dlNd: DeclNd <- dl.decls) {
      dlNd match {
        case ObjDeclNd(isGhost, isConst, acc, ty, initExp) => {
          val heapTransContext = new TransContext("Heap", "This_" + dlNd.fqn.toString)
          val objType: String = TypeCodeGen(ty)
          val objInit: String = ExpCodeGen.initExpCodeGen(initExp,heapTransContext)
          ty.tipe.get match {
            case ArrayLocationType (baseType) => {
              outputBuilder.putln("const unique " + dlNd.fqn + ":" + "Field(ArrayRef " + objType + ");")
            }
            case LocationType (baseType) => {
              outputBuilder.putln("const unique " + dlNd.fqn + ":" + "Field " + objType + ";")
              }
            case _=>
          }

        }
        case IntfDeclNd() => {
          outputBuilder.putln("const unique " + dlNd.name + " : ClassName;")
        }
        case ClassDeclNd() => {
          val classCode = new ClassCodeGen(dlNd, outputBuilder)
          classCode.classCodeGen()
          outputBuilder = classCode.getOutputBuilder()
          outputBuilder.newLine
        }
        case _ =>
      }
    }
  }
}
