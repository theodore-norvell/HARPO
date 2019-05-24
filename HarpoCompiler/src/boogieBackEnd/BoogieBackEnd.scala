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
import scala.io.Source
import checker.CheckerTypes.PrimitiveType;
import java.io.File
import util.Format
import scala.text.Document
import util.OutputBuilder;

class BoogieBackEnd(val masterDeclList : frontEnd.AST.DeclList, var outputBuffer: OutputBuilder) extends Format {
  
	def getBoogieCode(): OutputBuilder = {
			val boogiePrelude = getBoogiePrelude() 
			outputBuffer.put(boogiePrelude)
			outputBuffer.newLine
			genBoogieCode( masterDeclList)
			outputBuffer		
	}   
	private def getBoogiePrelude():String = {       
        	val preludeUrl : URL = this.getClass().getResource("/boogieBackEnd/BoogiePrelude.txt")
          val prelude = new File(preludeUrl.toURI())
          val contents = Source.fromFile(prelude)
          val boogiePrelude = try contents.mkString finally contents.close()
          return boogiePrelude
}

	private def genBoogieCode( dl : DeclList) : OutputBuilder = {
			    
	        var globalObjCode = ""
					var initializeCode = ""
					
					var globalObjects = ""
					var interfaces = ""
					var classes = ""
					
				 for(dlNd : DeclNd <- dl.decls) {
						dlNd match{ 						
						case ObjDeclNd( isGhost,isConst, acc, ty, initExp ) => {
						  val objType: String = TypeCodeGen(ty)
						  val objInit: String = new ExpCodeGen().initExpCodeGen(initExp)
						  outputBuffer.newLine
						  outputBuffer.put("\nconst unique " + dlNd.fqn + ":" + "Field " + objType + ";")
						}
						case IntfDeclNd() => {
						  interfaces += "\nconst unique " + dlNd.name + ": className;" 
						  outputBuffer.newLine
						  outputBuffer.put( "\nconst unique " + dlNd.name + ": className;")
						}	
						case ClassDeclNd() => {
						  val classCode=new ClassCodeGen(dlNd, outputBuffer)
						  outputBuffer = classCode.classCodeGen()
						}
						case _ => {
						  val code = "No main declarations were found"
						  outputBuffer
						}
					}
				}
			outputBuffer
  }
}