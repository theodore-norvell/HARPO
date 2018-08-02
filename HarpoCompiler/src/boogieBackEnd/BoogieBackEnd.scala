package boogieBackEnd
import java.net.URL
import scala.io.Source
import java.io.OutputStreamWriter
import scala.collection.mutable.ArrayBuffer
import scala.collection.mutable.HashMap
import frontEnd.ErrorRecorder
import frontEnd.AST
import frontEnd.AST._
import java.net.URL
import scala.io.Source


class BoogieBackEnd(val masterDeclList : frontEnd.AST.DeclList) {


	private def getBoogieCode():String = {
			println(masterDeclList.toString());
			val boogieCode = genBoogiePrelude() + genDeclCode( masterDeclList )
			println("getBoogieCode Running")
			println(boogieCode);
			return boogieCode.toString();

	}   
	private def genBoogiePrelude():String = {
			val filename = "BoogiePrelude.txt"
					val boogiePrelude = Source.fromFile(filename).getLines.mkString;
			println(boogiePrelude)
			return boogiePrelude
	}

	private def genDeclCode( dl : DeclList):String = {
			var globalObjCode = ""
					var initializeCode = ""
					var nameTbl = HashMap[String, HashMap[String, String]]()
					var boogieCode = ""  
					
					for(dlNd: DeclNd <- dl.decls){ // declarations set
						/*dlNd match{          
						case ClassDeclNd() => {
							val code = "type className; \n function dtype(Ref) returns (className); \n const unique " + dNd.Name + " :className\n";
							boogieCode += code;
						}
						case IntfDeclNd() => {//To Do

						}
						case ObjDeclNd() => {// To Do
							val code = "\n const unique " + dNd.fqn + " :Field "+ ty; //inject name of Node
							boogieCode += code
						}
						
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
						}*/
					}
						return boogieCode;
				}
}