package checker

import frontEnd.AST._ 
import frontEnd.QN
import frontEnd.ErrorRecorder
import CheckerTypes._
import scala.collection.mutable.ArrayBuffer 

class Checker( val errorRecorder : ErrorRecorder ) {
    
    val symbolTableMaker = new SymbolTableMaker( errorRecorder ) 
    val typeCreator = new TypeCreator( errorRecorder )
    val classEnvironmentCreator = new ClassEnvironmentCreator( errorRecorder ) 
     
    private def addStandardDecls( decls : DeclList ) = {
        
        val boolTypeDeclNd = new PrimitiveTypeDeclNd(boolFQN)(noCoord)
        val trueInit = ValueInitExpNd(NameExpNd(NameNd(trueFQN)(noCoord))(noCoord))(noCoord)
        val falseInit = ValueInitExpNd(NameExpNd(NameNd(falseFQN)(noCoord))(noCoord))(noCoord)
        val boolTypeNd0 = new NamedTypeNd( NameNd(boolFQN)(noCoord))(noCoord)
        val boolTypeNd1 = new NamedTypeNd( NameNd(boolFQN)(noCoord))(noCoord)
        //TODO for isGhost, ObjDeclNd need to be double checked
        val trueDecl = ObjDeclNd(true,true, PublicAccess, boolTypeNd0, trueInit)("true",noCoord)
        val falseDecl = ObjDeclNd(true,true, PublicAccess, boolTypeNd0, falseInit)("false",noCoord)
//        val trueGhostConstDecl = ObjDeclNd(true,true, PublicAccess, boolTypeNd0, trueInit)("true",noCoord)
//        val falseGhostConstDecl = ObjDeclNd(true,true, PublicAccess, boolTypeNd1, falseInit)("false",noCoord)
//        val trueConstDecl = ObjDeclNd(false,true, PublicAccess, boolTypeNd0, trueInit)("true",noCoord)
//        val falseConstDecl = ObjDeclNd(false,true, PublicAccess, boolTypeNd1, falseInit)("false",noCoord)
//        val trueObjDecl = ObjDeclNd(false,false, PublicAccess, boolTypeNd0, trueInit)("true",noCoord)
//        val falseObjDecl = ObjDeclNd(true,false, PublicAccess, boolTypeNd1, falseInit)("false",noCoord)
//        val trueGhostObjDecl = ObjDeclNd(true,false, PublicAccess, boolTypeNd0, trueInit)("true",noCoord)
//        val falseGhostObjDecl = ObjDeclNd(true,false, PublicAccess, boolTypeNd1, falseInit)("false",noCoord)
        decls.addDeclaration(boolTypeDeclNd)
        decls.addDeclaration(PrimitiveTypeDeclNd(int8FQN)(noCoord))
        decls.addDeclaration(PrimitiveTypeDeclNd(int16FQN)(noCoord))
        decls.addDeclaration(PrimitiveTypeDeclNd(int32FQN)(noCoord))
        decls.addDeclaration(PrimitiveTypeDeclNd(int64FQN)(noCoord))
        decls.addDeclaration(PrimitiveTypeDeclNd(real16FQN)(noCoord))
        decls.addDeclaration(PrimitiveTypeDeclNd(real32FQN)(noCoord))
        decls.addDeclaration(PrimitiveTypeDeclNd(real64FQN)(noCoord))
        decls.addDeclaration(trueDecl)
        decls.addDeclaration(falseDecl)
//        decls.addDeclaration(trueGhostConstDecl)
//        decls.addDeclaration(falseGhostConstDecl)
//        decls.addDeclaration(trueConstDecl)
//        decls.addDeclaration(falseConstDecl)
//        decls.addDeclaration(trueGhostObjDecl)
//        decls.addDeclaration(falseGhostObjDecl)
//        decls.addDeclaration(trueObjDecl)
//        decls.addDeclaration(falseObjDecl)
    }

          
    def runChecker( decls : DeclList )
    : SymbolTable = {
        addStandardDecls( decls )
        
        // (0) Create a symbol table and also set the fqn field of each declaration.
        val symbolTable = symbolTableMaker.createSymbolTable( decls );
        
        if( errorRecorder.getFatalCount() > 0 ) {
            // Failed to build the symbol table.
            println("Failed to build the symbol table. Fetal Count: ", errorRecorder.getFatalCount())
            errorRecorder.bailOut() }
        
        symbolTable.dump
        
        // (1) Lookup name in the symbol table. Associate each NameNd with a declaration
        val resolver = new Resolver( errorRecorder, symbolTable ) 
        
        resolver.resolve( decls ) 
        
        if( errorRecorder.getFatalCount() > 0 ) {
            // Then there are some IDs that didn't resolve. Better quit gracefully.
            errorRecorder.bailOut() }
        
        // (2) Create types and fill in the "tipe" field of each TypeNd object
        // with the exception of NoTypeNd objects and TypeNds that are right operands
        // of "as" expressions.
        typeCreator.createTypes( decls )
        
        if( errorRecorder.getFatalCount() > 0 ) {
            // Then some types couldn't be created. Better quit gracefully.
            errorRecorder.bailOut() }
        
        //  // (3) Build a representation of each class and interface
        //  val classEnv = classEnvironmentCreator.buildClassEnvironment(decls )
        
        
        if( errorRecorder.getFatalCount() > 0 ) {
            // Then some classes were a problem. Better quit gracefully.
            errorRecorder.bailOut() }
        
        // (4) Check the types and associate each expression node with a type.
        // Also insert inferred conversions.
        
        val typeChecker = new TypeChecker( errorRecorder, typeCreator )
        typeChecker.typeCheck( decls ) 
        
        if( errorRecorder.getFatalCount() > 0 ) {
            // Then there are type errors. Better quit gracefully.
            errorRecorder.bailOut() }
        
        symbolTable
    }
}