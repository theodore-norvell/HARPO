package cBackEnd

import frontEnd._
import frontEnd.AST._
import java.io._
import checker.CheckerTypes._
import contracts.Contracts._
import scala.collection.mutable.HashMap

/**
 * @author nhacnguyen
 */
object ClassCodeGen {
  def apply(classNd: DeclNd, table: HashMap[String, HashMap[String, String]]): String = {
    val ccg = new ClassCodeGen()
    ccg.build(classNd, table)
  }
}

class ClassCodeGen {
  private var paramList = List[String]()
  private var constantList = List[String]()
  private var declCode = ""
  private var constructorCode = ""
  private var debugCode = ""
  private var procCode: String = ""
  private var clsCode: String = ""

  def build(clsNd: DeclNd, table: HashMap[String, HashMap[String, String]]): String = {
    val NM = NameManager
    val nd = clsNd
    clsNd match {
      case ClassDeclNd() => {
        var nameTbl = HashMap[String, String]()
        paramList = List()
        for (param <- clsNd.asInstanceOf[ClassDeclNd].constructorParams) {
          val objType: (String, String) = TypeCodeGen(param.ty)
          check(objType._2 == "")
          paramList = paramList :+ (objType._1 + " " + NM.mkName(param.name))
        }
        //paramList = List(clsNd.asInstanceOf[ClassDeclNd].constructorParams.map(_.name) : _*)
        clsCode = "typedef struct " + NM.mkName(clsNd.name) + "{\nvolatile int procLock ;\n"

        for (sup <- clsNd.asInstanceOf[ClassLike].superTypes) {
          clsCode += NM.mkName(sup.toString()) + " " + NM.mkSuperName(sup.toString()) + ";\n"
          constructorCode += NM.mkClassConstructorName(sup.toString()) + "(&" + NM.mkClassParaName(clsNd.name) + "->" + NM.mkSuperName(sup.toString()) + ");\n"
          nameTbl = nameTbl ++ table(sup.toString())
        }

        for (mem <- clsNd.asInstanceOf[ClassLike].directMembers) {
          mem match {
            case MethodDeclNd(acc: Access, params: List[ParamDeclNd]) =>
              procCode += MethCodeGen(mem, clsNd.name) + "\n"
              constructorCode += "InitProcStruct(&(" + NM.mkClassParaName(clsNd.name) + "->" + NM.mkName(mem.name) + ".ps));\n"
              nameTbl(mem.name) = NM.mkMethodName(mem.name, clsNd.name)
              NM.putParamMap(mem.name, clsNd.name, params)

            case ObjDeclNd(isConst: Boolean, acc: Access, ty: TypeNd, init: InitExpNd) =>
              val objCode = ObjCodeGen(mem)
              clsCode += objCode._1 + ";\n"
              //generates codes for class constructor
              if (init.isInstanceOf[NewInitExpNd]) {
                if (isConst) constantList = constantList :+ ExpCodeGen(init, mem.name, clsNd.name)
                else constructorCode += ExpCodeGen(init, mem.name, clsNd.name)
              } else {
                if (isConst) constantList = constantList :+ "*(" + ty.toString() + "*)&initConst->" + NM.mkName(mem.name) + " = " + ExpCodeGen(init, mem.name)
                else constructorCode += NM.mkClassParaName(clsNd.name) + "->" + NM.mkName(mem.name) + " = " + ExpCodeGen(init, mem.name) + ";\n"
              }

            //add class's field to nameTable, needed only when inheriting
            //nameTbl(mem.name) = NM.mkIntfObjName(mem.name, mClsName)
            case ThreadDeclNd(block: CommandNd) =>
              val threadName = mem.name.replace("#", "")
              val memFuncName = NM.mkName("FuncThread_" + clsNd.name + threadName)
              declCode += "void " + memFuncName + "(Continuation *k);\n"
              constructorCode += "Continuation cont_" + threadName + ";\n"
              constructorCode += "cont_" + threadName + ".context = " + NM.mkClassParaName(clsNd.name) + ";\n"
              constructorCode += "cont_" + threadName + ".fp = &" + memFuncName + ";\n"
              constructorCode += "PutContinuationOnHeap(&cont_" + threadName + ");\n";

              clsCode += resCommandNd(block, nameTbl, clsNd.name)
            case _ => {}
          }
        }
        clsCode += "} " + NM.mkName(clsNd.name) + ";\n"

        paramList = List(NM.mkName(clsNd.name) + " *" + NM.mkClassParaName(clsNd.name)) ++ paramList

        var constStr = ""
        if (!constantList.isEmpty) {
          //constructorCode = NM.mkName(clsNd.name) + " initConst = {" + (constantList mkString ",") + "};\nmemcpy(" + NM.mkClassParaName(clsNd.name) + ",&initConst,sizeof(" + NM.mkName(clsNd.name) + "));\n" + constructorCode
          constStr = NM.mkName(clsNd.name) + "* initConst = malloc(sizeof(" + NM.mkName(clsNd.name) + "));\n"
          constStr += constantList mkString ";\n"
          constStr += ";\nmemcpy(" + NM.mkClassParaName(clsNd.name) + ", initConst, sizeof(" + NM.mkName(clsNd.name) + "));\n"
        }
        constructorCode = constStr + constructorCode

        constructorCode = "void " + NM.mkClassConstructorName(clsNd.name) + "(" + (paramList mkString ",") + "){\n" + constructorCode + debugCode + "}\n"
      }
      case _ => {}
    }
    procCode + "\n" + clsCode + "\n" + declCode + "\n" + constructorCode + "\n"
  }

  private def resCommandNd(cmd: CommandNd, table: HashMap[String, String], className: String): String = {
    val NM = NameManager
    val result: String = cmd match {
      //    case SkipCmdNd()

      case SeqCommandNd(fstCmd: CommandNd, sndCmd: CommandNd) =>
        resCommandNd(fstCmd, table, className) + resCommandNd(sndCmd, table, className)

      case LocalDeclCmdNd(decl: LocalDeclNd) =>
        val objType: (String, String) = TypeCodeGen(decl.ty)
        val objName = NM.mkLocalObjName(decl.name)
        val objInit = ExpCodeGen(decl.init)
        if (decl.isConst) constantList = constantList :+ ("." + objName + " = " + objInit)
        constructorCode += NM.mkClassParaName(className) + "->" + objName + " = " + objInit + ";\n"
        objType._1 + " " + objName + objType._2 + ";\n" + resCommandNd(decl.cmd, table, className)

      //      case AssignmentCmdNd( lhs : Seq[ExpNd], var rhs : Seq[ExpNd])

      //      case CallCmdNd( method : ExpNd, argList : List[ExpNd])

      case IfCmdNd(guard: ExpNd, thenCmd: CommandNd, elseCmd: CommandNd) =>
        resCommandNd(thenCmd, table, className) + resCommandNd(elseCmd, table, className)

      case WhileCmdNd(guard: ExpNd, body: CommandNd) =>
        resCommandNd(body, table, className)

      case ForCmdNd(decl: ForDecl, repetitions: ExpNd, body: CommandNd) =>
        resCommandNd(body, table, className)

      case CoForCmdNd(decl: ForDecl, repetitions: ExpNd, body: CommandNd) =>
        resCommandNd(body, table, className)

      case CoCmdNd(fstCmd: CommandNd, sndCmd: CommandNd) =>
        //TODO: this will only work if there's only a single co in the class, need to fix that
        constructorCode += "InitCounter(2, &" + NM.mkClassParaName(className) + "->" + NM.mkCounterName(className) + ");\n"
        "Counter " + NM.mkCounterName(className) + ";\n" + resCommandNd(fstCmd, table, className) + resCommandNd(sndCmd, table, className)

      case AcceptCmdNd(methodImplementationList: List[MethodImplementationDeclNd]) =>
        var code: String = ""
        for (meth <- methodImplementationList) {
          val name = meth.asInstanceOf[DeclNd].name
          code += table(name) + " " + NM.mkName(name) + ";\n"
        }
        code

      case WithCmdNd(lock: ExpNd, guard: ExpNd, command: CommandNd) =>
        resCommandNd(command, table, className)

      case _ => ""
    }
    result
  }
}