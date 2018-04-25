package cBackEnd

/**
 * @author nhacnguyen
 */
object NameManager {
  var nameMap = scala.collection.mutable.Map[String, String]()
  var paramMap = scala.collection.mutable.Map[String, (String, List[frontEnd.AST.ParamDeclNd])]()
  
  def clear() = {
    nameMap = scala.collection.mutable.Map[String, String]()
    paramMap = scala.collection.mutable.Map[String, (String, List[frontEnd.AST.ParamDeclNd])]()
  }
  
  def mkName(name : String) : String = {
    // FIXME: don't like handling true/false here, find a better place
    if (name.equals("true") || name.equals("false")) return name
    val v1 = nameMap.get(name)
    v1 match {
      case Some(value) => return v1.get
      case None => {
        var i = 0
        var newName : String = name.replace("_", "_u")
        newName = newName.replace("$", "_s")
        newName = newName.replace("'", "_q")
        newName += "_"
        
        if (newName.length() > 31) newName = newName.substring(0, 30) + "_"
        val v2 = nameMap.find(_._2 == newName)
        v2 match {
          case Some(v2) => {
            if (newName.length() > 21) newName = newName.substring(0, 20) + "_"
            val len = newName.length()
            do {
              newName = newName.substring(0, len) + to10DigitString(i)
              i += 1
            } while (nameMap.find(_._2 == newName) != None)
            nameMap(name) = newName
            return newName
          }
          case None => {
            nameMap(name) = newName
            return newName
          }
        }
      }
    }
  }
  
  def mkPointerName(name : String) = mkName("p_" + name)  
  def mkClassConstructorName(name : String) = mkName("c_" + name)  
  def mkContinuationName(name : String) = mkName("cont_" + name)  
  def mkLocalObjName(name : String) = mkName("l_" + name)  
  def mkClassParaName(clsName : String) : String = mkName("init" + clsName)
  // Is there a reason for MethodName and IntfObjName being different?
  // From looking at code in the generation classes they both seem to just handle members.
  // Test this later.
  def mkMethodName(name : String, parent : String) = mkName(parent + "_" + name)
  def mkIntfObjName(name : String, parent : String) = mkName(parent + "_" + name)
  def mkSuperName(name : String) = mkName("sup_" + name.toLowerCase())
  def mkCounterName(name : String) = mkName("cnt_" + name)
  def putParamMap(name: String, className: String, params: List[frontEnd.AST.ParamDeclNd]) =
  {
    if (!paramMap.contains(name)) {
      paramMap(name) = (className, params)
    }
  }
  
  def getParamMap(name: String) : (String, List[frontEnd.AST.ParamDeclNd]) = {
    if (paramMap.contains(name)) {
      return paramMap(name)
    }
    return null
  }
  
  // TODO: make sure this function is actually valid
  // can the same variable name be used in different scopes?
  def getMethodNameByParam(argName: String) : String = {
    for ((key, value) <- paramMap) {
      for (arg <- value._2) {
        if (arg.name.equals(argName)) return key
      }
    }
    return ""
  }
  
  private def to10DigitString(i : Int) : String = {
    var s = i.toString()
    while (s.length() < 10) {
      s = "0" + s
    }
    return s
  } 
}