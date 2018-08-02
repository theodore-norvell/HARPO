package executive
import java.io.PrintWriter;
import java.io.File;

class PrintDeclList(val dl: frontEnd.AST.DeclList) {
  
dl.pp.format(50, new PrintWriter(new File("DecList.txt" )))
dl.pp.:/:("DecList.txt");


//val json = JacksMapper.writeValueAsString[frontEnd.AST.DeclList](masterDeclList);
//
//val pw = new PrintWriter(new File("DeclList.text" ))
//pw.write()
//pw.close
  
}