/* Generated By:JavaCC: Do not edit this line. HarpoParserConstants.java */
package parser;


/**
 * Token literal values and constants.
 * Generated by org.javacc.parser.OtherFilesGen#start()
 */
public interface HarpoParserConstants {

  /** End of File. */
  int EOF = 0;
  /** RegularExpression Id. */
  int SINGLE_LINE_COMMENT = 1;
  /** RegularExpression Id. */
  int IMPLIES = 18;
  /** RegularExpression Id. */
  int FOLLOWS_FROM = 19;
  /** RegularExpression Id. */
  int BICOND = 20;
  /** RegularExpression Id. */
  int OR = 21;
  /** RegularExpression Id. */
  int AND = 22;
  /** RegularExpression Id. */
  int NOT = 23;
  /** RegularExpression Id. */
  int NE = 25;
  /** RegularExpression Id. */
  int LE = 27;
  /** RegularExpression Id. */
  int GE = 28;
  /** RegularExpression Id. */
  int PRIME = 41;
  /** RegularExpression Id. */
  int BOOL_LITERAL = 86;
  /** RegularExpression Id. */
  int NAME = 87;
  /** RegularExpression Id. */
  int DEC_LITERAL = 88;
  /** RegularExpression Id. */
  int BIN_LITERAL = 89;
  /** RegularExpression Id. */
  int OCT_LITERAL = 90;
  /** RegularExpression Id. */
  int HEX_LITERAL = 91;
  /** RegularExpression Id. */
  int HEX_CHAR = 92;
  /** RegularExpression Id. */
  int REAL_LITERAL = 93;
  /** RegularExpression Id. */
  int EXP = 94;
  /** RegularExpression Id. */
  int UNEXPECTED_CHARACTER = 95;

  /** Lexical state. */
  int DEFAULT = 0;
  /** Lexical state. */
  int IN_MULTI_LINE_COMMENT = 1;
  /** Lexical state. */
  int FINISH_MULTI_LINE_COMMENT = 2;

  /** Literal token values. */
  String[] tokenImage = {
    "<EOF>",
    "<SINGLE_LINE_COMMENT>",
    "\"(*\"",
    "\"(*\"",
    "<token of kind 4>",
    "\"*)\"",
    "\"\"",
    "\" \"",
    "\"\\r\"",
    "\"\\t\"",
    "\"\\n\"",
    "\"(\"",
    "\")\"",
    "\"[\"",
    "\"]\"",
    "\"{\"",
    "\"}\"",
    "\":\"",
    "<IMPLIES>",
    "<FOLLOWS_FROM>",
    "<BICOND>",
    "<OR>",
    "<AND>",
    "<NOT>",
    "\"=\"",
    "<NE>",
    "\"<\"",
    "<LE>",
    "<GE>",
    "\">\"",
    "\"+\"",
    "\"-\"",
    "\"*\"",
    "\"/\"",
    "\";\"",
    "\",\"",
    "\":=\"",
    "\"|\"",
    "\"||\"",
    "\".\"",
    "\"@\"",
    "<PRIME>",
    "\"accept\"",
    "\"acc\"",
    "\"as\"",
    "\"assert\"",
    "\"assume\"",
    "\"borrows\"",
    "\"canRead\"",
    "\"canWrite\"",
    "\"class\"",
    "\"claim\"",
    "\"co\"",
    "\"const\"",
    "\"do\"",
    "\"div\"",
    "\"else\"",
    "\"extends\"",
    "\"for\"",
    "\"gives\"",
    "\"ghost\"",
    "\"if\"",
    "\"implements\"",
    "\"in\"",
    "\"interface\"",
    "\"invariant\"",
    "\"mod\"",
    "\"new\"",
    "\"obj\"",
    "\"out\"",
    "\"permission\"",
    "\"pre\"",
    "\"post\"",
    "\"private\"",
    "\"proc\"",
    "\"public\"",
    "\"takes\"",
    "\"then\"",
    "\"thread\"",
    "\"type\"",
    "\"when\"",
    "\"while\"",
    "\"with\"",
    "\"this\"",
    "\"length\"",
    "\"forall\"",
    "<BOOL_LITERAL>",
    "<NAME>",
    "<DEC_LITERAL>",
    "<BIN_LITERAL>",
    "<OCT_LITERAL>",
    "<HEX_LITERAL>",
    "<HEX_CHAR>",
    "<REAL_LITERAL>",
    "<EXP>",
    "<UNEXPECTED_CHARACTER>",
  };

}
