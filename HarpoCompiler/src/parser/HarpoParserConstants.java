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
  int NAME = 76;
  /** RegularExpression Id. */
  int DEC_LITERAL = 77;
  /** RegularExpression Id. */
  int BIN_LITERAL = 78;
  /** RegularExpression Id. */
  int OCT_LITERAL = 79;
  /** RegularExpression Id. */
  int HEX_LITERAL = 80;
  /** RegularExpression Id. */
  int HEX_CHAR = 81;
  /** RegularExpression Id. */
  int REAL_LITERAL = 82;
  /** RegularExpression Id. */
  int EXP = 83;
  /** RegularExpression Id. */
  int UNEXPECTED_CHARACTER = 84;

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
    "\"accept\"",
    "\"as\"",
    "\"class\"",
    "\"co\"",
    "\"const\"",
    "\"do\"",
    "\"div\"",
    "\"else\"",
    "\"extends\"",
    "\"for\"",
    "\"if\"",
    "\"implements\"",
    "\"in\"",
    "\"interface\"",
    "\"mod\"",
    "\"new\"",
    "\"obj\"",
    "\"out\"",
    "\"private\"",
    "\"proc\"",
    "\"public\"",
    "\"then\"",
    "\"thread\"",
    "\"type\"",
    "\"when\"",
    "\"while\"",
    "\"with\"",
    "\"assert\"",
    "\"assume\"",
    "\"ghost\"",
    "\"pre\"",
    "\"post\"",
    "\"takes\"",
    "\"gives\"",
    "\"borrows\"",
    "\"claim\"",
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
