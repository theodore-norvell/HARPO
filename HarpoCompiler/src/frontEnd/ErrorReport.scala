package frontEnd

trait ErrorReport {
    
    def getTotalErrorCount() : Int 
    
    def getErrorText( i : Int ) : String
    
    def getErrorCoord( i : Int ) : AST.Coord
    
    def getFatalCount() : Int 
    
    def getFatalText( i : Int ) : String
    
    def getFatalCoord( i : Int ) : AST.Coord
    
    def getWarningCount() : Int
    
    def getWarningText( i : Int ) : String
    
    def getWarningCoord( i : Int ) : AST.Coord 
    
    def getVerificationCount() : Int
    
    def getVerificationText( i : Int ) : String
    
    def getVerificationCoord( i : Int ) : AST.Coord 
    
    def printErrors( stream : java.io.PrintStream ) 
}
