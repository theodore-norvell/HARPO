package frontEnd

trait ErrorRecorder extends ErrorReport {
    
    def reportFatal( mess : String, coord : AST.Coord )
    
    def reportWarning( mess : String, coord : AST.Coord ) 
    
    def reportVerification( mess : String, coord : AST.Coord )
    
    def checkFatal( guard : Boolean, mess : String, coord : AST.Coord ) = {
        if( guard ) true 
        else { reportFatal( mess, coord) ; false }
    } 
    
    def checkWarn( guard : Boolean, mess : String, coord : AST.Coord ) = {
        if( guard ) true 
        else { reportWarning( mess, coord) ; false }
    }

    def bailOut() : Nothing
}
