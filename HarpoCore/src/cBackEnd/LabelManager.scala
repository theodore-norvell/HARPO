package cBackEnd

class LabelManager {
  var curFreeLabelNum = 0
  def newLabel: String = {
    curFreeLabelNum = curFreeLabelNum + 1
    return "LABEL" + (curFreeLabelNum-1).toString
  }

}