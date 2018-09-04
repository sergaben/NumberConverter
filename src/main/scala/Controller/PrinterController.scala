package Controller
import Model.Tools
import View.Printer
/**
  *
  * @project numberparser
  * @author sergaben on 31/08/2018.
  *
  */

//TODO - Have another tought of how to implement it better
class PrinterController(val numb : Int) {

  // print if the number contains 0
  // print if number is between 11 and 19
  // print all the two digits number
  // add default message when the number is not valid
  val toolsForNumber = new Tools(numb)
  val splittedNumber: List[Int] = toolsForNumber.splitNumberByDigits
  val decisionOnTwoDigit: Int = toolsForNumber.identifyTypeOfTwoDigitNumber
  val printerForNumber = new Printer(numb,splittedNumber,decisionOnTwoDigit)

  def printNumber() : Unit = {
    println(toolsForNumber.getNumericLength)
    toolsForNumber.getNumericLength match {
      case 1 => println(printerForNumber.printOneDigitNumber)
      case 2 => println(printerForNumber.chooseWhichTwoDigitNumberToPrint)
      case 3 => println(printerForNumber.printHundred)
      case 4 | 5 | 6 => println()
      case 7 | 8 | 9 => println()
      case _ => println("No matching number")
    }
  }
}
