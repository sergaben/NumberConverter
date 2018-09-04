package View

import Util._

/**
  *
  * @project numberparser
  * @author sergaben on 31/08/2018.
  *
  */

//TODO - have another thought on how designing this class

// This class only prints the numbers. SRP

class Printer(val number: Int, val splittedNumber : List[Int], decision: Int) {

  def printOneDigitNumber : String = {
      From1to9.values.filter(_.id == splittedNumber.head - 1).head.toString
  }

  def printTwoDigitNumberThatContainsZero : String = {
    From10to90.values.filter(_.id == splittedNumber.head.toInt -1).head.toString
  }

  def printTwoDigitNumberBet11and19 : String = {

//      val numberInDigits: List[Int] = splitNumberByDigits(number) // head (9,0) -> get 9

     // substitute everything for pattern matching
//      if(containsZeroDigit(number)){
//
//      }
//      else if(number >= 11 && number <= 19){
        From11to19.values.filter(_.id == splittedNumber(1) -1).head.toString
//      }
//      else{
//
//      }
  }

  def printTwoDigitNumberWithNoEspecialReq : String = {
    val indexUnitNumber = splittedNumber(1)
    val tenthNumberAsText = From10to90.values.filter(_.id == splittedNumber.head -1).head.toString
    val unitNumberAsText = printOneDigitNumber(indexUnitNumber)
    tenthNumberAsText + ConjunctionAndOthers.space + unitNumberAsText
  }

  def chooseWhichTwoDigitNumberToPrint:String = {
    decision match {
      case 1 => printTwoDigitNumberThatContainsZero
      case 2 => printTwoDigitNumberBet11and19
      case 3 => printTwoDigitNumberWithNoEspecialReq
    }
  }

  def printHundred : String = {

      val restOfNumberAsInt = splittedNumber.tail.mkString.toInt
      println(splittedNumber.head)
      val firstValueAsText = printOneDigitNumber + ConjunctionAndOthers.space + Hundred.hundred

      val restOfNumberAsText = ConjunctionAndOthers.space + ConjunctionAndOthers.and + ConjunctionAndOthers.space + chooseWhichTwoDigitNumberToPrint

      val completeNumberAsText = firstValueAsText + restOfNumberAsText

      completeNumberAsText
  }

//  def printThousand(number : Int) : String = {
//    if(getNumericLength(number) == 4){
//
//      val numberInDigits : List[Int] = splitNumberByDigits(number)
//
//      val firstNumber = parseDecimalOneDigitNumberToText(numberInDigits.head) + ConjunctionAndOthers.space + Thousand.thousand + ConjunctionAndOthers.space + ConjunctionAndOthers.and
//
//      val restOfNumber = ConjunctionAndOthers.space + printHundred(numberInDigits.tail.mkString.toInt)
//
//      firstNumber + restOfNumber
//
//    }else if(getNumericLength(number) == 5){
//
//      val splittedNumber : List[Int] = splitNumberByDigits(number)
//      val splittedList = splittedNumber.splitAt(2)
//
//      val firstTwoNumbers = parseDecimalTwoDigitNumberToText(splittedList._1.mkString.toInt) + ConjunctionAndOthers.space + Thousand.thousand + ConjunctionAndOthers.space + ConjunctionAndOthers.and
//      val restOfNumber = ConjunctionAndOthers.space + printHundred(splittedList._2.mkString.toInt)
//
//      firstTwoNumbers + restOfNumber
//
//    }else if(getNumericLength(number) == 6){
//
//      val splittedNumber : List[Int] = splitNumberByDigits(number)
//      val splittedList = splittedNumber.splitAt(3)
//
//      val firstThreeNumbers = printHundred(splittedList._1.mkString.toInt) + ConjunctionAndOthers.space +  Thousand.thousand + ConjunctionAndOthers.comma
//      val restOfNumber = ConjunctionAndOthers.space + printHundred(splittedList._2.mkString.toInt)
//
//      firstThreeNumbers + restOfNumber
//
//    }else{
//      "Number has more or less than 4,5,6 digits or is not a valid number, use another method for this purpose"
//    }
//  }

//  def printMillion(number : Int) : String = {
//    if(getNumericLength(number) == 7){
//      val numberInDigits : List[Int] = splitNumberByDigits(number)
//
//      val firstNumber : String = parseDecimalOneDigitNumberToText(numberInDigits.head) + ConjunctionAndOthers.space + Million.million + ConjunctionAndOthers.comma + ConjunctionAndOthers.space
//
//      val restOfNumber : String = printThousand(numberInDigits.tail.mkString.toInt)
//
//      firstNumber + restOfNumber
//
//    } else if (getNumericLength(number) == 8){
//      val numberInDigits : List[Int] = splitNumberByDigits(number)
//
//      val splittedList  = numberInDigits.splitAt(2)
//      val firstNumber : String = parseDecimalTwoDigitNumberToText(splittedList._1.mkString.toInt) + ConjunctionAndOthers.space + Million.million + ConjunctionAndOthers.comma + ConjunctionAndOthers.space
//
//      val restOfNumber : String = printThousand(splittedList._2.mkString.toInt)
//
//      firstNumber + restOfNumber
//      // two million value
//    } else if(getNumericLength(number) == 9) {
//
//      val splittedNumber : List[Int] = splitNumberByDigits(number)
//      val splittedList  = splittedNumber.splitAt(3)
//
//      val firstThreeNumbers : String = printHundred(splittedList._1.mkString.toInt) + ConjunctionAndOthers.space + Million.million + ConjunctionAndOthers.comma + ConjunctionAndOthers.space
//      val restOfNumber : String = printThousand(splittedList._2.mkString.toInt)
//
//      firstThreeNumbers + restOfNumber
//    }else {
//      "Number has more or less than 7,8,9 digits or is not a valid number, use another method for this purpose"
//    }
//  }

}
