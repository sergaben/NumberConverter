package Logic

import Util._

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */

//TODO - finish Parser - build recognition of numbers into words
class NumberParser(){

  def isNegNum(number : Int) : Boolean = if(number<0) true else false

  def isOverBillion(number : Int) : Boolean = number > 999999999

  def getNumericLength(number : Int): Int = if(!isNegNum(number) && !isOverBillion(number)) BigDecimal(number).precision else -1

  def splitNumberByDigits(number : Int): List[Int] = if(getNumericLength(number) != 1) number.toString.map(_.asDigit).toList else List()

  def containsZeroDigit(number : Int) : Boolean = if(splitNumberByDigits(number).contains(0)) true else  false

  def parseDecimalOneDigitNumberToText(number : Int) : String = {
    if(getNumericLength(number) == 1) {
      From1to9.values.filter(_.id == number - 1).head.toString
    }else {
      "Number has two digits or is not a valid number, use another method for this purpose"
    }
  }

  def parseDecimalTwoDigitNumberToText(number : Int) : String = {
    if(getNumericLength(number) == 2) {
      val numberInDigits: List[Int] = splitNumberByDigits(number) // head (9,0) -> get 9
      if(containsZeroDigit(number)){
        From10to90.values.filter(_.id == numberInDigits.head.toInt -1).head.toString
      }
      else if(number >= 11 && number <= 19){
        From11to19.values.filter(_.id == numberInDigits(1) -1).head.toString
      }
      else{
        val indexUnitNumber = numberInDigits(1)
        val tenthNumberAsText = From10to90.values.filter(_.id == numberInDigits.head -1).head.toString
        val unitNumberAsText = parseDecimalOneDigitNumberToText(indexUnitNumber)

        tenthNumberAsText + ConjunctionAndOthers.space + unitNumberAsText
      }
    }else {
      "Number has more or less than 2 digits or is not a valid number, use another method for this purpose"
    }
  }

  def parseHundredNumberToText(number : Int) : String = {
    if(getNumericLength(number) == 3) {
      val numberInDigits : List[Int] = splitNumberByDigits(number)

      val restOfNumberAsInt = numberInDigits.tail.mkString.toInt

      val firstValueAsText = parseDecimalOneDigitNumberToText(numberInDigits.head) + ConjunctionAndOthers.space + Hundred.hundred
      val restOfNumberAsText = ConjunctionAndOthers.space + ConjunctionAndOthers.and + ConjunctionAndOthers.space + parseDecimalTwoDigitNumberToText(restOfNumberAsInt)

      val completeNumberAsText = firstValueAsText + restOfNumberAsText

      completeNumberAsText
    }else {
      "Number has more or less than 3 digits or is not a valid number, use another method for this purpose"
    }
  }

  def parseThousandNumberToText(number : Int) : String = {
    if(getNumericLength(number) == 4){

      val numberInDigits : List[Int] = splitNumberByDigits(number)

      val firstNumber = parseDecimalOneDigitNumberToText(numberInDigits.head) + ConjunctionAndOthers.space + Thousand.thousand + ConjunctionAndOthers.space + ConjunctionAndOthers.and

      val restOfNumber = ConjunctionAndOthers.space + parseHundredNumberToText(numberInDigits.tail.mkString.toInt)

      firstNumber + restOfNumber

    }else if(getNumericLength(number) == 5){

      val splittedNumber : List[Int] = splitNumberByDigits(number)
      val splittedList = splittedNumber.splitAt(2)

      val firstTwoNumbers = parseDecimalTwoDigitNumberToText(splittedList._1.mkString.toInt) + ConjunctionAndOthers.space + Thousand.thousand+ ConjunctionAndOthers.space
      val restOfNumber = ConjunctionAndOthers.and + ConjunctionAndOthers.space + parseHundredNumberToText(splittedList._2.mkString.toInt)

      firstTwoNumbers + restOfNumber

    }else if(getNumericLength(number) == 6){

      val splittedNumber : List[Int] = splitNumberByDigits(number)
      val splittedList = splittedNumber.splitAt(3)

      val firstThreeNumbers = parseHundredNumberToText(splittedList._1.mkString.toInt) + ConjunctionAndOthers.space +  Thousand.thousand + ConjunctionAndOthers.comma
      val restOfNumber = ConjunctionAndOthers.space + parseHundredNumberToText(splittedList._2.mkString.toInt)

      firstThreeNumbers + restOfNumber

    }else{
      "Number has more or less than 4,5,6 digits or is not a valid number, use another method for this purpose"
    }
  }

  def parseMillionNumberToText(number : Int) : String = {
    if(getNumericLength(number) == 7){
      val numberInDigits : List[Int] = splitNumberByDigits(number)

      val firstNumber : String = parseDecimalOneDigitNumberToText(numberInDigits.head) + ConjunctionAndOthers.space + Million.million + ConjunctionAndOthers.comma + ConjunctionAndOthers.space

      val restOfNumber : String = parseThousandNumberToText(numberInDigits.tail.mkString.toInt)

      firstNumber + restOfNumber

    } else if (getNumericLength(number) == 8){
      val numberInDigits : List[Int] = splitNumberByDigits(number)

      val splittedList  = numberInDigits.splitAt(2)
      println(splittedList._1)
      val firstNumber : String = parseDecimalTwoDigitNumberToText(splittedList._1.mkString.toInt) + ConjunctionAndOthers.space + Million.million + ConjunctionAndOthers.comma + ConjunctionAndOthers.space

      val restOfNumber : String = parseThousandNumberToText(splittedList._2.mkString.toInt)

      firstNumber + restOfNumber
      // two million value
    } else if(getNumericLength(number) == 9) {

      val splittedNumber : List[Int] = splitNumberByDigits(number)
      val splittedList  = splittedNumber.splitAt(3)

      val firstThreeNumbers : String = parseHundredNumberToText(splittedList._1.mkString.toInt) + ConjunctionAndOthers.space + Million.million + ConjunctionAndOthers.comma + ConjunctionAndOthers.space
      val restOfNumber : String = parseThousandNumberToText(splittedList._2.mkString.toInt)

      firstThreeNumbers + restOfNumber
    }else {
      "Number has more or less than 7,8,9 digits or is not a valid number, use another method for this purpose"
    }
  }









}
