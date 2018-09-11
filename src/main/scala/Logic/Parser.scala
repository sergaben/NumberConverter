package Logic

import Util._

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */

//TODO - change how the logic is structured, try to optimise it

class Parser() {

  private def isNegNum(number: Int): Boolean = if (number < 0) true else false

  private def isOverBillion(number: Int): Boolean = number > 999999999

  private def splitNumberByDigits(number: Int): List[Int] = number.toString.map(_.asDigit).toList

  private def getNumericLength(number: Int): Int ={
    if(isNegNum(number) || isOverBillion(number))
      -1
    else if(splitNumberByDigits(number).length == 1 && splitNumberByDigits(number).head == 0 ){
      -1
    }
    else {
      splitNumberByDigits(number).length
    }
  }

  private def containsAllZerosAfterFirstDigit(splitNumber: (List[Int], List[Int])): Boolean = {
    splitNumber._2.forall(_ == 0)
  }

  private def hasMixedZerosAndNumbers(splitNumber: (List[Int], List[Int])): Boolean = {
    splitNumber._2.head == 0 && !containsAllZerosAfterFirstDigit(splitNumber)
  }

  private def concatenateNumbersAsCompleteString(firstPartOfNumber: String, secondPartOfNumber:String ="",
                                                 typeOfNumber:String = "", commaOrAnd:String =""): String ={

    if(commaOrAnd.isEmpty && secondPartOfNumber.isEmpty){
      val completeNumber =  firstPartOfNumber + ConjunctionAndOthers.space + typeOfNumber

      completeNumber
    }
    else if(commaOrAnd.isEmpty && typeOfNumber.isEmpty){
      val completeNumber = firstPartOfNumber + ConjunctionAndOthers.space + secondPartOfNumber
      completeNumber
    }
    else{
      commaOrAnd match {
        case ConjunctionAndOthers.comma => val completeNumber = firstPartOfNumber + ConjunctionAndOthers.space + typeOfNumber + commaOrAnd + ConjunctionAndOthers.space + secondPartOfNumber
          completeNumber
        case ConjunctionAndOthers.and => val completeNumber = firstPartOfNumber + ConjunctionAndOthers.space + typeOfNumber + ConjunctionAndOthers.space + commaOrAnd + ConjunctionAndOthers.space + secondPartOfNumber
          completeNumber
      }
    }

  }

  def getMillionWithZerosInMiddle(firstPartOfNumber:String,number: List[Int]): String ={

    if(number.head == 0) getMillionWithZerosInMiddle(firstPartOfNumber,number.tail)
    else{
      val secondPartOfNumber = printNumberAsText(number.mkString.toInt)
      concatenateNumbersAsCompleteString(firstPartOfNumber,secondPartOfNumber,Million.million,ConjunctionAndOthers.comma)
    }
  }

  // ***** helpers method for Thousand and million functions ***************

  private def getThousandWithFourDigits(number: List[Int]): String = {
    val splitNumber = number.splitAt(1)
    val firstPartOfNumber = parseDecimalOneDigitNumberToText(List(number.head))


    if (containsAllZerosAfterFirstDigit(splitNumber)) { // all zeros after first number
      concatenateNumbersAsCompleteString(firstPartOfNumber,"",Thousand.thousand)
    }
    else {
      val restOfNumber = parseHundredNumberToText(number.tail)
      concatenateNumbersAsCompleteString(firstPartOfNumber,restOfNumber,Thousand.thousand,ConjunctionAndOthers.and)
    }
  }

  private def getThousandWithFiveDigits(number: List[Int]): String = {
    val splitList = number.splitAt(2)
//    val numberWithOneDigit = parseDecimalOneDigitNumberToText(List(number.head))
    if (containsAllZerosAfterFirstDigit(splitList)) { // find whole thousand numbers

      val numberWithTwoDigits = parseDecimalTwoDigitNumberToText(splitList._1)
      concatenateNumbersAsCompleteString(numberWithTwoDigits,"",Thousand.thousand)

    }
    else if (hasMixedZerosAndNumbers(splitList)) { // find thousand numbers with middle zeros
      val restOfNumber = parseHundredNumberToText(splitList._2)
      val numberWithTwoDigits = parseDecimalTwoDigitNumberToText(splitList._1)
      concatenateNumbersAsCompleteString(numberWithTwoDigits,restOfNumber,Thousand.thousand,ConjunctionAndOthers.and)

    }
    else { // for a common number without zeros in the middle of the number
      val restOfNumber = parseHundredNumberToText(splitList._2)
      val numberWithTwoDigits = parseDecimalTwoDigitNumberToText(splitList._1)
      concatenateNumbersAsCompleteString(numberWithTwoDigits,restOfNumber,Thousand.thousand,ConjunctionAndOthers.comma)

    }
  }

  private def getThousandWithSixDigits(number: List[Int]): String = {
    val splitNumber = number.splitAt(3)

    if (hasMixedZerosAndNumbers(splitNumber)) { // thousand hundred with zeros in between the number
      val restOfNumber = parseHundredNumberToText(splitNumber._2)
      val firstThreeDigitsOfNumber = parseHundredNumberToText(splitNumber._1)
      concatenateNumbersAsCompleteString(firstThreeDigitsOfNumber,restOfNumber,Thousand.thousand,ConjunctionAndOthers.and)
    }
    else if (containsAllZerosAfterFirstDigit(splitNumber)) { // thousand hundred numbers all with trailing zeros
      val firstThreeDigitsOfNumber = parseHundredNumberToText(splitNumber._1)
      concatenateNumbersAsCompleteString(firstThreeDigitsOfNumber,"",Thousand.thousand)
    }
    else {
      val restOfNumber = parseHundredNumberToText(splitNumber._2)
      val firstThreeDigitsOfNumber = parseHundredNumberToText(splitNumber._1)
      concatenateNumbersAsCompleteString(firstThreeDigitsOfNumber,restOfNumber,Thousand.thousand,ConjunctionAndOthers.comma)
    }
  }

  private def getMillionWithSevenDigits(number: List[Int]): String = {
    val splitNumber = number.splitAt(1)
    val millionOneDigits = parseDecimalOneDigitNumberToText(List(number.head))

    if (containsAllZerosAfterFirstDigit(splitNumber)) {
      concatenateNumbersAsCompleteString(millionOneDigits,"",Million.million)
    }
    else if (hasMixedZerosAndNumbers(splitNumber)) { // fix duplication of this if statement
      val fiveDigitsOfNumber = splitNumber._2.tail

      getMillionWithZerosInMiddle(millionOneDigits,fiveDigitsOfNumber)

    }
    else {
      val restOfNumber: String = parseThousandNumberToText(number.tail.length, number.tail)
      concatenateNumbersAsCompleteString(millionOneDigits,restOfNumber,Million.million,ConjunctionAndOthers.comma)
    }
  }

  private def getMillionWithEightDigits(number: List[Int]): String = {
    val splitNumber = number.splitAt(2)
    val millionTwoDigits = parseDecimalTwoDigitNumberToText(splitNumber._1)
    val restOfNumber: String = parseThousandNumberToText(splitNumber._2.length, splitNumber._2)

    if (containsAllZerosAfterFirstDigit(splitNumber)) {
      concatenateNumbersAsCompleteString(millionTwoDigits,"",Million.million)
    }
    else if (hasMixedZerosAndNumbers(splitNumber)) { // fix duplication of code in this if statement
      val fiveDigitsOfNumber = splitNumber._2.tail

      getMillionWithZerosInMiddle(millionTwoDigits,fiveDigitsOfNumber)
    }
    else {
      concatenateNumbersAsCompleteString(millionTwoDigits,restOfNumber,Million.million,ConjunctionAndOthers.comma)
    }
  }

  private def getMillionWithNineDigits(number: List[Int]): String = {

    val splittedNumber = number.splitAt(3)
    val millionWithThreeDigits = parseHundredNumberToText(splittedNumber._1)

    if (containsAllZerosAfterFirstDigit(splittedNumber)) {
      concatenateNumbersAsCompleteString(millionWithThreeDigits, "", Million.million)
    }
    else if (hasMixedZerosAndNumbers(splittedNumber)) {
      val fiveDigitsOfNumber = splittedNumber._2.tail
      getMillionWithZerosInMiddle(millionWithThreeDigits,fiveDigitsOfNumber)
    }
    else {
      val restOfNumber: String = parseThousandNumberToText(splittedNumber._2.length, splittedNumber._2)
      concatenateNumbersAsCompleteString(millionWithThreeDigits,restOfNumber,Million.million,ConjunctionAndOthers.comma)
    }

  }

  // ***********************************************************************

  private def parseDecimalOneDigitNumberToText(number: List[Int]): String = {
      From1to9.values.filter(_.id == number.head - 1).head.toString
  }

  private def parseDecimalTwoDigitNumberToText(number: List[Int]): String = {

    val lastPartOfNumberAsInt = number.tail.head // get the last part of list 'number' and get the value of that part using head
    val firstPartOfNumberAsInt = number.head // get the first part of list 'number'
    val numberAsInt = number.mkString.toInt
    val indexUnitNumber = List(number(1))

      if (lastPartOfNumberAsInt == 0) { // for two digits number ending in 0
        From10to90.values.filter(_.id == number.head.toInt - 1).head.toString
      }
      else if(firstPartOfNumberAsInt == 0){ // for two digits number where the first number is a zero
        parseDecimalOneDigitNumberToText(List(lastPartOfNumberAsInt))
      }
      else if (numberAsInt >= 11 && numberAsInt <= 19) {
        From11to19.values.filter(_.id == number(1) - 1).head.toString
      }
      else{
        val unitNumberAsText = parseDecimalOneDigitNumberToText(indexUnitNumber)
        val tenthNumberAsText = From10to90.values.filter(_.id == number.head - 1).head.toString
        concatenateNumbersAsCompleteString(tenthNumberAsText,unitNumberAsText)
      }

  }

  private def parseHundredNumberToText(number: List[Int]): String = {

     if(number.head == 0){
       val splittedNumber = number.splitAt(0)
       if(splittedNumber._2.head != 0){ // if first digit of a two digit number is not 0, it means is a two digit number
         parseDecimalTwoDigitNumberToText(splittedNumber._2) //
       }else{
         parseDecimalTwoDigitNumberToText(splittedNumber._2.tail)
       }
     }
     else if(number.tail.forall(_ == 0)){ // to display just hundred of any number such as 100, 500 , 600
       val firstNumberAsText = parseDecimalOneDigitNumberToText(List(number.head))
       concatenateNumbersAsCompleteString(firstNumberAsText,"",Hundred.hundred)
     }
     else{
       val firstNumberAsText = parseDecimalOneDigitNumberToText(List(number.head))
       val restOfNumber = parseDecimalTwoDigitNumberToText(number.tail)
       concatenateNumbersAsCompleteString(firstNumberAsText,restOfNumber,Hundred.hundred,ConjunctionAndOthers.and)
     }
  }

  private def parseThousandNumberToText(length:Int, number: List[Int]): String = {
    if (length == 4) {
      getThousandWithFourDigits(number)
    }
    else if (length == 5) {

      getThousandWithFiveDigits(number)

    }
    else  // 6 digits
      getThousandWithSixDigits(number)

  }

  private def parseMillionNumberToText(length : Int, number: List[Int]): String = {
    if (length == 7) {
      getMillionWithSevenDigits(number)
    }
    else if (length == 8) {
      getMillionWithEightDigits(number)
    }
    else getMillionWithNineDigits(number)
  }

  def printNumberAsText(number: Int): String = {
    val numericLength = getNumericLength(number)
    val numberAsList = splitNumberByDigits(number)
    numericLength match {
            case 1 => parseDecimalOneDigitNumberToText(numberAsList)
            case 2 => parseDecimalTwoDigitNumberToText(numberAsList)
            case 3 => parseHundredNumberToText(numberAsList)
            case n if n > 0 && n < 7 => parseThousandNumberToText(n,numberAsList)
            case m if m > 0 && m < 10 =>parseMillionNumberToText(m,numberAsList)
            case _ => "Not a valid number, a valid number is one between 0 and 999,999,999"
    }
  }

}
