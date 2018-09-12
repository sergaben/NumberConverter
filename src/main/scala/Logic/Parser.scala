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

  private def genericLogicForThousand(firstNumber:String,splitNumber:(List[Int],List[Int])): String ={
    if (hasMixedZerosAndNumbers(splitNumber)) { // find thousand numbers with middle zeros
      val restOfNumber = parseHundredNumberToText(splitNumber._2)
      concatenateNumbersAsCompleteString(firstNumber,restOfNumber,Thousand.thousand,ConjunctionAndOthers.and)
    }
    else if (containsAllZerosAfterFirstDigit(splitNumber)) { // all zeros after first number
      concatenateNumbersAsCompleteString(firstNumber,"",Thousand.thousand)
    }
    else {
      val restOfNumber = parseHundredNumberToText(splitNumber._2)
      concatenateNumbersAsCompleteString(firstNumber,restOfNumber,Thousand.thousand,ConjunctionAndOthers.comma)
    }
  }

  private def genericLogicForMillion(firstNumber:String,splitNumber:(List[Int],List[Int])): String ={
    if (containsAllZerosAfterFirstDigit(splitNumber)) {
      concatenateNumbersAsCompleteString(firstNumber,"",Million.million)
    }
    else if (hasMixedZerosAndNumbers(splitNumber)) { // fix duplication of this if statement
      val fiveDigitsOfNumber = splitNumber._2.tail

      getMillionWithZerosInMiddle(firstNumber,fiveDigitsOfNumber)

    }
    else {
      val restOfNumber: String = parseThousandNumberToText(splitNumber._2.length, splitNumber._2)
      concatenateNumbersAsCompleteString(firstNumber,restOfNumber,Million.million,ConjunctionAndOthers.comma)
    }
  }

  private def genericMillionNumber(number:List[Int],splitBy:Int): String ={
    val splitNumber = number.splitAt(splitBy)

    splitBy match {
      case 1 => val firstOneDigitOfNumber = parseDecimalOneDigitNumberToText(splitNumber._1)
        genericLogicForMillion(firstOneDigitOfNumber,splitNumber)
      case 2 => val firstTwoDigitsOfNumber = parseDecimalTwoDigitNumberToText(splitNumber._1)
        genericLogicForMillion(firstTwoDigitsOfNumber,splitNumber)
      case 3 =>val firstThreeDigitsOfNumber = parseHundredNumberToText(splitNumber._1)
        genericLogicForMillion(firstThreeDigitsOfNumber,splitNumber)
    }
  }

  private def genericThousandNumber(number:List[Int],splitBy:Int): String ={
    val splitNumber = number.splitAt(splitBy)

    splitBy match {
      case 1 => val firstOneDigitOfNumber = parseDecimalOneDigitNumberToText(splitNumber._1)
        genericLogicForThousand(firstOneDigitOfNumber,splitNumber)
      case 2 => val firstTwoDigitsOfNumber = parseDecimalTwoDigitNumberToText(splitNumber._1)
        genericLogicForThousand(firstTwoDigitsOfNumber,splitNumber)
      case 3 =>val firstThreeDigitsOfNumber = parseHundredNumberToText(splitNumber._1)
        genericLogicForThousand(firstThreeDigitsOfNumber,splitNumber)
    }

  }

  // ****************************************************************************

  private def parseDecimalOneDigitNumberToText(number: List[Int]): String = {
      From1to9.values.filter(_.id == number.head - 1).head.toString
  }

  private def parseDecimalTwoDigitNumberToText(number: List[Int]): String = {

    val lastPartOfNumberAsInt = number.tail.head // get the last part of list 'number' and get the value of that part using head
    val firstPartOfNumberAsInt = number.head // get the first part of list 'number'
    val numberAsInt = number.mkString.toInt
    val indexUnitNumber = List(number(1))

      if (lastPartOfNumberAsInt == 0) { // for two digits number ending in 0
        From10to90.values.filter(_.id == number.head - 1).head.toString
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

      genericThousandNumber(number,1)

    }
    else if (length == 5) {

      genericThousandNumber(number,2)

    }
    else  // 6 digits

      genericThousandNumber(number,3)

  }

  private def parseMillionNumberToText(length : Int, number: List[Int]): String = {
    if (length == 7) {

      genericMillionNumber(number,1)

    }
    else if (length == 8) {
      genericMillionNumber(number,2)
    }
    else genericMillionNumber(number,3)
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
