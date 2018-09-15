package Logic

import Constants._

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */

//TODO - change how the logic is structured, try to optimise it

class Converter() {

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

  // ***** helpers method for Thousand and million functions ***************

  private def getMillionWithZerosInMiddle(firstPartOfNumber:String,number: List[Int]): String ={

    if(number.head == 0) getMillionWithZerosInMiddle(firstPartOfNumber,number.tail)
    else{
      val secondPartOfNumber = printNumberAsText(number.mkString.toInt)
      concatenateNumbersAsCompleteString(firstPartOfNumber,secondPartOfNumber,Million.million,ConjunctionAndOthers.comma)
    }
  }

  private def genericLogicForThousandAndMillion(firstPartOfNumber:String, splitNumber:(List[Int],List[Int]), typeOfNumber:String): String ={

    if (containsAllZerosAfterFirstDigit(splitNumber)) { // all zeros after first number
      concatenateNumbersAsCompleteString(firstPartOfNumber,"",typeOfNumber)
    }
    else if (hasMixedZerosAndNumbers(splitNumber)) { // find thousand numbers with middle zeros
      typeOfNumber match {
        case Thousand.thousand =>  val restOfNumber = convertHundredNumberToText(splitNumber._2)
          concatenateNumbersAsCompleteString(firstPartOfNumber,restOfNumber,Thousand.thousand,ConjunctionAndOthers.and)
        case Million.million =>  val fiveDigitsOfNumber = splitNumber._2.tail
          getMillionWithZerosInMiddle(firstPartOfNumber,fiveDigitsOfNumber)
      }
    }
    else {
      typeOfNumber match {
        case Thousand.thousand => val restOfNumber = convertHundredNumberToText(splitNumber._2)
          concatenateNumbersAsCompleteString(firstPartOfNumber,restOfNumber,typeOfNumber,ConjunctionAndOthers.comma)
        case Million.million => val restPartOfNumber: String = convertThousandNumberToText(splitNumber._2.length, splitNumber._2)
              concatenateNumbersAsCompleteString(firstPartOfNumber,restPartOfNumber,typeOfNumber,ConjunctionAndOthers.comma)
      }
    }

  }

  private def getThousandOrMillion(number:List[Int], numberToSplitAt:Int,typeOfNumber:String): String ={
    val splitNumber = number.splitAt(numberToSplitAt)
    numberToSplitAt match {
      case 1 => val firstOneDigitOfNumber = convertDecimalOneDigitNumberToText(splitNumber._1)
       genericLogicForThousandAndMillion(firstOneDigitOfNumber,splitNumber,typeOfNumber)
      case 2 => val firstTwoDigitsOfNumber = convertDecimalTwoDigitNumberToText(splitNumber._1)
        genericLogicForThousandAndMillion(firstTwoDigitsOfNumber,splitNumber,typeOfNumber)
      case 3 =>val firstThreeDigitsOfNumber = convertHundredNumberToText(splitNumber._1)
        genericLogicForThousandAndMillion(firstThreeDigitsOfNumber,splitNumber,typeOfNumber)
    }
  }

  // ****************************************************************************

  private def convertDecimalOneDigitNumberToText(number: List[Int]): String = {
      From1to9.values.filter(_.id == number.head - 1).head.toString
  }

  private def convertDecimalTwoDigitNumberToText(number: List[Int]): String = {

    val lastPartOfNumberAsInt = number.tail.head // get the last part of list 'number' and get the value of that part using head
    val firstPartOfNumberAsInt = number.head // get the first part of list 'number'
    val numberAsInt = number.mkString.toInt
    val indexUnitNumber = List(number(1))

      if (lastPartOfNumberAsInt == 0) { // for two digits number ending in 0
        From10to90.values.filter(_.id == number.head - 1).head.toString
      }
      else if(firstPartOfNumberAsInt == 0){ // for two digits number where the first number is a zero
        convertDecimalOneDigitNumberToText(List(lastPartOfNumberAsInt))
      }
      else if (numberAsInt >= 11 && numberAsInt <= 19) {
        From11to19.values.filter(_.id == number(1) - 1).head.toString
      }
      else{
        val unitNumberAsText = convertDecimalOneDigitNumberToText(indexUnitNumber)
        val tenthNumberAsText = From10to90.values.filter(_.id == number.head - 1).head.toString
        concatenateNumbersAsCompleteString(tenthNumberAsText,unitNumberAsText)
      }

  }

  private def convertHundredNumberToText(number: List[Int]): String = {

     if(number.head == 0){
       val splittedNumber = number.splitAt(0)
       if(splittedNumber._2.head != 0){ // if first digit of a two digit number is not 0, it means is a two digit number
         convertDecimalTwoDigitNumberToText(splittedNumber._2) //
       }else{
         convertDecimalTwoDigitNumberToText(splittedNumber._2.tail)
       }
     }
     else if(number.tail.forall(_ == 0)){ // to display just hundred of any number such as 100, 500 , 600
       val firstNumberAsText = convertDecimalOneDigitNumberToText(List(number.head))
       concatenateNumbersAsCompleteString(firstNumberAsText,"",Hundred.hundred)
     }
     else{
       val firstNumberAsText = convertDecimalOneDigitNumberToText(List(number.head))
       val restOfNumber = convertDecimalTwoDigitNumberToText(number.tail)
       concatenateNumbersAsCompleteString(firstNumberAsText,restOfNumber,Hundred.hundred,ConjunctionAndOthers.and)
     }
  }

  private def convertThousandNumberToText(length:Int, number: List[Int]): String = {
    if (length == 4) {

      getThousandOrMillion(number,1,Thousand.thousand)

    }
    else if (length == 5) {

      getThousandOrMillion(number,2,Thousand.thousand)

    }
    else  // 6 digits

      getThousandOrMillion(number,3,Thousand.thousand)

  }

  private def convertMillionNumberToText(length : Int, number: List[Int]): String = {
    if (length == 7) {

      getThousandOrMillion(number,1,Million.million)

    }
    else if (length == 8) {
      getThousandOrMillion(number,2,Million.million)
    }
    else getThousandOrMillion(number,3,Million.million)
  }

  def printNumberAsText(number: Int): String = {
    val numericLength = getNumericLength(number)
    val numberAsList = splitNumberByDigits(number)
    numericLength match {
      case 1 => convertDecimalOneDigitNumberToText(numberAsList)
      case 2 => convertDecimalTwoDigitNumberToText(numberAsList)
      case 3 => convertHundredNumberToText(numberAsList)
      case n if n > 0 && n < 7 => convertThousandNumberToText(n,numberAsList)
      case m if m > 0 && m < 10 =>convertMillionNumberToText(m,numberAsList)
      case _ => "Not a valid number, a valid number is one between 0 and 999,999,999"
    }
  }

}
