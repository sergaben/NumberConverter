package Logic

import Util._

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */

//TODO - change how the identification of digits work
//TODO - change how the logic is structured, try to optimise it
//TODO -

class Parser() {

  private def isNegNum(number: Int): Boolean = if (number < 0) true else false

  private def isOverBillion(number: Int): Boolean = number > 999999999

  private def splitNumberByDigits(number: Int): List[Int] = number.toString.map(_.asDigit).toList

  private def getNumericLength(number: Int): Int ={
    if(isNegNum(number) || isOverBillion(number))
      -1
    else if(splitNumberByDigits(number).length == 1 && splitNumberByDigits(number).head == 0 ){
      -1
    } else {
      splitNumberByDigits(number).length
    }
  }

  def parseDecimalOneDigitNumberToText(number: List[Int]): String = {
      From1to9.values.filter(_.id == number.head - 1).head.toString
  }

  def parseDecimalTwoDigitNumberToText(number: List[Int]): String = {

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
        val tenthNumberAsText = From10to90.values.filter(_.id == number.head - 1).head.toString + ConjunctionAndOthers.space
        val unitNumberAsText = parseDecimalOneDigitNumberToText(indexUnitNumber)

        tenthNumberAsText + unitNumberAsText
      }

  }

  //**************** MIGHT HAVE TO BE REFACTORED ***************

  def parseHundredNumberToText(number: List[Int]): String = {

     if(number.head == 0){
       val splittedNumber = number.splitAt(0)
       if(splittedNumber._2.head != 0){ // if first digit of a two digit number is not 0, it means is a two digit number
         parseDecimalTwoDigitNumberToText(splittedNumber._2) //
       }else{
         parseDecimalTwoDigitNumberToText(splittedNumber._2.tail)
       }
     }
     else if(number.tail.forall(_ == 0)){ // to display just hundred of any number such as 100, 500 , 600


       val firstNumberAsText = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space +
         Hundred.hundred

       firstNumberAsText

     }
     else{
       val restOfNumberAsInt = number.tail

       val firstNumberAsText = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space +
         Hundred.hundred

       val restOfNumberAsText = ConjunctionAndOthers.space + ConjunctionAndOthers.and + ConjunctionAndOthers.space +
         parseDecimalTwoDigitNumberToText(restOfNumberAsInt)

       val completeNumberAsText = firstNumberAsText + restOfNumberAsText

       completeNumberAsText
     }
  }

  // ******************** IMMEDIATE REFACTOR ******************************

  def parseThousandNumberToText(length:Int, number: List[Int]): String = {
    if (length == 4) {
      val splittedNumber = number.splitAt(1)

      if(splittedNumber._2.forall(_ == 0)){

        val thousand = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space +
          Thousand.thousand

        thousand
      }
      else{
        val firstNumber = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space +
          Thousand.thousand + ConjunctionAndOthers.space + ConjunctionAndOthers.and

        val restOfNumber = ConjunctionAndOthers.space + parseHundredNumberToText(number.tail)

        val thousandWithZerosInBetween = firstNumber + restOfNumber

        thousandWithZerosInBetween
      }

    }
    else if (length == 5) {

      val splittedList = number.splitAt(2)

      if(splittedList._2.forall(_ == 0)){ // find whole thousand numbers

        val thousandTwoDigitsWithNotTrailingNumbers =  parseDecimalTwoDigitNumberToText(splittedList._1) +  ConjunctionAndOthers.space +
          Thousand.thousand

        thousandTwoDigitsWithNotTrailingNumbers

      }
      else if(splittedList._2.head == 0){ // find thousand numbers with middle zeros

        val firstTwoNumbers = parseDecimalTwoDigitNumberToText(splittedList._1) + ConjunctionAndOthers.space +
          Thousand.thousand + ConjunctionAndOthers.space + ConjunctionAndOthers.and + ConjunctionAndOthers.space

        val restOfNumber = parseHundredNumberToText(splittedList._2)

        val thousandTwoDigitsWithZerosInBetween = firstTwoNumbers + restOfNumber

        thousandTwoDigitsWithZerosInBetween
      }
      else{ // for a common number without zeros in the middle of the number

        val firstTwoNumbers = parseDecimalTwoDigitNumberToText(splittedList._1) + ConjunctionAndOthers.space +
          Thousand.thousand + ConjunctionAndOthers.comma + ConjunctionAndOthers.space

        val restOfNumber = parseHundredNumberToText(splittedList._2)

        val thousandTwoDigitsWithNoEdgeCases = firstTwoNumbers + restOfNumber
        thousandTwoDigitsWithNoEdgeCases
      }

    }
    else {

      val splittedNumber = number.splitAt(3)

      if(splittedNumber._2.head == 0 && !splittedNumber._2.forall(_ == 0)){ // thousand hundred with zeros in between the number
        val firstThreeDigitsOfNumber = parseHundredNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Thousand.thousand +
          ConjunctionAndOthers.space + ConjunctionAndOthers.and + ConjunctionAndOthers.space
        val restOfNumber = parseHundredNumberToText(splittedNumber._2)

        val thousandHundredWithZerosInbetween = firstThreeDigitsOfNumber + restOfNumber

        thousandHundredWithZerosInbetween
      }
      else if(splittedNumber._2.forall(_ == 0)){ // thousand hundred numbers all with trailing zeros
        val thousandHundredWithTrailingZeros = parseHundredNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Thousand.thousand
        thousandHundredWithTrailingZeros
      }
      else{

        val firstThreeNumbers = parseHundredNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Thousand.thousand + ConjunctionAndOthers.comma
        val restOfNumber = ConjunctionAndOthers.space + parseHundredNumberToText(splittedNumber._2)
        val thousandHundredWithNoEdgeCases =  firstThreeNumbers + restOfNumber
       thousandHundredWithNoEdgeCases
      }
    }
  }

  def parseMillionNumberToText( length : Int, number: List[Int]): String = {
    if (length == 7) {

      val splittedNumber = number.splitAt(1)

      if(splittedNumber._2.forall(_==0)){
        val millionWithTrailingZeros = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space + Million.million
        millionWithTrailingZeros
      }
      else if(splittedNumber._2.head == 0 && !splittedNumber._2.forall(_==0)){
        val fiveDigitsOfNumber = splittedNumber._2.tail

        if(fiveDigitsOfNumber.head == 0){
          val fourDigitsOfNumber = fiveDigitsOfNumber.tail

          if(fourDigitsOfNumber.head == 0){
            val threeDigitsOfNumber = fourDigitsOfNumber.tail

            val firstDigitOfNumber: String = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space + Million.million +
              ConjunctionAndOthers.comma + ConjunctionAndOthers.space
            val restOfNumber: String = parseHundredNumberToText(threeDigitsOfNumber)

            val millionWithZerosInBetween = firstDigitOfNumber + restOfNumber

            millionWithZerosInBetween

          }
          else{
            val firstDigitOfNumber: String = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space + Million.million +
              ConjunctionAndOthers.comma + ConjunctionAndOthers.space
            val restOfNumber: String = parseThousandNumberToText(fourDigitsOfNumber.length,fourDigitsOfNumber)

            val millionWithZerosInBetween = firstDigitOfNumber + restOfNumber
            millionWithZerosInBetween
          }
        }
        else{
          val firstDigitOfNumber: String = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space + Million.million +
            ConjunctionAndOthers.comma + ConjunctionAndOthers.space
          val restOfNumber: String = parseThousandNumberToText(fiveDigitsOfNumber.length,fiveDigitsOfNumber)

          val millionWithZerosInBetween = firstDigitOfNumber + restOfNumber
          millionWithZerosInBetween

        }
      }
      else{
        val firstDigitOfNumber: String = parseDecimalOneDigitNumberToText(List(number.head)) + ConjunctionAndOthers.space + Million.million +
          ConjunctionAndOthers.comma + ConjunctionAndOthers.space

        val restOfNumber: String = parseThousandNumberToText(number.tail.length,number.tail)

        val millionWithNoEdgeCases =  firstDigitOfNumber + restOfNumber

        millionWithNoEdgeCases
      }

    }
    else if (length == 8) {
      val splittedNumber = number.splitAt(2)

      if(splittedNumber._2.forall(_==0)){
        val millionTwoDigitsWithTrailingZeros = parseDecimalTwoDigitNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million
        millionTwoDigitsWithTrailingZeros
      }
      else if(splittedNumber._2.head == 0 && !splittedNumber._2.forall(_==0)){
        val fiveDigitsOfNumber = splittedNumber._2.tail

        if(fiveDigitsOfNumber.head == 0){
          val fourDigitsOfNumber = fiveDigitsOfNumber.tail

          if(fourDigitsOfNumber.head == 0){
            val threeDigitsOfNumber = fourDigitsOfNumber.tail

            val firstTwoDigitOfNumber: String = parseDecimalTwoDigitNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million +
               ConjunctionAndOthers.comma + ConjunctionAndOthers.space
            val restOfNumber: String = parseHundredNumberToText(threeDigitsOfNumber)

            val millionTwoDigitsWithZerosInBetween = firstTwoDigitOfNumber + restOfNumber

            millionTwoDigitsWithZerosInBetween

          }
          else{
            val firstDigitOfNumber: String = parseDecimalTwoDigitNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million +
              ConjunctionAndOthers.comma + ConjunctionAndOthers.space
            val restOfNumber: String = parseThousandNumberToText(fourDigitsOfNumber.length,fourDigitsOfNumber)

            val millionTwoDigitsWithZerosInBetween = firstDigitOfNumber + restOfNumber
            millionTwoDigitsWithZerosInBetween
          }
        }
        else{
          val firstDigitOfNumber: String = parseDecimalTwoDigitNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million +
            ConjunctionAndOthers.comma + ConjunctionAndOthers.space
          val restOfNumber: String = parseThousandNumberToText(fiveDigitsOfNumber.length,fiveDigitsOfNumber)

          val millionTwoDigitsWithZerosInBetween = firstDigitOfNumber + restOfNumber
          millionTwoDigitsWithZerosInBetween

        }
      }
      else{
        val firstDigitOfNumber: String = parseDecimalTwoDigitNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million +
          ConjunctionAndOthers.comma + ConjunctionAndOthers.space

        val restOfNumber: String = parseThousandNumberToText(splittedNumber._2.length,splittedNumber._2)

        val millionWithNoEdgeCases =  firstDigitOfNumber + restOfNumber

        millionWithNoEdgeCases
      }
    }
    else {
      val splittedNumber = number.splitAt(3)
      if(splittedNumber._2.forall(_==0)){
        val millionHundredWithTrailingZeros = parseHundredNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million
        millionHundredWithTrailingZeros
      }
      else if(splittedNumber._2.head == 0 && !splittedNumber._2.forall(_==0)){
        val fiveDigitsOfNumber = splittedNumber._2.tail

        if(fiveDigitsOfNumber.head == 0){
          val fourDigitsOfNumber = fiveDigitsOfNumber.tail

          if(fourDigitsOfNumber.head == 0){
            val threeDigitsOfNumber = fourDigitsOfNumber.tail

            val firstTwoDigitOfNumber: String = parseHundredNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million +
              ConjunctionAndOthers.comma + ConjunctionAndOthers.space
            val restOfNumber: String = parseHundredNumberToText(threeDigitsOfNumber)

            val millionHundredWithZerosInBetween = firstTwoDigitOfNumber + restOfNumber

            millionHundredWithZerosInBetween

          }
          else{
            val firstDigitOfNumber: String = parseHundredNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million +
              ConjunctionAndOthers.comma + ConjunctionAndOthers.space
            val restOfNumber: String = parseThousandNumberToText(fourDigitsOfNumber.length,fourDigitsOfNumber)

            val millionHundredWithZerosInBetween = firstDigitOfNumber + restOfNumber
            millionHundredWithZerosInBetween
          }
        }
        else{
          val firstDigitOfNumber: String = parseHundredNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million +
            ConjunctionAndOthers.comma + ConjunctionAndOthers.space
          val restOfNumber: String = parseThousandNumberToText(fiveDigitsOfNumber.length,fiveDigitsOfNumber)

          val millionHundredWithZerosInBetween = firstDigitOfNumber + restOfNumber
          millionHundredWithZerosInBetween

        }
      }
      else{
        val firstDigitOfNumber: String = parseHundredNumberToText(splittedNumber._1) + ConjunctionAndOthers.space + Million.million +
          ConjunctionAndOthers.comma + ConjunctionAndOthers.space

        val restOfNumber: String = parseThousandNumberToText(splittedNumber._2.length,splittedNumber._2)

        val millionHundredWithNoEdgeCases =  firstDigitOfNumber + restOfNumber

        millionHundredWithNoEdgeCases
      }
    }
  }

  //********************** END OF REFACTORING ***********************************

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
