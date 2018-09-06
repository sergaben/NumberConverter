package Model

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */
//This class gives information about the number SRP
//TODO - see if this class is relevant
class Tools(val number : Int){

  def isNegNum : Boolean = if(number<0) true else false

  def isOverBillion : Boolean = number > 999999999

  def getNumericLength: Int = if(!isNegNum && !isOverBillion) BigDecimal(number).precision else -1

  def splitNumberByDigits: List[Int] = if(getNumericLength > 1) number.toString.map(_.asDigit).toList else List()

  def containsZeroDigit : Boolean = if(splitNumberByDigits.contains(0)) true else  false

  def between11and19 : Boolean = if(number >= 11 && number <= 19) true else false

  def identifyTypeOfTwoDigitNumber : Int = if(containsZeroDigit) 1 else if(between11and19) 2 else 3

}
