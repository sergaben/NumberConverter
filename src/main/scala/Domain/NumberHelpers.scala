package Domain

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 13/09/2018.
  *
  */
trait NumberHelpers {
  private def isNegNum(number: Int): Boolean = if (number < 0) true else false

  private def isOverBillion(number: Int): Boolean = number > 999999999

  def splitNumberByDigits(number: Int): List[Int] = number.toString.map(_.asDigit).toList

  def getNumericLength(number: Int): Int ={
    if(isNegNum(number) || isOverBillion(number))
      -1
    else if(splitNumberByDigits(number).length == 1 && splitNumberByDigits(number).head == 0 ){
      -1
    }
    else {
      splitNumberByDigits(number).length
    }
  }
}
