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

  private def isNegOrOverBillion(number:Int): Boolean = if(isNegNum(number) || isOverBillion(number)) true else false

  def splitNumberByDigits(number: Int): List[Int] = if(isNegOrOverBillion(number)) List() else number.toString.map(_.asDigit).toList

  def getNumericLength(number: Int): Int ={
    if(isNegOrOverBillion(number))
      -1
    else if(number == 0){
      -1
    }
    else {
      splitNumberByDigits(number).length
    }
  }
}
