package Helpers

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 16/09/2018.
  *
  */
trait ErrorHandling {
  protected def isNegNum(number: Int): Boolean = if (number < 0) true else false

  protected def isOverBillion(number: Int): Boolean = number > 999999999

  protected def isNegOrOverBillion(number:Int): Boolean = if(isNegNum(number) || isOverBillion(number)) true else false
}
