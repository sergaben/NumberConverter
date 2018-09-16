package Helpers

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 13/09/2018.
  *
  */
trait NumberHelpers extends ErrorHandling{

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

  def containsAllZerosAfterFirstDigit(splitNumber: (List[Int], List[Int])): Boolean = {
    splitNumber._2.forall(_ == 0)
  }

  def hasMixedZerosAndNumbers(splitNumber: (List[Int], List[Int])): Boolean = {
    splitNumber._2.head == 0 && !containsAllZerosAfterFirstDigit(splitNumber)
  }


}
