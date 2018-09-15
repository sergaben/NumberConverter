package Model

import Domain.NumberHelpers
import Util.From1to9

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 15/09/2018.
  *
  */
class OneDigitNumber(number:Int) extends NumberHelpers{

  def convertDecimalOneDigitNumberToString: String = {
    From1to9.values.filter(_.id == splitNumberByDigits(number).head - 1).head.toString
  }

}
