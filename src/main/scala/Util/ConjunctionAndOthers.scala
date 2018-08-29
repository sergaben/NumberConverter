package Util

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 29/08/2018.
  *
  */
object ConjunctionAndOthers extends Enumeration {

  type OtherKeywords = Value

  val and = Value("and").toString
  val space = Value(" ").toString
  val comma = Value(",").toString

}
