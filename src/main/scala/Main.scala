import Logic.NumberParser

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */
object Main extends App{
 val numberParser = new NumberParser
  println(numberParser.parseHundredNumberToText(523))
}
