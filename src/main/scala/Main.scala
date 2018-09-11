import Logic.Parser

import scala.util.Random

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 29/08/2018.
  *
  */

object Main extends App {

  val parser = new Parser
  val randomNumber = Random.nextInt(999999999)

  parser.printNumberAsText(randomNumber)

}
