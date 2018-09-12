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
  val randomNumber = Random.nextInt(84059)

  println(parser.printNumberAsText(4001))

}
