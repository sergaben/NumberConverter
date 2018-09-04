import Controller.PrinterController

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */
object Main extends App{

  val printerController = new PrinterController(456)
  printerController.printNumber()

}
