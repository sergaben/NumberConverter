import Domain.NumberHelpers
import org.scalamock.proxy.Mock

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 13/09/2018.
  *
  */

// TODO - Mocking the trait NumberHelpers, not entirely sure if this is the right way to do it
class NumberHelpersTest extends UnitTest ("NumberToConvert"){

  val numberHelpersMock: NumberHelpers with Mock = mock[NumberHelpers]


  "The helper method getNumericLength" should "return 1 as the length of the number, if the number passed is 8" in {
    numberHelpersMock.expects('getNumericLength)(8).returning(1)
    numberHelpersMock.getNumericLength(8)
  }
  "The helper method getNumericLength" should "return 2 as the length of the number, if the number passed is 78" in {
    numberHelpersMock.expects('getNumericLength)(78).returning(2)
    numberHelpersMock.getNumericLength(78)
  }
  "The helper method getNumbericLength" should "return 3 as the length of the number, if the number passed is 789" in {
    numberHelpersMock.expects('getNumericLength)(789).returning(3)
    numberHelpersMock.getNumericLength(789)
  }
  "The helper method getNumericLength" should "return 4 as the length of the number, if the number passed is 7894" in {
    numberHelpersMock.expects('getNumericLength)(7894).returning(4)
    numberHelpersMock.getNumericLength(7894)
  }
  "The helper method getNumericLength" should "return 5 as the length of the number, if the number passed is 78945" in{
    numberHelpersMock.expects('getNumericLength)(78945).returning(5)
    numberHelpersMock.getNumericLength(78945)
  }
  "The helper method getNumericLength" should "return 6 as the length of the number, if the number passed is 789456" in {
    numberHelpersMock.expects('getNumericLength)(789456).returning(6)
    numberHelpersMock.getNumericLength(789456)
  }
  "The helper method getNumericLength" should "return 7 as the length of the number, if the number passed is 7894651" in {
    numberHelpersMock.expects('getNumericLength)(7894561).returning(7)
    numberHelpersMock.getNumericLength(7894561)
  }
  "The helper method getNumericLength" should "return 8 as the length of the number, if the number passed is 78945612" in {
    numberHelpersMock.expects('getNumericLength)(78945612).returning(8)
    numberHelpersMock.getNumericLength(78945612)
  }
  "The helper method getNumericLength" should "return 9 as the length of the number, if the number passed is 789456123" in {
    numberHelpersMock.expects('getNumericLength)(789456123).returning(9)
    numberHelpersMock.getNumericLength(789456123)
  }
  "The helper method getNumericLength" should "return -1 as the length of the number, if the number passed is over 999,999,999" in {
    numberHelpersMock.expects('getNumericLength)(1789456123).returning(-1)
    numberHelpersMock.getNumericLength(1789456123)
  }
  "The helper method getNumericLength" should "return -1 as the length of the number, if the number passed is a negative number" in {
    numberHelpersMock.expects('getNumericLength)(-893).returning(-1)
    numberHelpersMock.getNumericLength(-893)
  }

  "The helper method splitNumberByDigits" should "return a list containing the digits that made up the number that was passed as an argument, " +
    "such as 789 -> List(7,8,9)" in {
    numberHelpersMock.expects('splitNumberByDigits)(789).returning(List(7,8,9))
    numberHelpersMock.splitNumberByDigits(789)
  }

}
