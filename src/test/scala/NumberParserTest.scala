import Logic.Parser

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */
class NumberParserTest extends UnitTest("Parser"){

  val parser = new Parser

  it should "return not a valid number if the given number is -1" in {
    parser.printNumberAsText(-89) shouldEqual "Not a valid number, a valid number is one between 0 and 999,999,999"
  }
  it should "return not a valid number if the given number is 0" in {
    parser.printNumberAsText(0) shouldEqual "Not a valid number, a valid number is one between 0 and 999,999,999"
  }
  it should "return not a valid number if the given number is greater than 999,999,999" in {
    parser.printNumberAsText(1000000000) shouldEqual "Not a valid number, a valid number is one between 0 and 999,999,999"
  }

  it should "return one as the given number is 1" in {
    parser.printNumberAsText(1)  shouldEqual "one"
  }

  it should "return nine as the given number is 9" in {
    parser.printNumberAsText(9)  shouldEqual "nine"
  }

  it should "return ten if the given number is 10" in {
    parser.printNumberAsText(10) shouldEqual "ten"
  }

  it should "return twenty if the given number is 20" in {
    parser.printNumberAsText(20) shouldEqual "twenty"
  }

  it should "return thirteen if the given number is 13" in {
    parser.printNumberAsText(13) shouldEqual "thirteen"
  }

  it should "return eighty seven if the given number is 87" in {
    parser.printNumberAsText(87) shouldEqual "eighty seven"
  }

  it should "return one hundred and one if the given number is 101" in {
    parser.printNumberAsText(101) shouldEqual "one hundred and one"
  }

  it should "return one hundred and ten if the given number is 110" in {
    parser.printNumberAsText(110) shouldEqual "one hundred and ten"
  }

  it should "return one hundred and fifteen if the given number is 115" in {
    parser.printNumberAsText(115) shouldEqual "one hundred and fifteen"
  }

  it should "return five hundred and seventy nine if the given number is 579" in {
    parser.printNumberAsText(579) shouldEqual "five hundred and seventy nine"
  }

  it should "return five thousand if the given number is 5000" in {
    parser.printNumberAsText(5000) shouldEqual "five thousand"
  }

  it should "return one thousand and one if the given number is 1001" in {
    parser.printNumberAsText(1001) shouldEqual "one thousand and one"
  }

  it should "return one thousand and eleven if the given number is 1011" in {
    parser.printNumberAsText(1011) shouldEqual "one thousand and eleven"
  }

  it should "return four thousand, five hundred and eighty one if the given number is 4581" in {
    parser.printNumberAsText(4581) shouldEqual "four thousand, five hundred and eighty one"
  }

  it should "return four thousand, five hundred and twelve if the given number is 4512" in {
    parser.printNumberAsText(4512) shouldEqual "four thousand, five hundred and twelve"
  }

  it should "return forty thousand if the given number is 40000" in {
    parser.printNumberAsText(40000) shouldEqual "forty thousand"
  }

  it should "return forty thousand and one if the given number is 40001" in {
    parser.printNumberAsText(40001) shouldEqual "forty thousand and one"
  }

  it should "return forty thousand and ten if the given number is 40010" in {
    parser.printNumberAsText(40010) shouldEqual "forty thousand and ten"
  }

  it should "return forty thousand, one hundred if the given number is 40100" in {
    parser.printNumberAsText(40100) shouldEqual "forty thousand, one hundred"
  }

  it should "return forty thousand, six hundred and nineteen if the given number is 40619" in {
    parser.printNumberAsText(40619) shouldEqual "forty thousand, six hundred and nineteen"
  }

  it should "return seventy eight thousand, nine hundred and forty nine if the given number is 78949" in {
    parser.printNumberAsText(78949) shouldEqual "seventy eight thousand, nine hundred and forty nine"
  }

  it should "return five hundred and twenty five thousand , six hundred and nineteen if the given number is 525619" in {
    parser.printNumberAsText(525619) shouldEqual "five hundred and twenty five thousand, six hundred and nineteen"
  }

  it should "return five hundred thousand if the given number is 500000" in {
    parser.printNumberAsText(500000) shouldEqual "five hundred thousand"
  }

  it should "return five hundred thousand and one if the given number is 500001" in {
    parser.printNumberAsText(500001) shouldEqual "five hundred thousand and one"
  }

  it should "return seven million if the given number is 7000000" in {
    parser.printNumberAsText(7000000) shouldEqual "seven million"
  }

  it should "return seven million, one if the given number is 7000001" in {
    parser.printNumberAsText(7000001) shouldEqual "seven million, one"
  }

  it should "return seven million, ten if the given number is 7000010" in {
    parser.printNumberAsText(7000010) shouldEqual "seven million, ten"
  }

  it should "return seven million, one hundred if the given number is 7000100" in {
    parser.printNumberAsText(7000100) shouldEqual "seven million, one hundred"
  }

  it should "return seven million and one thousand if the given number is 7001000" in {
    parser.printNumberAsText(7001000) shouldEqual "seven million, one thousand"
  }

  it should "return seven million and ten thousand if the given number is 7010000" in {
    parser.printNumberAsText(7010000) shouldEqual "seven million, ten thousand"
  }

  it should "return seven million, nine hundred and twenty five thousand, five hundred and twelve if the given number is 7925512" in {
    parser.printNumberAsText(7925512) shouldEqual "seven million, nine hundred and twenty five thousand, five hundred and twelve"
  }

  it should "return ten million, eight hundred and fifty five thousand, two hundred and thirty five if the given number is 10855235" in {
    parser.printNumberAsText(10855235) shouldEqual "ten million, eight hundred and fifty five thousand, two hundred and thirty five"
  }

  it should "return one hundred million, one if the given number is 100000001" in {
   parser.printNumberAsText(100000001) shouldEqual "one hundred million, one"
  }

  it should "return one hundred million, one hundred and one if the given number is 100000101" in {
    parser.printNumberAsText(100000101) shouldEqual "one hundred million, one hundred and one"
  }
  it should "return nine hundred and ninety nine million, nine hundred and ninety nine thousand, nine hundred and ninety nine if the given number is 999999999" in {
    parser.printNumberAsText(999999999) shouldEqual "nine hundred and ninety nine million, nine hundred and ninety nine thousand, nine hundred and ninety nine"
  }

}
