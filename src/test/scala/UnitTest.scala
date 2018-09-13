import org.scalamock.proxy.ProxyMockFactory
import org.scalamock.specs2.MockContext
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */
abstract class UnitTest( component: String) extends FlatSpec with Matchers with ProxyMockFactory{
  implicit val mockContext:MockContext
}
