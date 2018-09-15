import org.scalamock.proxy.ProxyMockFactory
import org.scalamock.scalatest.MockFactory
import org.scalatest.{FlatSpec, Matchers}

/**
  *
  * @project NumberToWrittenEng
  * @author sergaben on 28/08/2018.
  *
  */
abstract class UnitTest( component: String) extends FlatSpec with MockFactory with ProxyMockFactory with Matchers {

}
