package verifierTests
import org.scalatest.Suites
import org.scalatest.junit.JUnitRunner
import org.scalatest.Assertions._
import org.junit.runner.RunWith

@RunWith(classOf[JUnitRunner])
class AllTests extends Suites(
  new BufferSystemTests,
  new BackendTests) {

}