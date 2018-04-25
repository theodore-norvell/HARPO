package tests

import org.scalatest.Suites
import org.scalatest.junit.JUnitRunner
import org.scalatest.Assertions._
import org.junit.runner.RunWith


@RunWith(classOf[JUnitRunner]) 
class AllTests extends Suites(
                  new ParserTests,
                  new LiteralTests,
                  new CheckerTests,
                  new TypeCheckerTests,
                  new TypeCheckerFuncTests,
                  new TypeCheckerUnitTests,
                  new BackEndTests)
