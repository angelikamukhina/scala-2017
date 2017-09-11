import java.io.{Reader, StringReader, StringWriter, Writer}

import org.scalatest.FunSuite
import ru.spbau.scala.SimpleCalculator

import scala.collection.mutable.ListBuffer

class SimpleCalculatorTest extends FunSuite {
  def compareDouble(d1: Double, d2: Double): Boolean = {
    val accuracy = 1e-5
    Math.abs(d2 - d1) <= accuracy
  }

  test("testEvaluateExpression") {
    val PostfixTokensList: ListBuffer[String] = new ListBuffer[String]
    PostfixTokensList += "4"
    PostfixTokensList += "3"
    PostfixTokensList += "+"
    PostfixTokensList += "12"
    PostfixTokensList += "13"
    PostfixTokensList += "6"
    PostfixTokensList += "*"
    PostfixTokensList += "-"
    PostfixTokensList += "*"
    val res: Double = SimpleCalculator.evaluateExpression(PostfixTokensList)
    assert(compareDouble(res, -462.0))
  }

  test("testCalculate") {
    val reader1: Reader = new StringReader("( 3 + 5 ) * 2")
    val writer1: Writer = new StringWriter()
    SimpleCalculator.calculate(reader1, writer1)
    val res1: Double = writer1.toString.toDouble
    assert(compareDouble(res1, 16.0))

    val reader2: Reader = new StringReader("( 3 - 5 ) / 4")
    val writer2: Writer = new StringWriter()
    SimpleCalculator.calculate(reader2, writer2)
    val res2: Double = writer2.toString.toDouble
    assert(compareDouble(res2, -0.5))

    reader1.close
    reader2.close
    writer1.close
    writer2.close
  }

  test("testEvaluate") {
    assert(compareDouble(SimpleCalculator.evaluate("+", 43, 67), 110.0))
    assert(compareDouble(SimpleCalculator.evaluate("-", 23, 43), 20.0)) //reverse order
    assert(compareDouble(SimpleCalculator.evaluate("*", 3, 60), 180.0))
    assert(compareDouble(SimpleCalculator.evaluate("/", 7, 42), 6.0)) // reverse order
  }
}
