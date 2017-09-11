package ru.spbau.scala

import java.io.{Reader, Writer}
import java.util.Stack

import scala.collection.mutable.ListBuffer
import scala.util.Try

object SimpleCalculator {
  val priorities = Map("+" -> 6, "-" -> 6, "*" -> 7, "/" -> 7, "(" -> 1)

  def calculate(reader: Reader, writer: Writer): Unit = {
    var read: Int = reader.read
    var expression: String = ""
    while (read != -1) {
      expression = expression.concat(read.toChar.toString)
      read = reader.read
    }
    val postfixTokensList = infixToPostfix(expression)
    writer.write(evaluateExpression(postfixTokensList).toString)
  }

  private def infixToPostfix(infixExpression: String): ListBuffer[String] = {
    val initialTokensList: List[String] = infixExpression.split(" ").toList
    val operationsStack: Stack[String] = new Stack[String]
    var finalTokensList = new ListBuffer[String]
    for (tokenIndex <- initialTokensList.indices) {
      if (initialTokensList(tokenIndex).equals("(")) {
        operationsStack.push(initialTokensList(tokenIndex))
      }
      else if (initialTokensList(tokenIndex).equals(")")) {
        var top: String = operationsStack.pop
        while (!top.equals("(")) {
          finalTokensList += top
          top = operationsStack.pop()
        }
      }
      else if (parseDouble(initialTokensList(tokenIndex)).isDefined) {
        finalTokensList += initialTokensList(tokenIndex)
      }
      else {
        while (!operationsStack.empty &&
          priorities(operationsStack.peek) >= priorities(initialTokensList(tokenIndex))) {
          finalTokensList += operationsStack.pop
        }
        operationsStack.push(initialTokensList(tokenIndex))
      }
    }
    while (!operationsStack.empty) {
      finalTokensList += operationsStack.pop
    }
    finalTokensList
  }

  def evaluateExpression(postfixTokensList: ListBuffer[String]): Double = {
    val argsStack: Stack[Double] = new Stack[Double]
    for (token <- postfixTokensList) {
      var arg: Option[Double] = parseDouble(token)
      if (arg.isDefined) {
        argsStack.push(arg.get)
      }
      else {
        val arg1: Double = argsStack.pop
        val arg2: Double = argsStack.pop
        val res: Double = evaluate(token, arg1, arg2)
        argsStack.push(res)
      }
    }
    argsStack.pop
  }

  private def parseDouble(str: String): Option[Double] = Try {
    str.toDouble
  }.toOption

  def evaluate(operation: String, arg1: Double, arg2: Double): Double = {
    if (operation.equals("+")) {
      arg1 + arg2
    }
    else if (operation.equals("-")) {
      arg2 - arg1
    }
    else if (operation.equals("*")) {
      arg1 * arg2
    }
    else {
      arg2 / arg1
    }
  }
}
