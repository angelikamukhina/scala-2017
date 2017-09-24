package ru.spbau.mit.jvm.scala.parser

import java.text.SimpleDateFormat
import java.util.{Date, Locale}

import ru.spbau.mit.jvm.scala.parser.messages._

import scala.util.matching.Regex
import scala.util.parsing.combinator.RegexParsers

class MessageParser extends RegexParsers {
  override val whiteSpace: Regex = "[ \t\r\f]+".r
  val eventToRemindOfParser: Parser[String] = "\".*\"".r
  val dateParser: Parser[Date] = ".*".r ^^ {
    date =>
      val formatter = new SimpleDateFormat("HH:mm:ss MMM d yyyy", Locale.ENGLISH)
      val d = formatter.parse(date)
      d
  }
  val addReminder: Parser[AddReminderMessage] =
    "[Нн]апомни мне:".r ~> eventToRemindOfParser ~ ("в" ~> dateParser) ^^ {
      case eventToRemind ~ date => AddReminderMessage(eventToRemind, date)
    }
  val allMyReminders: Parser[UserMessage] = "[Пп]окажи все напоминания".r ^^ { _ => AllMyReminders }
  val checkReminder: Parser[UserMessage] = "[Пп]роверь напоминание на время:".r ~> dateParser ^^ {
    date => CheckReminder(date)
  }
  val userMessage: Parser[UserMessage] = addReminder | allMyReminders | checkReminder

  override def skipWhitespace: Boolean = true
}

object MessageParser extends MessageParser {
  def parse(text: String): UserMessage = {
    parse(userMessage, text) match {
      case Success(message, _) => message
      case _ => WrongMessage
    }
  }
}