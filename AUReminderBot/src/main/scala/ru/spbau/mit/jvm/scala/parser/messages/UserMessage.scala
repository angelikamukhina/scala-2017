package ru.spbau.mit.jvm.scala.parser.messages

import java.util.Date

trait UserMessage

case class AddReminderMessage(eventToRemind: String, date: Date) extends UserMessage

case object AllMyReminders extends UserMessage

case class CheckReminder(date: Date) extends UserMessage

case object WrongMessage extends UserMessage
