package ru.spbau.mit.jvm.scala.bot

import akka.actor.{Actor, ActorRef}
import akka.pattern.ask
import akka.util.Timeout
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.api.{Polling, TelegramBot}
import ru.spbau.mit.jvm.scala.database.AUReminderActor._
import ru.spbau.mit.jvm.scala.parser.MessageParser
import ru.spbau.mit.jvm.scala.parser.messages.{AddReminderMessage, AllMyReminders, CheckReminder, WrongMessage}

import scala.collection.mutable
import scala.concurrent.duration.DurationInt
import scala.util.Success


class AskActor(bot: AUReminderBot) extends Actor {

  override def receive = {
    case _ => bot.askUsers()
  }
}

class AUReminderBot(val token: String, val database: ActorRef) extends TelegramBot with Polling with Commands {

  val map: mutable.HashMap[Long, String] = mutable.HashMap.empty

  def askUsers(): Unit = {

  }

  onMessage {
    implicit message =>
      message.text.foreach { string: String =>
        MessageParser.parse(string) match {
          case AddReminderMessage(text, date) =>
            database !
              AddReminder(message.chat.id, date, text)
            reply("ОК! Напоминание добавлено.")
          case AllMyReminders =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetAllReminders(message.chat.id)).onComplete {
              case Success(Reminders(buffer)) =>
                reply(buffer.map {
                  case (date, text) => s"$text в ${date.toString}"
                }.mkString("\n"))
              case _ =>
                reply("Ошибка базы данных")
            }
          case CheckReminder(date) =>
            implicit val timeout: Timeout = Timeout(1.second)
            (database ? GetReminder(message.chat.id, date)).onComplete {
              case Success(Reminder(eventDate, eventToRemind)) =>
                reply(s"Да, напоминание на это время установлено: $eventToRemind в ${eventDate.toString}")
              case _ =>
                reply("Ни одного напоминания на это время не установлено.")
            }
          case WrongMessage =>
            reply("Неверная команда")
        }
      }
  }
}
