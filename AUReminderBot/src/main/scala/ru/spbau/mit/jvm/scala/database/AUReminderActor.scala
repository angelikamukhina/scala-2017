package ru.spbau.mit.jvm.scala.database

import java.util.Date

import akka.persistence.PersistentActor
import ru.spbau.mit.jvm.scala.database.AUReminderActor._

import scala.collection.mutable

class AUReminderActor extends PersistentActor {

  val map: mutable.HashMap[Long, mutable.HashMap[Date, String]] = mutable.HashMap.empty

  override def receiveRecover = {
    case event: Event => receiveEvent(event)
  }

  override def receiveCommand = {
    case event: Event => persist(event)(receiveEvent)
    case GetAllReminders(id) =>
      sender ! Reminders(map.getOrElse(id, mutable.HashMap.empty))
    case GetReminder(id, date) =>
      sender ! Reminder(date, map.getOrElse(id, mutable.HashMap.empty).getOrElse(date, null))
  }

  def receiveEvent(event: Event): Unit = {
    event match {
      case AddReminder(id, date, text) =>
        map.getOrElseUpdate(id, mutable.HashMap.empty) +=
          ((date, text))
    }
  }

  override def persistenceId = "au-reminder-database"
}

object AUReminderActor {

  trait Event

  case class AddReminder(id: Long, date: Date, text: String) extends Event

  case class GetAllReminders(id: Long)

  case class GetReminder(id: Long, date: Date)

  case class Reminders(buffer: mutable.HashMap[Date, String])

  case class Reminder(date: Date, eventToRemind: String)

}
