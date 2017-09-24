package ru.spbau.mit.jvm.scala

import akka.actor.{ActorSystem, Props}
import com.typesafe.akka.extension.quartz.QuartzSchedulerExtension
import ru.spbau.mit.jvm.scala.bot.{AUReminderBot, AskActor}
import ru.spbau.mit.jvm.scala.database.AUReminderActor

object Main extends App {
  val token = "398399572:AAHISu-dBxIy5_rUSoN8BIbBl9qhX4u7QDw"

  val system = ActorSystem()
  val scheduler = QuartzSchedulerExtension(system)
  val database = system.actorOf(Props(classOf[AUReminderActor]))
  val actor = system.actorOf(Props(classOf[AskActor], bot))
  private val bot = new AUReminderBot(token, database)

  scheduler.createSchedule("every minute", None, "	0/1 * * 1/1 * ? *")
  scheduler.schedule("every minute", actor, "Ask")

  bot.run()
}
