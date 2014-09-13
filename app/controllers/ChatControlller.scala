package controllers

import play.api.mvc.{Action, Controller}
import play.api.Logger
import scala.collection.mutable.ListBuffer
import play.api.libs.json.Json
import java.util.concurrent.atomic.AtomicInteger


case class Record(msgId: Int, user: String, val text: String)


object ChatController extends Controller {


  val counter = new AtomicInteger(12)

  val users = new ListBuffer[String]()

  val messages = new ListBuffer[Record]()

  def login(name: String) = Action {
    Logger.debug("New user arrived " + name)
    users += name
    Ok(s"Welcome, $name")
  }


  private def doInsert(user: String, text: String, update: Boolean = true) = {
    if (!users.contains(user)) {
      Unauthorized(s"$user, please login first")
    } else {
      Logger.debug(s"User <$user> posted message <$text> to update $update")
      val flt = messages.filter(_.user == user)
      if (update && flt.nonEmpty) {
        val filter = messages.filter(_.user == user)
        val rec = filter.last
        val updRec = rec.copy(text = rec.text + text)
        messages.update(messages.indexOf(rec), updRec)
        Ok("Added")
      } else {
        messages += Record(counter.getAndIncrement, user, text)
        Ok("Saved")
      }
    }
  }

  def putText(user: String, text: String) = Action {
    doInsert(user, text, update = false)
  }

  def updateText(user: String, text: String) = Action {
    doInsert(user, text, update = true)
  }

  implicit val format = Json.format[Record]

  def getUpdates = Action {
    val toDrop = if (messages.size > 10) messages.size - 10 else 0
    val obj = Json.obj("messages" -> messages.drop(toDrop))
    Ok(obj.toString)
  }

}
