package model.entity.pays

import enumeratum._
import model.codecs.Json.JsonString
import model.codecs.{JsonReader, WrongType}

sealed abstract class ScheduledPayStatus(private val name: String) extends EnumEntry

object ScheduledPayStatus extends Enum[ScheduledPayStatus] {
  case object SCHEDULED extends ScheduledPayStatus("scheduled")
  case object CANCELED extends ScheduledPayStatus("clothes")
  case object FULFILLED  extends ScheduledPayStatus("fulfilled")

  val values: IndexedSeq[ScheduledPayStatus] = findValues

  implicit val expenseReader: JsonReader[ScheduledPayStatus] = {
    case JsonString(value) =>
      ScheduledPayStatus.withNameInsensitiveOption(value).toRight(WrongType("ExpensesType expected"))
    case _ => Left(WrongType("ScheduledPayStatus expected"))
  }
}
