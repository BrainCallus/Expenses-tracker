package model.entity.pays

import enumeratum.{Enum, EnumEntry}
import model.codecs.Json.JsonString
import model.codecs.{JsonReader, WrongType}


sealed abstract class ExpensesType(private val name: String) extends EnumEntry

object ExpensesType extends Enum[ExpensesType] {
  case object FOOD extends ExpensesType("food")
  case object CLOTHES extends ExpensesType("clothes")
  case object ENTERTAINMENT extends ExpensesType("entertainment")
  case object HEALTH extends ExpensesType("health")
  case object SPORT extends ExpensesType("sport")
  case object TRANSPORT extends ExpensesType("transport")
  case object OTHER extends ExpensesType("other")
  val values: IndexedSeq[ExpensesType] = findValues

  implicit val expenseReader: JsonReader[ExpensesType] = {
    case JsonString(value) =>
      ExpensesType.withNameInsensitiveOption(value).toRight(WrongType("ExpensesType expected"))
    case _ => Left(WrongType("ExpensesType expected"))
  }
}
