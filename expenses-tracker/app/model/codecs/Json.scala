package model.codecs

import cats.Show

import java.time.LocalDateTime

sealed trait Json
object Json {
  final case object JsonNull extends Json
  final case class JsonString(value: String) extends Json
  final case class JsonInt(value: Int) extends Json
  final case class JsonLong(value: Long) extends Json
  final case class JsonDouble(value: Double) extends Json
  final case class JsonDateTime(value: LocalDateTime) extends Json
  final case class JsonArray(value: List[Json]) extends Json
  final case class JsonObject(value: Map[String, Json]) extends Json

  implicit val show: Show[Json] = {
    case JsonNull          => "null"
    case JsonString(str)   => s"\"$str\""
    case JsonInt(value)    => value.toString
    case JsonLong(value)   => value.toString
    case JsonDouble(value) => value.toString
    case JsonDateTime(value) =>
      s"${value.getYear}-${value.getMonth}-${value.getDayOfMonth} ${value.getHour}:${value.getMinute}:${value.getSecond}:${value.getNano}"
    case JsonArray(value) => (value map show.show).mkString("[", ", ", "]")
    case Json.JsonObject(value) =>
      (value map (entry => s"\"${entry._1}\": ${show.show(entry._2)}")).mkString("{\n", ",\n", "\n}")
  }
}
