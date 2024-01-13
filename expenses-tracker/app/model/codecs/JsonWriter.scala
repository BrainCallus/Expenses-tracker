package model.codecs

import model.codecs.Json._

import java.time.LocalDateTime

trait JsonWriter[A] {
  def write(a: A): Json
}

object JsonWriter {
  def apply[A](implicit writer: JsonWriter[A]): JsonWriter[A] = writer

  implicit class JsonWriterOps[A](val a: A) {
    def toJson(implicit jsonWriter: JsonWriter[A]): Json = jsonWriter.write(a)
  }

  implicit val stringWriter: JsonWriter[String] = JsonString(_)

  implicit val intWriter: JsonWriter[Int] = JsonInt(_)

  implicit val longWriter: JsonWriter[Long] = JsonLong(_)

  implicit val doubleWriter: JsonWriter[Double] = JsonDouble(_)

  implicit val dateWriter: JsonWriter[LocalDateTime] = JsonDateTime(_)

  implicit val noneWriter: JsonWriter[None.type] = _ => JsonNull

  implicit def listWriter[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[List[A]] =
    a => JsonArray({ a.map(jsonWriter.write) })

  implicit def optionWriter[A](implicit jsonWriter: JsonWriter[A]): JsonWriter[Option[A]] = {
    case Some(v) => jsonWriter.write(v)
    case None    => JsonNull
  }
}
