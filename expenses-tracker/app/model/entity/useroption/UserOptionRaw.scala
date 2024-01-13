package model.entity.useroption

import doobie.util.Write
import model.codecs.JsonReader
import model.codecs.JsonReader._

import java.time.LocalDateTime

final case class UserOptionRaw(key: String, value: String, userId: Long, updationTime: LocalDateTime) extends UserOption

object UserOptionRaw {
  implicit def userOptionRawReader: JsonReader[UserOptionRaw] = objectReader(map =>
    for {
      key <- getField[String](map, "key")
      value <- getField[String](map, "value")
      userId <- getField[Long](map, "userId")
      updationTime <- getField[LocalDateTime](map, "updationTime")
    } yield UserOptionRaw(key, value, userId, updationTime)
  )

  implicit val writeUserOptionRaw: Write[UserOptionRaw] =
    Write[(String, String, Long)].contramap { opt =>
      (opt.key, opt.value, opt.userId)
    }
}
