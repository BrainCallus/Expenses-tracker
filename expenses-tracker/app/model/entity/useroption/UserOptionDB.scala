package model.entity.useroption

import doobie.util.Read
import model.codecs.JsonReader
import model.codecs.JsonReader.{getField, objectReader}
import model.entity.DatabaseEntity
import model.util.DateUtil

import java.time.{LocalDate, LocalDateTime}

final case class UserOptionDB(
  id: Long,
  key: String,
  value: String,
  userId: Long,
  updationTime: LocalDateTime,
  creationTime: LocalDate
) extends UserOption
  with DatabaseEntity

object UserOptionDB {

  implicit def userOptionDBReader: JsonReader[UserOptionDB] = objectReader(map =>
    for {
      id <- getField[Long](map, "id")
      key <- getField[String](map, "key")
      value <- getField[String](map, "value")
      userId <- getField[Long](map, "userId")
      updationTime <- getField[LocalDateTime](map, "updationTime")
      creationTime <- getField[LocalDateTime](map, "creationTime").map(_.toLocalDate)
    } yield UserOptionDB(id, key, value, userId, updationTime, creationTime)
  )

  implicit val readUserOptionDB: Read[UserOptionDB] =
    Read[(Long, String, String, Long, String, String)].map {
      case (id, key, value, userId, updationTime, creationTime) =>
        UserOptionDB(
          id,
          key,
          value,
          userId,
          DateUtil.dateTimeFromStringUnsafe(updationTime),
          DateUtil.localDateFromStringUnsafe(creationTime)
        )
    }

}
