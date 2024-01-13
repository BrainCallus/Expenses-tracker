package model.entity.user

import model.codecs.Json.JsonObject
import model.codecs.JsonReader._
import model.codecs._
import model.codecs.JsonWriter.JsonWriterOps

final case class UserWithId(id: Long, login: String, name: String) extends User

object UserWithId {
  def apply(user: UserWithIdAndPassword): UserWithId = UserWithId(user.id, user.login, user.name)

  implicit def userWithIdWriter: JsonWriter[UserWithId] =
    user =>
      JsonObject(
        Map(
          "id" -> user.id.toJson,
          "login" -> user.login.toJson,
          "name" -> user.name.toJson
        )
      )

  implicit def userWithIdReader: JsonReader[UserWithId] = objectReader(map =>
    for {
      id <- getField[Long](map, "id")
      name <- getField[String](map, "name")
      login <- getField[String](map, "login")
    } yield UserWithId(id, login, name)
  )
}
