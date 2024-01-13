package model.entity.user

import model.codecs.JsonReader
import model.codecs.JsonReader.{getField, objectReader}

final case class UserWithPassword(login: String, name: String, password: String) extends User

object UserWithPassword {
  implicit def userWithPasswordReader: JsonReader[UserWithPassword] = objectReader(map =>
    for {
      name <- getField[String](map, "name")
      login <- getField[String](map, "login")
      password <- getField[String](map, "password")
    } yield UserWithPassword(login, name, password)
  )
}
