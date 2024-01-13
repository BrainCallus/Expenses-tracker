package model.entity.user

final case class UserWithIdAndPassword(id: Long, login: String, name: String, password: String) extends User
