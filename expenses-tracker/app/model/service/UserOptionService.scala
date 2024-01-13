package model.service

import cats.effect.IO
import model.dao.io.{DbIOProvider, UserOptionDao}
import model.entity.useroption.UserOptionRaw
import model.service.CommonService.actionFromOption

object UserOptionService {
  def setOption(userId: Long, key: String, value: String): IO[Unit] = {
    actionFromOption(UserOptionDao.findByKeyAndUserId(key, userId))(
      UserOptionDao.insert(UserOptionRaw(key, value, userId, DbIOProvider.findNow()))
    )(option => UserOptionDao.update(option, value))
  }
}
