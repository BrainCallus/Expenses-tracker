package util.account

import util.CommonTestUtils.{getToday, getYesterday}
import model.entity.pays.{ExpenseFull, ScheduledPayFull}
import util.pays.TestPays.paysInSegment

import java.time.LocalDate

final case class AccountWithPays(
  id: Long,
  login: String,
  name: String,
  expenses: List[ExpenseFull],
  scheduledPays: List[ScheduledPayFull]
) extends TestUserAccount {

  /** in segment [today; endPeriod]
    *
    * @param endPeriod
    *   date until witch take following pays
    * @return
    */
  def getFollowPays(endPeriod: LocalDate = LocalDate.of(2199, 1, 1)): List[ScheduledPayFull] =
    paysInSegment[ScheduledPayFull](scheduledPays, getToday, endPeriod)

  def getPastTermPays(start: LocalDate = LocalDate.of(1999, 1, 1)): List[ScheduledPayFull] =
    paysInSegment[ScheduledPayFull](scheduledPays, start, getYesterday)

}

object AccountWithPays {
  def apply(user: BlankUserAccount, expenses: List[ExpenseFull], pays: List[ScheduledPayFull]) =
    new AccountWithPays(user.id, user.login, user.name, expenses, pays)
}
