package model.entity.pays

trait ScheduledPay extends PayType {
  def status: ScheduledPayStatus
}
