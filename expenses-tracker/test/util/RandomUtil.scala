package util

import model.util.DateUtil.DateCalc

import java.time.LocalDate
import scala.util.Random

object RandomUtil {

  /** [min; max]
    * @param min
    *   minimum value
    * @param max
    *   maximum value
    * @return
    */
  def genRandUInt(min: Int = 0, max: Int = Int.MaxValue): Int =
    Random.nextInt(max - min + 1) + min

  def genRandDate(diffDays: Int, end: LocalDate): LocalDate = {
    genRandUInt(0, diffDays).daysBefore(end)
  }
}
