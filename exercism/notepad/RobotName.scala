import util.Random

class Robot() {
  private def createName = {
    val randomletters = ('A' + Random.nextInt(26)).toChar
    val randomNum = "%03d".format(Random.nextInt(999))
    s"${randomletters}${randomletters}${randomNum}"
  }

  private var n = createName
  def name = n
  def reset() = n = createName
}
