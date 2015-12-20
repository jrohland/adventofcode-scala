/**
  * Created by jesse on 12/20/15.
  */
object Day20Part2 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day20-input.txt").getLines()
    val presentsDeliveredCheck = input.next().toInt

    println(findByElf(presentsDeliveredCheck))
  }

  def findByElf(presentsDeliveredCheck: Int): Int = {
    val houses = Array.fill(presentsDeliveredCheck / 10)(0)
    (1 to presentsDeliveredCheck / 10).foreach(elf => {
      (1 to 50).foreach(house => {
        if ((house * elf) < houses.length) {
          houses(house * elf) += (elf * 11)
        }
      })

      if (houses(elf) >= presentsDeliveredCheck) {
        println(s"hit present limit at $elf")
        return elf
      } else {
        println(s"house $elf got ${houses(elf)} presents")
      }
    })

    -1
  }

}
