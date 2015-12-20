/**
  * Created by jesse on 12/20/15.
  */
object Day20Part1 {

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day20-input.txt").getLines()
    val presentsDeliveredCheck = input.next().toInt

    // This was my first attempt but super slow
    // Calculates each house one by one
    //println(findByHouse(presentsDeliveredCheck))

    // Second attempt, super fast
    // Iterates over the elves and increments each house's presents
    println(findByElf(presentsDeliveredCheck))

  }

  def findByHouse(presentsDeliveredCheck: Int): Int = {
    Stream.from(1).foreach(house => {
      val presentsDelivered = (1 to house).filter(house % _ == 0).map(_ * 10).sum
      println(s"house $house got $presentsDelivered presents")

      if (presentsDelivered >= presentsDeliveredCheck) {
        return house
      }
    })

    -1
  }

  def findByElf(presentsDeliveredCheck: Int): Int = {
    val houses = Array.fill(presentsDeliveredCheck / 10)(0)
    (1 to presentsDeliveredCheck / 10).foreach(elf => {
      (elf until houses.length by elf).foreach(house => {
        houses(house) += (elf * 10)
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
