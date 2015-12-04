/**
  * Created by jesse on 12/4/15.
  */
object Day04Part1Short {

  def main (args: Array[String]) {

    val input = scala.io.Source.fromFile("input/day04-input.txt").getLines().next()
    println(Stream.from(0).dropWhile(
      curVal => !java.security.MessageDigest.getInstance("MD5").digest(s"$input$curVal".getBytes).
        map("%02X".format(_)).mkString.startsWith("00000")).head)

  }

}
