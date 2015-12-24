/**
  * Created by jesse on 12/24/15.
  */
object Day22Part2 {

  val input = scala.io.Source.fromFile("input/day22-input.txt").getLines()

  val Array(bossHP, bossDamage) = input.map(line => {
    line.split(": ").last.toInt
  }).toArray

  val spells = Map(
    "Magic Missile" -> 53,
    "Drain" -> 73,
    "Shield" -> 113,
    "Poison" -> 173,
    "Recharge" -> 229
  )

  var minSuccessfulManaSpent = Int.MaxValue

  def main(args: Array[String]): Unit = {

    val winningRound = simulateRound(FightStatus(bossHP, 50, 500)).toList.sortBy(_.totalMana).head

    println(s"Min mana spent: ${winningRound.totalMana}")

    println(winningRound.actions.mkString(" -> "))

  }

  def simulateRound(fightStatus: FightStatus): Iterable[FightStatus] = {
    spells.flatMap(spell => {
      if (spell._2 <= fightStatus.currentMana) {
        val canCast = spell._1 match {
          case "Poison" => if (fightStatus.poisonRounds <= 1) true else false
          case "Shield" => if (fightStatus.shieldRounds <= 1) true else false
          case "Recharge" => if (fightStatus.rechargeRounds <= 1) true else false
          case _ => true
        }
        if (canCast) {
          List(simulatePlayerRound(fightStatus, spell))
        } else {
          List.empty[FightStatus]
        }
      } else {
        List.empty[FightStatus]
      }
    }).filter(_.playerHP > 0).filter(_.totalMana < minSuccessfulManaSpent).
      flatMap(status => {
        if (status.bossHP == 0) {
          // Boss is dead
          minSuccessfulManaSpent = Math.min(minSuccessfulManaSpent, status.totalMana)
          List(status)
        } else {
          val bossStatus = simulateBossRound(status)
          if (bossStatus.bossHP == 0) {
            // Boss is dead
            minSuccessfulManaSpent = Math.min(minSuccessfulManaSpent, status.totalMana)
            List(bossStatus)
          } else if (bossStatus.playerHP == 0) {
            // You're dead
            List.empty[FightStatus]
          } else {
            // Both players alive, keep simulating rounds
            simulateRound(bossStatus)
          }
        }
      })
  }

  def simulatePlayerRound(fightStatus: FightStatus, spell: (String, Int)): FightStatus = {
    if (fightStatus.playerHP - 1 == 0) {
      FightStatus(fightStatus.bossHP, 0, fightStatus.currentMana, fightStatus.totalMana,
        fightStatus.actions, 0, 0, 0)
    } else {
      val damageToBoss = (spell._1 match {
        case "Magic Missile" => 4
        case "Drain" => 2
        case _ => 0
      }) + (if (fightStatus.poisonRounds > 0) 3 else 0)

      FightStatus(
        Math.max(fightStatus.bossHP - damageToBoss, 0),
        fightStatus.playerHP + (if (spell._1 == "Drain") 2 else 0) - 1,
        fightStatus.currentMana + (if (fightStatus.rechargeRounds > 0) 101 else 0) - spell._2,
        fightStatus.totalMana + spell._2,
        fightStatus.actions :+ spell._1,
        if (spell._1 == "Poison") 6 else Math.max(fightStatus.poisonRounds - 1, 0),
        if (spell._1 == "Shield") 6 else Math.max(fightStatus.shieldRounds - 1, 0),
        if (spell._1 == "Recharge") 5 else Math.max(fightStatus.rechargeRounds - 1, 0))
    }
  }

  def simulateBossRound(fightStatus: FightStatus): FightStatus = {
    if (fightStatus.poisonRounds > 0 && fightStatus.bossHP <= 3) {
      // Poison kills boss
      FightStatus(0, fightStatus.playerHP, fightStatus.currentMana, fightStatus.totalMana,
        fightStatus.actions, 0, 0, 0)
    } else {
      val damageToPlayer = Math.max(bossDamage - (if (fightStatus.shieldRounds > 0) 7 else 0), 1)

      FightStatus(fightStatus.bossHP - (if (fightStatus.poisonRounds > 0) 3 else 0),
        Math.max(fightStatus.playerHP - damageToPlayer, 0),
        fightStatus.currentMana + (if (fightStatus.rechargeRounds > 0) 101 else 0),
        fightStatus.totalMana,
        fightStatus.actions,
        Math.max(fightStatus.poisonRounds - 1, 0),
        Math.max(fightStatus.shieldRounds - 1, 0),
        Math.max(fightStatus.rechargeRounds - 1, 0))
    }
  }

  case class FightStatus(bossHP: Int, playerHP: Int, currentMana: Int, totalMana: Int = 0,
                         actions: List[String] = List.empty[String],
                         poisonRounds: Int = 0, shieldRounds: Int = 0, rechargeRounds: Int = 0)

}
