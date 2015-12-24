/**
  * Created by jesse on 12/23/15.
  */
object Day22Part1 {

  case class FightStatus(bossHP: Int, yourHP: Int, currentMana: Int, totalManaSpent: Int = 0,
                         previousActions: List[String] = List.empty,
                         activeShieldTurns: Int = 0, activePoisonTurns: Int = 0, activeRechargeTurns: Int = 0)

  val input = scala.io.Source.fromFile("input/day22-input.txt").getLines()

  val Array(bossHP, bossDamage) = input.map(line => {
    line.split(": ").last.toInt
  }).toArray

  var minManaSpent = Int.MaxValue

  def main(args: Array[String]): Unit = {
    val winningRounds = fight(FightStatus(bossHP = bossHP, yourHP = 50, currentMana = 500)).
      sortBy(_.totalManaSpent)

    println(s"Minimal mana spent: $minManaSpent")

    winningRounds.head.previousActions.
      foldLeft(FightStatus(bossHP = bossHP, yourHP = 50, currentMana = 500))((status, spell) => {
        val playerStatus = calculatePlayerRound(status, spell, showStatus = true)
        val bossStatus = calculateBossRound(playerStatus, showStatus = true)
        bossStatus
      })
  }

  def calculatePlayerRound(fightStatus: FightStatus, spell: String, showStatus: Boolean = false): FightStatus = {

    if (showStatus) {
      println("-- Player turn --")
      printStatus(fightStatus)
      println(s"Player casts $spell.")
      println
    }

    val manaCost = spell match {
      case "Magic Missle" => 53
      case "Drain" => 73
      case "Shield" => 113
      case "Poison" => 173
      case "Recharge" => 229
    }

    val manaRecharge = fightStatus.activeRechargeTurns match {
      case i if i >= 1 => 101
      case _ => 0
    }

    val dmgToBoss = (spell match {
      case "Magic Missle" => 4
      case "Poison" => 3
      case "Drain" => 2
      case _ => 0
    }) + (fightStatus.activePoisonTurns match {
      case i if i >= 1 => 3
      case _ => 0
    })

    FightStatus (
      bossHP = Math.max(fightStatus.bossHP - dmgToBoss, 0),
      yourHP = fightStatus.yourHP + (if (spell == "Drain") 2 else 0),
      currentMana = fightStatus.currentMana + manaRecharge - manaCost,
      fightStatus.totalManaSpent + manaCost,
      fightStatus.previousActions :+ spell,
      if (spell == "Shield") 6 else Math.max(fightStatus.activeShieldTurns - 1, 0),
      if (spell == "Poison") 5 else Math.max(fightStatus.activePoisonTurns - 1, 0),
      if (spell == "Recharge") 5 else Math.max(fightStatus.activeRechargeTurns - 1, 0)
    )
  }

  def calculateBossRound(fightStatus: FightStatus, showStatus: Boolean = false): FightStatus = {
    if (showStatus) {
      println("-- Boss turn --")
      printStatus(fightStatus)
      if (fightStatus.activeShieldTurns > 0) {
        println(s"Boss attacks for $bossDamage - 7 = ${bossDamage - 7} damage!")
      } else {
        println(s"Boss attacks for $bossDamage damage.")
      }
      println
    }

    val manaRecharge = fightStatus.activeRechargeTurns match {
      case i if i >= 1 => 101
      case _ => 0
    }

    val dmgToBoss = fightStatus.activePoisonTurns match {
      case i if i >= 1 => 3
      case _ => 0
    }

    val dmgToYou = if (fightStatus.bossHP - dmgToBoss <= 0) {
      0
    } else {
      Math.max(bossDamage - (if (fightStatus.activeShieldTurns >= 2) 7 else 0), 1)
    }

    FightStatus (
      bossHP = Math.max(fightStatus.bossHP - dmgToBoss, 0),
      yourHP = Math.max(fightStatus.yourHP - dmgToYou, 0),
      currentMana = fightStatus.currentMana + manaRecharge,
      fightStatus.totalManaSpent,
      fightStatus.previousActions,
      Math.max(fightStatus.activeShieldTurns - 1, 0),
      Math.max(fightStatus.activePoisonTurns - 1, 0),
      Math.max(fightStatus.activeRechargeTurns - 1, 0)
    )
  }

  def fight(fightStatus: FightStatus): List[FightStatus] = {
    val canCastMagicMissle = fightStatus.currentMana >= 53
    val canCastDrain = fightStatus.currentMana >= 73
    val canCastShield = fightStatus.currentMana >= 113 && fightStatus.activeShieldTurns <= 0
    val canCastPoison = fightStatus.currentMana >= 173 && fightStatus.activePoisonTurns <= 0
    val canCastRecharge = fightStatus.currentMana >= 229 && fightStatus.activeRechargeTurns <= 0

    val magicMissleResult = if (canCastMagicMissle) {
      Option(calculatePlayerRound(fightStatus, "Magic Missle"))
    } else {
      None
    }

    val drainResult = if (canCastDrain) {
      Option(calculatePlayerRound(fightStatus, "Drain"))
    } else {
      None
    }

    val shieldResult = if (canCastShield) {
      Option(calculatePlayerRound(fightStatus, "Shield"))
    } else {
      None
    }

    val poisonResult = if (canCastPoison) {
      Option(calculatePlayerRound(fightStatus, "Poison"))
    } else {
      None
    }

    val rechargeResult = if (canCastRecharge) {
      Option(calculatePlayerRound(fightStatus, "Recharge"))
    } else {
      None
    }

    List(magicMissleResult, drainResult, shieldResult, poisonResult, rechargeResult).flatMap(result => {
      result match {
        case Some(r) =>
          if (r.bossHP == 0) {
            // Winner
            if (r.totalManaSpent > minManaSpent) {
              // We already have a solution that spent less mana
              List.empty[FightStatus]
            } else {
              minManaSpent = r.totalManaSpent
              List(r)
            }
          } else {
            if (r.totalManaSpent < minManaSpent) {
              val bossResult = calculateBossRound(r)

              if (bossResult.bossHP == 0 && bossResult.yourHP > 0) {
                // Winner
                if (bossResult.totalManaSpent > minManaSpent) {
                  // We already have a solution that spent less mana
                  List.empty[FightStatus]
                } else {
                  minManaSpent = r.totalManaSpent
                  List(bossResult)
                }
              } else if (bossResult.yourHP == 0) {
                // Loooooooser
                List.empty[FightStatus]
              } else {
                fight(bossResult)
              }
            } else {
              List.empty[FightStatus]
            }
          }
        case None => List.empty[FightStatus]
      }
    })

  }

  def printStatus(status: FightStatus) = {
    println(s"- Player has ${status.yourHP} hit points, ${if (status.activeShieldTurns > 0) 7 else 0} armor, ${status.currentMana} mana")
    println(s"- Boss has ${status.bossHP} hit points")
    if (status.activePoisonTurns > 0) {
      println(s"Poison deals 3 damage; its timer is now ${status.activePoisonTurns - 1}.")
    }
    if (status.activeRechargeTurns > 0) {
      println(s"Recharge provides 101 mana; its timer is now ${status.activeRechargeTurns - 1}.")
    }
    if (status.activeShieldTurns > 0) {
      println(s"Shield's timer is now ${status.activeShieldTurns - 1}.")
    }
  }

}
