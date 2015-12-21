/**
  * Created by jesse on 12/21/15.
  */
object Day21Part1 {

  val store = Map(
    "weapons" -> List(
      new StoreItem("Dagger", 8, 4, 0),
      new StoreItem("Shortsword", 10, 5, 0),
      new StoreItem("Warhammer", 25, 6, 0),
      new StoreItem("Longsword", 40, 7, 0),
      new StoreItem("Greataxe", 74, 8, 0)
    ),
    "armor" -> List(
      new StoreItem("No Armor", 0, 0, 0),
      new StoreItem("Leather", 13, 0, 1),
      new StoreItem("Chainmail", 31, 0, 2),
      new StoreItem("Splintmail", 53, 0, 3),
      new StoreItem("Bandedmail", 75, 0, 4),
      new StoreItem("Platemail", 102, 0, 5)
    ),
    "rings" -> List(
      new StoreItem("No Left Ring", 0, 0, 0),
      new StoreItem("No Right Ring", 0, 0, 0),
      new StoreItem("Damage +1", 25, 1, 0),
      new StoreItem("Damage +2", 50, 2, 0),
      new StoreItem("Damage +3", 100, 3, 0),
      new StoreItem("Defense +1", 20, 0, 1),
      new StoreItem("Defense +2", 40, 0, 2),
      new StoreItem("Defense +3", 80, 0, 3)
    )
  )

  def main(args: Array[String]): Unit = {
    val input = scala.io.Source.fromFile("input/day21-input.txt").getLines()

    val Array(bossHP, bossDamage, bossArmor) = input.map(line => {
      line.split(": ").last.toInt
    }).toArray

    val combos = for (
      weapon <- store("weapons");
      armor <- store("armor");
      rings <- store("rings").combinations(2))
      yield weapon + armor + rings.reduce((ring1, ring2) => ring1 + ring2)

    val winningCombos = combos.filter(combo => {
      fight(bossHP, 100, bossDamage, bossArmor, combo)
    }).sortBy(_.cost)

    println(winningCombos.head.cost)
  }

  def fight(bossHP: Int, yourHP: Int, bossDamage: Int, bossArmor: Int, items: StoreItem): Boolean = {
    val dmgToBoss = Math.max(items.damage - bossArmor, 0)
    val dmgToYou = Math.max(bossDamage - items.armor, 0)

    if (bossHP - dmgToBoss <= 0) {
      true
    } else if (yourHP - dmgToYou <= 0) {
      false
    } else {
      fight(bossHP - dmgToBoss, yourHP - dmgToYou, bossDamage, bossArmor, items)
    }
  }

  case class StoreItem(name: String, cost: Int, damage: Int, armor: Int) {
    def +(that: StoreItem): StoreItem = {
      StoreItem(
        name = s"${this.name} & ${that.name}",
        cost = this.cost + that.cost,
        damage = this.damage + that.damage,
        armor = this.armor + that.armor
      )
    }
  }

}
