import scala.collection.immutable.SortedMap
class School {
  type DB = Map[Int, Seq[String]]

  var database: DB = Map.empty

  def add(name: String, g: Int) = {
    val updatedGrade = database.getOrElse(g, List.empty) :+ name
    database = database.updated(g, updatedGrade)
  }

  def db: DB = database

  def grade(g: Int): Seq[String] = database.getOrElse(g, List.empty)

  def sorted: DB = SortedMap.empty[Int, Seq[String]] ++ database.map { case (g, n) => g -> n.sorted }
}

