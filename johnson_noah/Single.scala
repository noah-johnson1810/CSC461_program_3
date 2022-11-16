package johnson_noah
import johnson_noah.XMLHelper
import scala.collection.mutable
import scala.xml.{Elem, Node}
import java.text.DecimalFormat


class Single(private var name: String = "", private var numCalories: Int = 100,
             private var numCups: Double = 1) extends Ingredient {

  private val format = new DecimalFormat("0.##")

  def findIngredient(ingredientToFind: String): Option[Ingredient] = {
    if (ingredientToFind.toLowerCase() == name.toLowerCase()) {
      return Some(this)
    }
    null
  }

  def getCalories: Double = {
    numCalories.toDouble
  }

  def getName: String = name

  def getString(depth: Integer): String = {
    "\n" + "  " * depth + "______" + name + "______"
      + "\n" + "  " * depth + "Cups: " + format.format(numCups) +
      "\n" + "  " * depth + "Calories: " + numCalories
  }

  def getVolume: Double = {
    numCups.toDouble
  }

  override def loadXML(n: Node): Unit = {
    val singleName = n.text
    name = singleName
    val singleCalories = n.attribute("calories").getOrElse("100").toString
    numCalories = singleCalories.toDouble.round.toInt
    val singleCups = n.attribute("cups").getOrElse("1.0").toString
    numCups = singleCups.toDouble
  }

  override def writeXML(): Elem = {
    val attr1: mutable.HashMap[String, String] = mutable.HashMap(("cups", numCups.toString))
    val attr2: mutable.HashMap[String, String] = mutable.HashMap(("calories", numCalories.toString))
    val attributes = attr1 ++ attr2
    val text = scala.xml.Text(name)
    val singleXML = XMLHelper.makeNode("single", attributes, text)
    singleXML
  }
}