package johnson_noah
import scala.collection.mutable
import scala.xml.{Elem, Node}

class Remeasure(private var quantity: Double = 1.0, private var child: Ingredient = null) extends Ingredient {

  def findIngredient(ingredientToFind: String): Option[Ingredient] = {
    child.findIngredient(ingredientToFind)
  }

  def getCalories: Double = {
    child.getCalories * (quantity / child.getVolume)
  }

  def getName: String = {
    child.getName
  }

  def getString(depth: Integer): String = {
    "\n\n" + "  " * depth + "Remeasure to " + quantity + " cups" + child.getString(depth + 1)
  }

  def getVolume: Double = {
    quantity.toDouble
  }

  override def loadXML(n: Node): Unit = {
    val remeasureQuantity = n.attribute("quantity").getOrElse("1.0").toString
    quantity = remeasureQuantity.toDouble
    val children = n.child
    children.foreach(childNode => //check each child
    {
      val tag = childNode.label //what type of node
      if (tag == "mix") {
        val newMix = new Mix()
        newMix.loadXML(childNode)
        child = newMix
      } else if (tag == "baked") {
        val newBaked = new Baked()
        newBaked.loadXML(childNode)
        child = newBaked
      } else if (tag == "remeasure") {
        val newRemeasure = new Remeasure()
        newRemeasure.loadXML(childNode)
        child = newRemeasure
      } else if (tag == "single") {
        val newSingle = new Single("", 0, 0)
        newSingle.loadXML(childNode)
        child = newSingle
      }
    })
  }

  override def writeXML(): Elem = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("quantity", quantity.toString))
    val ingredientXML = child.writeXML()
    val remeasureXML = XMLHelper.makeNode("remeasure", attr, ingredientXML)
    remeasureXML
  }
}