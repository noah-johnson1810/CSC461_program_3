package johnson_noah
import scala.collection.mutable
import scala.xml.{Elem, Node}

class Baked(private var name: String = "", private var expansionFactor: Double = 1.0,
            private var child: Ingredient = null) extends Ingredient {

  def findIngredient(ingredientToFind: String): Option[Ingredient] = {
    if (ingredientToFind.toLowerCase() == getName.toLowerCase()) {
      Some(this)
    }
    else {
      child.findIngredient(ingredientToFind)
    }
  }

  def getCalories: Double = {
    child.getCalories
  }

  def getString(depth: Integer): String = {
    var printName = name
    if (name == "") {
      printName = "baked " + child.getName
    }
    "\n" + "  " * depth + printName + " (" + expansionFactor + ")" + child.getString(depth + 1)
  }

  def getVolume: Double = {
    child.getVolume * expansionFactor
  }

  def getName: String = {
    if (name == "") "baked " + child.getName else name
  }

  override def loadXML(n: Node): Unit = {
    val bakedName = n.attribute("name").getOrElse("").toString
    name = bakedName
    val bakedExpansionFactor = n.attribute("expansion").getOrElse("1.0").toString
    expansionFactor = bakedExpansionFactor.toDouble
    val children = n.child
    children.foreach(childNode => //check each child
    {
      val tag = childNode.label //what type of node
      if (tag == "mix") { //find matching node, and let it load itself
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
    val attr1: mutable.HashMap[String, String] = mutable.HashMap(("name", name))
    val attr2: mutable.HashMap[String, String] = mutable.HashMap(("expansion", expansionFactor.toString))
    val attr = attr1 ++ attr2
    val childXML = child.writeXML()
    val bakedXML = XMLHelper.makeNode("baked", attr, childXML)
    bakedXML
  }
}