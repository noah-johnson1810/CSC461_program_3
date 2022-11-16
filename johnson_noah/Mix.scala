package johnson_noah

import collection.parallel.CollectionConverters.seqIsParallelizable
import scala.collection.mutable
import scala.xml.{Elem, Node}

class Mix(private var name: String = "") extends Ingredient {

  private val ingredients = mutable.ListBuffer[Ingredient]()

  def addIngredient(newIngredient: Ingredient): Unit = {
    ingredients += newIngredient
  }

  def findIngredient(ingredientToFind: String): Option[Ingredient] = {
    if (ingredientToFind.toLowerCase() == name.toLowerCase()) {
      Some(this)
    }
    else {
      ingredients.find(i => i.findIngredient(ingredientToFind) != null
        && i.findIngredient(ingredientToFind).isDefined)
    }
  }

  def getCalories: Double = {
    ingredients.par.map(_.getCalories).sum
  }

  def getName: String = name

  def getString(depth: Integer): String = {
    var returnString = "\n" + "  " * depth + name
      + "\n" + "  " * depth + "*****************************"
    for (ingredient <- ingredients) {
      returnString += ingredient.getString(depth + 1)
    }
    returnString += "\n" + "  " * depth + "*****************************"
    returnString
  }

  def getVolume: Double = {
    ingredients.par.map(_.getVolume).sum
  }

  override def loadXML(n: Node): Unit = {
    val bakedName = n.attribute("name").getOrElse("").toString
    name = bakedName
    val children = n.child
    children.foreach(childNode => //check each child
    {
      val tag = childNode.label //what type of node
      if (tag == "mix") { //find matching node, and let it load itself
        val newMix = new Mix()
        newMix.loadXML(childNode)
        ingredients += newMix
      } else if (tag == "baked") {
        val newBaked = new Baked()
        newBaked.loadXML(childNode)
        ingredients += newBaked
      } else if (tag == "remeasure") {
        val newRemeasure = new Remeasure()
        newRemeasure.loadXML(childNode)
        ingredients += newRemeasure
      } else if (tag == "single") {
        val newSingle = new Single("", 0, 0)
        newSingle.loadXML(childNode)
        ingredients += newSingle
      }
    })
  }

  override def writeXML(): Elem = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", name))
    val ingredientsXML = ingredients.map(x => x.writeXML())
    val mixXML = XMLHelper.makeNode("mix", attr, ingredientsXML)
    mixXML
  }
}