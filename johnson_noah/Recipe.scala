package johnson_noah

import johnson_noah.XMLReadWrite
import johnson_noah.Ingredient
import collection.parallel.CollectionConverters.seqIsParallelizable
import scala.collection.mutable
import scala.xml.{Elem, Node}
import johnson_noah.Mix

class Recipe(private var name: String = "") extends XMLReadWrite {

  private val ingredients = mutable.ListBuffer[Ingredient]()

  def addIngredient(i: Ingredient): Unit = {
    ingredients += i
  }

  def calcCalories: Double = {
    // GRADING: PARALLEL
    ingredients.par.map(_.getCalories).sum
  }

  def calcDensity: Number = {
    calcCalories / calcVolume
  }

  def calcVolume: Double = {
    ingredients.par.map(_.getVolume).sum
  }

  def findIngredient(ingredientToFind: String): Option[Ingredient] = {
    ingredients.find(i => i.findIngredient(ingredientToFind) != null
      && i.findIngredient(ingredientToFind).isDefined)
  }

  def getName: String = this.name

  def getString(depth: Integer): String = {
    var returnString = "\nRecipe: " + getName + "\n=================================="
    for (ingredient <- ingredients) {
      returnString = returnString + ingredient.getString(depth + 1)
    }
    returnString
  }

  override def loadXML(n: Node): Unit = {
    val recipeName = n.attribute("name").getOrElse("").toString
    name = recipeName
    val children = n.child
    children.foreach(child =>
    {
      val tag = child.label
      if (tag == "mix") {
        val newMix = new Mix()
          newMix.loadXML(child)
        ingredients += newMix
      } else if (tag == "baked") {
        val newBaked = new Baked()
          newBaked.loadXML(child)
        ingredients += newBaked
      } else if (tag == "remeasure") {
        val newRemeasure = new Remeasure()
          newRemeasure.loadXML(child)
        ingredients += newRemeasure
      } else if (tag == "single") {
        val newSingle = new Single()
          newSingle.loadXML(child)
        ingredients += newSingle
      }
    })
  }

  override def writeXML(): Elem = {
    val attr: mutable.HashMap[String, String] = mutable.HashMap(("name", name))
    val ingredientsXML = ingredients.map(x => x.writeXML())
    val recipeXML = XMLHelper.makeNode("recipe", attr, ingredientsXML)
    recipeXML
  }
}