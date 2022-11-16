package johnson_noah

import johnson_noah.Recipe
import johnson_noah.Ingredient
import java.text.DecimalFormat

import scala.collection.mutable.ListBuffer
import scala.xml.{Elem, Node}

class RecipeBook extends XMLReadWrite {

  private val recipeList = ListBuffer[Recipe]()
  private val format = new DecimalFormat("0.0")

  def addRecipe(recipe: Recipe): Unit = {
    recipeList += recipe
  }

  def calcCalories(recipeToCalculate: String): Double = {
    recipeList.find(recipe => recipe.getName.toLowerCase() == recipeToCalculate.toLowerCase()).orNull.calcCalories
  }

  def calcCalorieDensity(): Unit = {
    for (recipe <- recipeList) {
      print(recipe.getName + ": " + format.format(recipe.calcDensity) + "\n")
    }
  }

  def calcVolume(recipeToCalculate: String): Double = {
    recipeList.find(recipe => recipe.getName.toLowerCase() == recipeToCalculate.toLowerCase()).orNull.calcVolume
  }

  def findRecipe(ingredientToFind: String): Option[Recipe] = {
    recipeList.find(recipe =>
      recipe.getName.toLowerCase() == ingredientToFind.toLowerCase()
        || recipe.findIngredient(ingredientToFind) != null
        && recipe.findIngredient(ingredientToFind).isDefined)
  }

  def getString: String = {
    for (recipe <- recipeList) {
      print(recipe.getString(0))
    }
    ""
  }

  def loadXML(n: Node): Unit = {
    val children = n.child
    children.foreach(child => {
      val tag = child.label
      if (tag == "recipe") {
        val newRecipe = new Recipe()
        newRecipe.loadXML(child)
        recipeList += newRecipe
      }
    })
  }

  def recipeExists(recipeName: String): Boolean = {
    var exists: Boolean = false
    for (recipe <- recipeList) {
      if (recipe.getName == recipeName) {
        exists = true
      }
    }
    exists
  }

  def removeRecipe(name: String): Boolean = {
    var found = false
    var removeIndex = -1
    for (recipe <- recipeList) {
      if (recipe.getName.toLowerCase() == name.toLowerCase()) {
        found = true
        removeIndex = recipeList.indexOf(recipe)
      }
    }
    if (found) {
      recipeList.remove(removeIndex)
    }
    found
  }

  def writeXML(): Elem = {
    val recipesXML = recipeList.map(x => x.writeXML())
    val recipeBookXML = XMLHelper.makeNode("recipebook", null, recipesXML)
    recipeBookXML
  }
}