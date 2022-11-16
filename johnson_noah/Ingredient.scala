package johnson_noah

import johnson_noah.XMLHelper
import scala.xml.{Elem, Node}
import scala.annotation.unused

abstract class Ingredient() extends XMLReadWrite {

  override def loadXML(n: Node): Unit
  override def writeXML(): Elem

  def findIngredient(ingredientToFind: String): Option[Ingredient]
  def getCalories: Double
  def getString(depth: Integer): String
  def getName: String
  def getVolume: Double
}
