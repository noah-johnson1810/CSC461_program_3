package johnson_noah

import scala.xml._

trait XMLReadWrite {
  def loadXML(n: Node): Unit
  def writeXML(): Elem
}
