import sbt._
import xsbt._

class CannyEdgeProject(info: ProjectInfo) extends DefaultProject(info) {
  override def compileOptions = Seq(Optimize) ++ super.compileOptions
}
