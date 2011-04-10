
import sbt._
import com.twitter.sbt._

class Project(info: ProjectInfo) extends StandardLibraryProject(info) {
  val mockito = "org.mockito" % "mockito-all" % "1.8.5" % "test" withSources()
  val specs = "org.scala-tools.testing" % "specs_2.8.1" % "1.6.7" % "test" withSources()
}

