import com.github.darl.translate.FlowScalaGenerator
import scala.io.Source

object FlowGenerator extends App {

  if (args.size != 1) {
    println(
      """Usage: java $JAVA_PROPS FlowGenerator file.scala
        |
      """.stripMargin)
  } else {
    val file = args.head
    val source = Source.fromFile(file)

    val gen = new FlowScalaGenerator
    val problem = gen.generate(source.getLines().mkString("\n"), Map.empty)
    println("Result: ")
    println(problem)
  }
}
