package fix

import java.nio.charset.StandardCharsets
import java.nio.file.Files
import org.scalatest.funsuite.AnyFunSuite
import scala.io.Source
import scala.util.Using
import scalafix.testkit.TestkitPath
import scalafix.testkit.TestkitProperties

/** Guards the testkit against a rule being registered without a fixture, which runAllTests would silently skip. */
class RuleRegistrationSuite extends AnyFunSuite {
  test("every registered rule is named by at least one fixture's header") {
    val fixtures = TestkitPath.fromProperties(TestkitProperties.loadFromResources()).map { test =>
      val text   = Files.readString(test.input.toNIO)
      val header = text.linesIterator.takeWhile(!_.contains("*/")).toVector
      test.testName -> header.collectFirst {
        case line if line.trim.matches("""rules?\s*=.*""") => line.split("=", 2)(1).trim
      }
    }
    assert(fixtures.forall(_._2.isDefined), fixtures.filter(_._2.isEmpty).mkString("\n"))
    val declared =
      fixtures.flatMap(_._2.toList).flatMap(_.split(',').map(_.trim.stripPrefix("[").stripSuffix("]"))).toSet
    val registered = Using.resource(
      getClass.getClassLoader.getResourceAsStream("META-INF/services/scalafix.v1.Rule")
    )(stream =>
      Source.fromInputStream(stream, StandardCharsets.UTF_8.name).getLines().map(_.trim).filter(_.nonEmpty)
        .map(_.split('.').last).toSet
    )
    assert(
      registered.diff(declared).isEmpty,
      s"rules without a fixture: ${registered.diff(declared).toVector.sorted.mkString(", ")}"
    )
  }
}
