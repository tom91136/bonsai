package net.kurobako.bonsai.analysis

import java.nio.file.Path
import net.kurobako.bonsai.analysis.ClassFiles.*
import scala.util.Using

/** Suites that read the call graph of a compiled fixture beside its extracted model. */
trait Graphed extends AnalysisSuite {

  private def withWorld[A](directories: Vector[Path])(body: ClassWorld => A): A =
    ClassSource.of(directories, classpath, Some(Path.of(sys.props("java.home")))) match {
      case Left(error)   => fail(error)
      case Right(source) => Using.resource(source)(source => body(ClassWorld(source.bytes(_))))
    }

  /** The model, call graph and class world of a build, rooted as the analysis roots it and at `rooted` beside. */
  protected def graphed[A](
      main: Seq[(String, String)],
      test: Seq[(String, String)] = Nil,
      hot: Vector[String] = Vector.empty,
      rooted: Vector[Reference] = Vector.empty
  )(use: (Extraction.Result, CallGraph, ClassWorld) => A): A =
    inspect(main, test, hot = hot)((result, directories) =>
      withWorld(directories) { world =>
        use(result, CallGraph.build(world, CallGraph.roots(result.graph, world) ++ rooted), world)
      }
    )

}
