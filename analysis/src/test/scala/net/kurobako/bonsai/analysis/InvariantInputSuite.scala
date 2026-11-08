package net.kurobako.bonsai.analysis

/** Calls of the file system or the network that give the same answer for every element of a traversal. */
class InvariantInputSuite extends AnalysisSuite {

  private lazy val probed = extract(program(
    "Probe",
    """import java.io.DataInputStream
      |import java.io.File
      |import java.nio.ByteBuffer
      |import java.nio.file.Files
      |import java.nio.file.Path
      |import java.nio.file.attribute.FileTime
      |object Probe {
      |  var count = 0
      |  def next(): Path = { count += 1; Path.of(count.toString) }
      |  def sizes(present: List[Path], sizes: Map[String, Long]): List[Path] =
      |    present.filter(p => sizes.values.exists(_ == Files.size(p)))
      |  def constant(names: List[String], root: Path): List[Boolean] = names.map(n => Files.exists(root) && n.nonEmpty)
      |  def instance(names: List[String], base: String): List[String] = names.filter(_ => new File(base).exists)
      |  def read(names: List[String], config: Path): List[(String, String)] =
      |    names.map(n => n -> Files.readString(config))
      |  def varying(paths: List[Path]): List[Long] = paths.map(p => Files.size(p))
      |  def derived(paths: List[Path]): List[Long] = paths.map(p => Files.size(p.resolve("x")))
      |  def bound(names: List[String], dir: Path): List[Boolean] = names.map { n => val q = dir.resolve(n); Files.exists(q) }
      |  def pattern(entries: List[(Path, Int)]): List[Long] = entries.map { case (p, _) => Files.size(p) }
      |  def stream(xs: List[Int], in: DataInputStream): List[Int] = xs.map(_ => in.readInt())
      |  def buffers(xs: List[Int]): List[ByteBuffer] = xs.map(_ => ByteBuffer.allocate(8))
      |  def times(xs: List[Long]): List[FileTime] = xs.map(_ => FileTime.fromMillis(0))
      |  def environment(xs: List[Int]): List[String] = xs.map(_ => System.getenv("HOME"))
      |  def written(xs: List[Int], p: Path): List[Path] = xs.map(_ => Files.writeString(p, ""))
      |  def single(p: Path): Long = Files.size(p)
      |  def option(o: Option[Int], p: Path): Option[Long] = o.map(_ => Files.size(p))
      |  def deferred(xs: List[Int], p: Path): List[() => Long] = xs.map(_ => () => Files.size(p))
      |  def cursor(xs: List[Int], paths: Array[Path]): List[Long] = {
      |    var i = 0
      |    xs.map { _ => i += 1; Files.size(paths(i)) }
      |  }
      |  def called(xs: List[Int]): List[Long] = xs.map(_ => Files.size(next()))
      |}""".stripMargin
  ))

  private lazy val invariant = Purity.findings(probed.purity, probed.graph.nodes).filter(_.rule.id == "invariant-input")

  test("a call of the file system whose arguments do not depend on the function's own is reported") {
    assertFinding(invariant, "invariant-input", "Probe.sizes", "Files.size(p)", "exists")(using Finding.Level.Info)
    assertFinding(invariant, "invariant-input", "Probe.constant", "Files.exists(root)", "map")(using Finding.Level.Info)
    assertFinding(invariant, "invariant-input", "Probe.instance", "exists", "filter")(using Finding.Level.Info)
    assertFinding(invariant, "invariant-input", "Probe.read", "Files.readString(config)", "map")(using
      Finding.Level.Info
    )
  }

  test("calls that depend on the element, are not input of the file system, or do not repeat are not reported") {
    assertEquals(invariant.size, 4, invariant.mkString("\n"))
  }
}
