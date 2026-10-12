/*
rule = ApiConsistency
 */
package fix

object ApiConsistency {
  final class Endpoint[I, O](val path: String)
  type AnyEndpoint = Endpoint[?, ?]

  object Endpoints {
    val user: Endpoint[Long, String]       = Endpoint("user")
    val illust: Endpoint[Long, Int]        = Endpoint("illust")
    val novel: Endpoint[String, String]    = Endpoint("novel")
    val forgotten: Endpoint[Long, Boolean] = Endpoint("forgotten") // assert: ApiConsistency.registry-drift
    val base: Endpoint[Unit, Unit]         = Endpoint("base")
    val derived: Endpoint[Unit, String]    = Endpoint(base.path + "/derived")
    private val hidden: Endpoint[Int, Int] = Endpoint("hidden")
    val limit: Int                         = 10
    val other: Int                         = 20

    val all: Vector[AnyEndpoint]  = Vector(user, illust, derived)
    val more: Vector[AnyEndpoint] = Vector(novel, hidden)
    val numbers: List[Int]        = List(limit, 1)
  }

  sealed trait Filter
  object Filter { case object Ios extends Filter }

  class Client {
    def illusts(user: Long, offset: Option[Long] = None, filter: Filter = Filter.Ios): Int    = 1
    def novels(user: Long, offset: Option[Long] = None, filter: Filter = Filter.Ios): Int     = 2
    def bookmarks(user: Long, offset: Long = 0L): Int                                         = 3 // assert: ApiConsistency.inconsistent-parameter
    def ranking(offset: Option[Long] = None, filter: Option[Filter] = None): Int              = 4 // assert: ApiConsistency.inconsistent-parameter
    def search(word: String, offset: Option[Long] = Some(0L)): Int                            = 5 // assert: ApiConsistency.inconsistent-parameter
    def detail(id: Long, size: Int): Int                                                      = 6
    def widths(size: Long): Int                                                               = 10
    def heights(size: Long): Int                                                              = 11
    def series(id: String): Int                                                               = 7
    def overloaded(user: String): Int                                                         = 8
    def overloaded(user: String, count: Int): Int                                             = 9
  }

  trait Fetcher {
    def illusts(user: Long, offset: Option[Long] = None): Int  = 1
    def novels(user: Long, offset: Option[Long] = None): Int   = 2
    def bookmarks(user: Long, offset: Long = 0L): Int          = 3 // assert: ApiConsistency.inconsistent-parameter
  }

  object Handlers {
    val first: Endpoint[Int, Int]    = Endpoint("first")
    val second: Endpoint[Int, Int]   = Endpoint("second")
    val third: Endpoint[Int, Int]    = Endpoint("third") // assert: ApiConsistency.registry-drift
    val registered: Seq[AnyEndpoint] = Seq(first, second)
  }

  object Stages {
    val one: Endpoint[Long, Long]        = Endpoint("one")
    val two: Endpoint[Long, Long]        = Endpoint("two")
    val three: Endpoint[Long, Long]      = Endpoint("three") // assert: ApiConsistency.registry-drift
    val listed: Set[AnyEndpoint]         = Set.apply(one, two)
    val indexed: IndexedSeq[AnyEndpoint] = IndexedSeq(one, two)
  }
}
