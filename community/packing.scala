// Spreads projects over shards by the time each took in the previous run.

val Overhead  = 120.0
val PerShard  = 20
val MaxShards = 256

def shardsOf(members: Int): Int = (members + PerShard - 1) / PerShard

/** The shards of `members` projects on the default profile when the other profiles hold `others` projects each, about
  * twenty projects a shard within the jobs the other profiles leave of a workflow run.
  */
def shardCount(members: Int, others: Vector[Int]): Int = shardsOf(members).min(MaxShards - others.map(shardsOf).sum)

def estimates(names: Vector[String], known: Map[String, Double]): Option[Vector[Double]] =
  Option.when(names.exists(known.contains)) {
    val mean = known.view.filterKeys(names.toSet).values.sum / names.count(known.contains)
    names.map(name => known.getOrElse(name, mean) + Overhead)
  }

def packed(estimated: Vector[Double], shards: Int): Vector[Int] = {
  val loads   = Array.fill(shards)(0.0)
  val chosen  = Array.fill(estimated.size)(0)
  val longest = estimated.indices.sortBy(i => (-estimated(i), i))
  longest.foreach { i =>
    val shard = loads.indices.minBy(s => (loads(s), s))
    loads(shard) += estimated(i)
    chosen(i) = shard
  }
  chosen.toVector
}

def spread(names: Vector[String], shards: Int, known: Map[String, Double]): Vector[Int] =
  estimates(names, known).fold(names.indices.map(_ % shards).toVector)(packed(_, shards))

def seconds(timings: String): Double =
  timings.linesIterator.flatMap(_.split('\t') match {
    case Array(_, value) => value.trim.toDoubleOption
    case _               => None
  }).sum
