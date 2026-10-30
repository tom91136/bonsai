package fix

object ShortBlockOnOneLine {
  def pick(process: Process, alive: Boolean): Int =
    if (alive)
    {
      process.destroyForcibly()
      -1
    }
    else 0

  def conditioned(process: Process, alive: Boolean): Int =
    if (
      alive
    ) { process.destroyForcibly(); -1 }
    else 0

  def stop(process: Process, alive: Boolean): Int =
    if (alive) 1
    else { process.destroyForcibly(); -1 }

  def nested(args: List[String], alive: Boolean): Int =
    if (alive) 1
    else {
      args.foreach { arg => println(arg) }
      -1
    }

  def commented(process: Process, alive: Boolean): Int =
    if (alive) 1
    else {
      // keep the exit code
      process.destroyForcibly()
      -1
    }

  def three(process: Process, alive: Boolean): Int =
    if (alive) 1
    else {
      process.destroyForcibly()
      process.destroyForcibly()
      -1
    }

  def declared(alive: Boolean): Int =
    if (alive) 1
    else {
      val code = -1
      code
    }

  def single(process: Process, alive: Boolean): Int =
    if (alive) 1
    else { process.destroyForcibly(); -1 }

  def long(process: Process, alive: Boolean): Int =
    if (alive) 1
    else {
      process.destroyForcibly()
      Integer.MIN_VALUE + Integer.MIN_VALUE + Integer.MIN_VALUE + Integer.MIN_VALUE + Integer.MIN_VALUE + Integer.MIN_VALUE
    }
}
