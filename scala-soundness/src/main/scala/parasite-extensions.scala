import soundness.*
import parasite.{AsyncError, Monitor, Task, race}

extension [ResultType](tasks: Iterable[Task[ResultType]])
  def raceAndCancelLosers()(using Monitor, Codicil): ResultType raises AsyncError =
    val result = tasks.race()
    tasks.each(task => if !task.ready then task.cancel())
    result
