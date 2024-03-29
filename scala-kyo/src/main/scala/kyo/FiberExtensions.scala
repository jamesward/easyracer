package kyo

import kyo.scheduler.{IOPromise, IOTask}

import scala.util.{Success, Try}

// copied from kyo.Fibers
private inline def foreach[T, U](l: Seq[T])(inline f: (Int, T) => Unit): Unit =
  l match
    case l: IndexedSeq[?] =>
      var i = 0
      val s = l.size
      while i < s do
        f(i, l(i))
        i += 1
    case _ =>
      val it = l.iterator
      var i  = 0
      while it.hasNext do
        f(i, it.next())
        i += 1

extension (fibers: Fibers.type)
  def raceSuccesses[T](l: Seq[T < Fibers])(using f: Flat[T < Fibers]): T < Fibers =
    l.size match
      case 0 => IOs.fail("Can't race an empty list.")
      case 1 => l(0)
      case _ =>
        Fibers.get(raceFiberSuccesses[T](l))

  def raceSuccesses[T](
                        v1: => T < Fibers,
                        v2: => T < Fibers
                      )(using f: Flat[T < Fibers]): T < Fibers =
    raceSuccesses(Seq(v1, v2))

  def raceFiberSuccesses[T](l: Seq[T < Fibers])(using f: Flat[T < Fibers]): Fiber[T] < IOs =
    l.size match
      case 0 => IOs.fail("Can't race an empty list.")
      case 1 => Fibers.run(l(0))
      case _ =>
        Locals.save.map { st =>
          IOs {
            val p = new IOPromise[T]
            foreach(l) { (i, io) =>
              val f = IOTask(IOs.attempt(io), st)
              p.interrupts(f)
              f.onComplete { tv =>
                IOs.run(tv).foreach { v =>
                  discard(p.complete(v))
                }
              }
            }
            Promise(p)
          }
        }
