package kyo

extension (randoms: Randoms.type)
  def nextString(length: Int): String < Fibers =
    val lettersFiber = Fibers.parallel:
      Seq.fill(length):
        Randoms.nextValue('a' to 'z')

    lettersFiber.map { chars =>
      chars.mkString
    }

  def nextBytes(length: Int): Array[Byte] < Fibers =
    val random = java.util.Random()
    val array = Array.ofDim[Byte](length)
    random.nextBytes(array)
    IOs(array)
