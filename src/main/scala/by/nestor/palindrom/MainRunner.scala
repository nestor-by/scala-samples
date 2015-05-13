package by.nestor.palindrom

object MainRunner {

  case class Operation(a: Int, b: Int) {
    def result = a * b

    override def toString = s"$a * $b = $result"

    def /(c: Int) = result / c

    def >(c: Int) = result > c

    def >(c: Operation) = result > c.result
  }

  object Operation {
    def empty = Operation(0, 0)
  }

  def filter = { number: Int =>
    List(1, 3, 7, 9).contains(number % 10)
  }

  def main(args: Array[String]) {
    for (i <- 0 to 0) {
      val start = System.currentTimeMillis
      println(find(capacity = 4) + s" at ${System.currentTimeMillis - start} ms")
    }
  }

  def find(capacity: Int) = {
    val max = Math.pow(10, capacity).toInt - 1
    val min = Math.pow(10, capacity - 1).toInt
    (max to(min, -1)).view.withFilter(filter).foldLeft(Operation.empty) { (result, a) =>
      (a to(Math.max(min, result / a), -1))
        .withFilter(filter)
        .map(b => Operation(a, b))
        .find(x => x.result.palindrom && x > result)
        .getOrElse(result)
    }
  }

  implicit class PalindromImplicits(val self: Int) {
    def palindrom = {
      val str = self.toString
      str == str.reverse
    }
  }

}

