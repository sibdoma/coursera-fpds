import scala.util.Random

trait Generator[+T] {
  self =>
  def generate: T
  def map[S](f: T => S): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate)
  }
  def flatMap[S](f: T => Generator[S]): Generator[S] = new Generator[S] {
    override def generate: S = f(self.generate).generate
  }
}
val integers = new Generator[Int] {
  var random = new Random
  override def generate: Int = random.nextInt
}

val booleans = for (x <- integers) yield x > 0
val pairs = for (x <- integers; y <- integers) yield (x, y)
