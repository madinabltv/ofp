class SortedSeq[T: Ordering](val values: Seq[T]) {
  def merge(other: SortedSeq[T]): SortedSeq[T] = {
    new SortedSeq[T](values ++ other.values).sorted
  }

  def multiply(n: T)(implicit num: Numeric[T]): SortedSeq[T] = {
    import num._
    new SortedSeq[T](values.map(_ * n).sorted)
  }
  private def sorted: SortedSeq[T] = new SortedSeq[T](values.sorted)

  override def toString: String = values.mkString("SortedSeq(", ", ", ")")
}

object SortedSeq {
  def apply[T: Ordering](values: T*): SortedSeq[T] = new SortedSeq[T](values.sorted)
}

val seq1 = SortedSeq(1, 5, 3, 7)
val seq2 = SortedSeq(2, 4, 6, 8)
println(seq1.merge(seq2)) 

val seq3 = SortedSeq(4, 2.5, 3.5, 9, 1)
println(seq3.multiply(2)) 

val seq4 = SortedSeq('a', 'k' , 'g', 'w')
val seq5 = SortedSeq('a', 'm', 'i', 'v')
println(seq4.merge(seq5)) 

