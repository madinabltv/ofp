val mul: (List[Int], List[Int]) => List[Int] = {
    case (Nil, Nil) => Nil
    case (list1, Nil) => list1
    case (Nil, list2) => list2
    case (x :: xs, y :: ys) => (x + y) :: mul(xs, ys)
}

val a = List(1, 0, 2)
val b = List(1, 2)
val res = mul(a, b)
println(res)
