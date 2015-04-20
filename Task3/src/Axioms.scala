object Axioms {
  def checker(e: Expression): Int = {
    e match {
      case Implication(left, Implication(_, right)) if left == right => 0
      case Implication(impl2 : Implication, Implication(Implication(impl4left, impl5: Implication), impl6: Implication))
        if (impl2.left == impl4left) &&
          (impl2.left == impl6.left) &&
          (impl2.right ==  impl5.left) &&
          (impl5.right == impl6.right) => 1
      case Implication(a, Implication(b, Conjunction(c, d))) if (a==c) && (b==d) => 2
      case Implication(a: Conjunction, b) if b == a.left => 3
      case Implication(Conjunction(_, a), b) if a == b => 4
      case Implication(a, Disjunction(b, _)) if a == b => 5
      case Implication(a, Disjunction(_, b)) if a == b => 6
      case Implication(Implication(i2l, i2r), Implication(Implication(i4l, i4r), Implication(Disjunction(d1l, d1r), i5r)))
        if (i2l==d1l) && (i2r == i4r) && (i2r == i5r) && (i4l == d1r) => 7
      case Implication(Implication(i2l, i2r), Implication(Implication(i4l, Negation(n1)), Negation(n2)))
        if (n2==i2l) && (n2==i4l) && (i2r == n1) => 8
      case Implication(Negation(Negation(n)), i1r) if n == i1r => 9
      case _ => -1
    }
  }
}
