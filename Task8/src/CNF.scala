object CNF {

  sealed trait CNF extends Ordered[CNF] {
    def compare(that: CNF): Int = {
      //TODO: finish CNF comparing, all are equal now
      0
    }
  }

  case class Atom(n: Integer) extends CNF {
    override def toString: String = n.toString
  }

  val zero = new Atom(0)
  val one = new Atom(1)

  case class CList(list: List[(CNF, Integer)], atom: Atom) extends CNF {
    if (list.size == 0) throw new Exception("nil in CList")

    override def toString: String =
      list.map(p => "(w "
        + (if (p._1 == one) ""
      else "^(" + p._1.toString + ")"
        + (if (p._2 == 1) "" else "*" + p._2.toString)
        + ")"))
        .mkString("+") + (if (atom == zero) "" else "+" + atom.toString)

  }

  def first(a: CNF): CNF = a match {
    case e: Atom => e
    case CList(list, p) => CList(list.head :: Nil, zero)
  }

  def rest(a: CNF): CNF = a match {
    case e: Atom => ???
    case CList(list, p) if list.length == 1 => p //WHY?
    case CList(list, p) => CList(list.tail, p)
  }

  def firstn(a: CNF, n: Int): CNF = a match {
    case t: Atom => t
    case CList(list, _) => n match {
      case _ if n > 1 => rest(firstn(a, n - 1)) match {
        case t: Atom => CList(list, t)
        case CList(list2, t) => CList(list ::: list2, t)
      }
      case 1 => rest(a) match {
        case t: Atom => CList(list, t)
        case CList(list2, _) => CList(list ::: list2, zero)
      }
      case _ => throw new Exception("n = 0 in firstn")
    }
  }

  def restn(a: CNF, n: Int): CNF = n match {
    case 0 => a
    case _ => rest(restn(a, n - 1))
  }

  def fe(a: CNF): CNF = a match {
    case Atom(_) => zero
    case CList(list, _) => list.head._1
  }

  def fc(a: CNF): Int = a match {
    case Atom(n) => n
    case CList(list, _) => list.head._2
  }

  def length(a: CNF): Int = a match {
    case Atom(_) => 0
    case CList(_, _) => length(rest(a)) + 1
  }

  def size(a: CNF): Int = a match {
    case Atom(_) => 1
    case p@CList(_, _) => size(fe(p)) + size(rest(p))
  }

  def limitp(a: CNF): Boolean = a match {
    case Atom(n) => n == 0
    case t: CList => limitp(rest(t))
  }

  def limitpart(a: CNF): CNF = a match {
    case Atom(n) => zero
    case CList(_, _) => first(a) match {
      case p@CList(list, _) => limitpart(p) match {
        case n: Atom => CList(list, n)
        case CList(list2, n) => CList(list ::: list2, n)
      }
    }
    case _ => throw new Exception("Pattern matching error in limitpart")
  }

  def natpart(a: CNF): CNF = a match {
    case t: Atom => t
    case _ => natpart(rest(a))
  }

  def +(a: CNF, b: CNF): CNF = a match {
    case Atom(n) if (b match {
      case Atom(m) => true
      case _ => false
    }) => Atom(n + b.asInstanceOf[Atom].n)
    case CList()
  }
}