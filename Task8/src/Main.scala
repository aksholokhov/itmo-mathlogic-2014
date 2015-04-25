import scala.language.reflectiveCalls

class Main extends App {

  val str = "w^(w*2+3)=2*(3^w+5^w)".split("\\=")
  val parser = new OrdinalParser
  println(parser.parseAll(parser.expr, str(0)))
  println(parser.parseAll(parser.expr, str(1)))

  sealed trait CNF extends Ordered[CNF] {
    def compare(that: CNF): Int = {
      //TODO: finish CNF comparing, all are equal now
      0
    }


    def +(b: CNF): CNF = this match { case Atom(n) if (b match {
        case Atom(m) => true
        case _ => false
      }) => Atom(n + b.asInstanceOf[Atom].n)
      case _ => if (fe(this) < fe(b)) b
        else if (fe(this) == fe(b)) addPair((fe(this), fc(this) + fc(b)), rest(b))
        else addPair((fe(this), fc(b)), rest(this) + b)

    }

    def -(b: CNF): CNF = this match {
      case Atom(n) => b match {
        case Atom(m) => if (n <= m) Atom(0) else Atom(n - m)
        case _ => throw new Exception("Atom - NonAtom: " + this + "-" + b)
      }
      case a if fe(a) < fe(b) => zero
      case a if fe(a) > fe(b) => a
      case a if fc(a) < fc(b) => zero
      case a if fc(a) > fc(b) => addPair((fe(a), fc(a) - fc(b)), rest(a))
      case _ => rest(this) - rest(b)
    }

    def *(b: CNF): CNF = b match {
      case `zero` => zero
      case _ if this == zero => zero
      case Atom(m) => this match {
        case Atom(n) => Atom(n*m)
        case a => addPair((fe(a), fc(a)*m), rest(a))
      }
      case a => addPair((fe(a) + fe(b), fc(b)), a * rest(b))
    }

    def ^(b: CNF) = this match {
      case `one` => one
      case _ if b == zero => one
      case t@Atom(n) => b match {
        case Atom(m) => Atom(n ^ m)
        case _ => exp1(t, b)
      }
      case _ if b.isInstanceOf[Atom] => exp3(this, b.asInstanceOf[Atom])
      case _ => exp4(this, b)
    }
  }

  case class Atom(n: Integer) extends CNF {
    override def toString: String = n.toString
  }

  val zero = new Atom(0)
  val one = new Atom(1)

  case class CList(list: List[(CNF, Int)], atom: Atom) extends CNF {
    if (list.size == 0) throw new Exception("nil in CList")

    override def toString: String =
      list.map(p => "(w " + (if (p._1 == one) ""
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

  def natpart(a: CNF): Atom = a match {
    case t: Atom => t
    case _ => natpart(rest(a))
  }

  def addPair(pair: (CNF, Int), b: CNF): CNF = b match {
    case t: Atom => CList(List(pair), t)
    case CList(list, t) => CList(pair :: list, t)
  }

  def exp1(t: Atom, b: CNF): CNF = b match {
    case a if rest(b).isInstanceOf[Atom] => CList(
      List(
        CList(
          List((fe(b) - one, fc(b))),
          zero),
        math.pow(t.n, rest(b).asInstanceOf[Atom].n)),
      zero)
  }

  def exp2(a: CNF, q: Atom): CNF = q match {
    case `one` => a
    case Atom(n) => CList(List((fe(a) * Atom(n-1), 1)), zero) * a
  }

  def exp3(a: CNF, q: Atom): CNF = q match {
    case `zero` => one
    case `one` => a
    case x if limitp(a) => exp2(a, q)
    case Atom(n) => exp3(a, Atom(n - 1)) * a
  }

  def exp4(a: CNF, b: CNF): CNF = CList(List((fe(a) * limitpart(b), 1)), zero) * exp3(a, natpart(b))

  def ordToCantor(o: Ordinal): CNF = o match {
    case W() => CList(List((one, 1)), zero)
    case Num(a) => Atom(a)
    case add(a, b) => ordToCantor(a) + ordToCantor(b)
    case mul(a, b) => ordToCantor(a) * ordToCantor(b)
    case pow(a, b) => ordToCantor(a) ^ ordToCantor(b)
  }

  def parseOrd(s: String) = ordToCantor(parser.parseAll(parser.expr, s).get)
  def intToAtom(i: Int) = parseOrd(i.toString).asInstanceOf[Atom]
}
