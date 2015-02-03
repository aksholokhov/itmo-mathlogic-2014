/**
 * Created by Шолохов on 16.11.2014.
 */
object Axioms {

  def change(e: Expression, a: Expression, x: Expression): Expression = {
    e match {
      case v: Variable => if (v.equals(x)) a else v
      case p: Predicate => new Predicate(p.name, p.terms.map(change(_, a, x)))
      case c: Conjunction => new Conjunction(change(c.left, a, x), change(c.right, a, x))
      case c: Disjunction => new Disjunction(change(c.left, a, x), change(c.right, a, x))
      case c: Implication => new Implication(change(c.left, a, x), change(c.right, a, x))
      case n: Negation => new Negation(change(n.expression, a, x))
      case s: Exists => new Exists(change(s.variable, a, x).asInstanceOf[Variable], change(s.expression, a, x))
      case s: ForAll => new ForAll(change(s.variable, a, x).asInstanceOf[Variable], change(s.expression, a, x))
      case t: Equals => new Equals(change(t.left, a, x), change(t.right, a, x))
      case t: Times => new Times(change(t.left, a, x), change(t.right, a, x))
      case a: Apostrophe => new Apostrophe(change(a.expression, a, x))
      case z: Zero => z
      case _ => null
    }
  }

  def newChecker(e: Expression): Int = {
    e match {
      case Implication(left, Implication(_, right)) if left == right => 0
      case Implication(impl2: Implication, Implication(Implication(impl4left, impl5: Implication), impl6: Implication))
        if (impl2.left == impl4left) &&
          (impl2.left == impl6.left) &&
          (impl2.right == impl5.left) &&
          (impl5.right == impl6.right) => 1
      case Implication(a, Implication(b, Conjunction(c, d))) if (a == c) && (b == d) => 2
      case Implication(a: Conjunction, b) if b == a.left => 3
      case Implication(Conjunction(_, a), b) if a == b => 4
      case Implication(a, Disjunction(b, _)) if a == b => 5
      case Implication(a, Disjunction(_, b)) if a == b => 6
      case Implication(Implication(i2l, i2r), Implication(Implication(i4l, i4r), Implication(Disjunction(d1l, d1r), i5r)))
        if (i2l == d1l) && (i2r == i4r) && (i2r == i5r) && (i4l == d1r) => 7
      case Implication(Implication(i2l, i2r), Implication(Implication(i4l, Negation(n1)), Negation(n2)))
        if (n2 == i2l) && (n2 == i4l) && (i2r == n1) => 8
      case Implication(Negation(Negation(n)), i1r) if n == i1r => 9
      case Implication(ForAll(variable, expression), i1r)
        if {
          var expr = Main.getExchange(expression, i1r)
          if (expr == null) expr = variable
          change(expression, expr, variable) == i1r
        } => 10
      case Implication(i1l, ex: Exists)
        if {
          var expr = Main.getExchange(ex.expression, i1l)
          if (expr == null) {
            expr = ex.variable
          }
          change(ex.expression, expr, ex.variable) == i1l
        } => 11
      case Implication(Equals(v1: Variable, v2: Variable), Equals(Apostrophe(a1), Apostrophe(a2)))
        if (v1 == a1) && (v2 == a2) => 101
      case Implication(Equals(v1: Variable, v2: Variable), Implication(Equals(e2l, v3: Variable), Equals(e3l, e3r)))
        if (v1 == e2l) && (v2 == e3l) && (v3 == e3r) => 102
      case Implication(Equals(Apostrophe(a1), Apostrophe(a2)), Equals(v1: Variable, v2: Variable))
        if (v1 == a1) && (v2 == a2) => 103
      case Negation(Equals(Apostrophe(v: Variable)), z: Zero) => 104
      case Equals(Plus(v1: Variable, Apostrophe(v2: Variable)), Apostrophe(p2: Plus))
        if (v2 == p2.right) && (v1 == p2.left) => 105
      case Equals(Plus(pl, z: Zero), v: Variable) if v == pl => 106
      case Equals(Times(v: Variable, z2: Zero), z: Zero) => 107
      case Equals(Times(v: Variable, ap: Apostrophe), Plus(Times(t2l, t2r), p1r))
        if (v == p1r) && (v == t2l) && (ap == t2r) && ap.expression.isInstanceOf[Variable] => 108
      case Implication(Conjunction(conl, ForAll(v, Implication(i2l, i2r))), i1r)
        if (conl == change(i2r, new Zero, v)) && (i1r == i2l) && (i2r == change(i1r, new Apostrophe(v), v)) => 13
      case _ => -1
    }
  }

  //TODO: remove this trash after newChecker testing

  def checker(e: Expression): Int = {
    if (axiom0(e)) return 0
    if (axiom1(e)) return 1
    if (axiom2(e)) return 2
    if (axiom3(e)) return 3
    if (axiom4(e)) return 4
    if (axiom5(e)) return 5
    if (axiom6(e)) return 6
    if (axiom7(e)) return 7
    if (axiom8(e)) return 8
    if (axiom9(e)) return 9
    if (axiom10(e)) return 10
    if (axiom11(e)) return 11
    if (checkA1(e)) return 101
    if (checkA2(e)) return 102
    if (checkA3(e)) return 103
    if (checkA4(e)) return 104
    if (checkA5(e)) return 105
    if (checkA6(e)) return 106
    if (checkA7(e)) return 107
    if (checkA8(e)) return 108
    if (checkA9(e)) return 13
    -1
  }

  private def axiom0(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.right.isInstanceOf[Implication]) {
        val impl2: Implication = impl1.right.asInstanceOf[Implication]
        return impl1.left.equals(impl2.right)
      }
    }
    false
  }

  private def axiom1(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Implication]) {
        val impl2: Implication = impl1.left.asInstanceOf[Implication]
        if (impl1.right.isInstanceOf[Implication]) {
          val impl3: Implication = impl1.right.asInstanceOf[Implication]
          if (impl3.left.isInstanceOf[Implication]) {
            val impl4: Implication = impl3.left.asInstanceOf[Implication]
            if (impl4.right.isInstanceOf[Implication]) {
              val impl5: Implication = impl4.right.asInstanceOf[Implication]
              if (impl3.right.isInstanceOf[Implication]) {
                val impl6: Implication = impl3.right.asInstanceOf[Implication]
                val fl1: Boolean = (impl2.left == impl4.left) && (impl2.left == impl6.left)
                val fl2: Boolean = impl2.right == impl5.left
                val fl3: Boolean = impl5.right == impl6.right
                return (fl1 && fl2 && fl3)
              }
            }
          }
        }
      }
    }
    false
  }

  private def axiom2(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.right.isInstanceOf[Implication]) {
        val impl2: Implication = impl1.right.asInstanceOf[Implication]
        if (impl2.right.isInstanceOf[Conjunction]) {
          val conj1: Conjunction = impl2.right.asInstanceOf[Conjunction]
          val fl1: Boolean = impl1.left == conj1.left
          val fl2: Boolean = impl2.left == conj1.right
          return (fl1 && fl2)
        }
      }
    }
    false
  }

  private def axiom3(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Conjunction]) {
        val conj1: Conjunction = impl1.left.asInstanceOf[Conjunction]
        return ((impl1.right == conj1.left))
      }
    }
    false
  }

  private def axiom4(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Conjunction]) {
        val conj1: Conjunction = impl1.left.asInstanceOf[Conjunction]
        return ((impl1.right == conj1.right))
      }
    }
    false
  }

  private def axiom5(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.right.isInstanceOf[Disjunction]) {
        val disj1: Disjunction = impl1.right.asInstanceOf[Disjunction]
        return ((impl1.left == disj1.left))
      }
    }
    false
  }

  private def axiom6(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.right.isInstanceOf[Disjunction]) {
        val disj1: Disjunction = impl1.right.asInstanceOf[Disjunction]
        return ((impl1.left == disj1.right))
      }
    }
    false
  }

  private def axiom7(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Implication]) {
        val impl2: Implication = impl1.left.asInstanceOf[Implication]
        if (impl1.right.isInstanceOf[Implication]) {
          val impl3: Implication = impl1.right.asInstanceOf[Implication]
          if (impl3.left.isInstanceOf[Implication]) {
            val impl4: Implication = impl3.left.asInstanceOf[Implication]
            if (impl3.right.isInstanceOf[Implication]) {
              val impl5: Implication = impl3.right.asInstanceOf[Implication]
              if (impl5.left.isInstanceOf[Disjunction]) {
                val disj1: Disjunction = impl5.left.asInstanceOf[Disjunction]
                val fl1: Boolean = impl2.left == disj1.left
                val fl2: Boolean = (impl2.right == impl4.right) && (impl2.right == impl5.right)
                val fl3: Boolean = impl4.left == disj1.right
                return (fl1 && fl2 && fl3)
              }
            }
          }
        }
      }
    }
    false
  }

  private def axiom8(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Implication]) {
        val impl2: Implication = impl1.left.asInstanceOf[Implication]
        if (impl1.right.isInstanceOf[Implication]) {
          val impl3: Implication = impl1.right.asInstanceOf[Implication]
          if (impl3.left.isInstanceOf[Implication]) {
            val impl4: Implication = impl3.left.asInstanceOf[Implication]
            if (impl4.right.isInstanceOf[Negation]) {
              val neg1: Negation = impl4.right.asInstanceOf[Negation]
              if (impl3.right.isInstanceOf[Negation]) {
                val neg2: Negation = impl3.right.asInstanceOf[Negation]
                var fl1: Boolean = neg2.expression == impl2.left
                fl1 = fl1 && (neg2.expression == impl4.left)
                val fl2: Boolean = impl2.right == neg1.expression
                return (fl1 && fl2)
              }
            }
          }
        }
      }
    }
    false
  }

  private def axiom9(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Negation]) {
        val neg1: Negation = impl1.left.asInstanceOf[Negation]
        if (neg1.expression.isInstanceOf[Negation]) {
          val neg2: Negation = neg1.expression.asInstanceOf[Negation]
          return impl1.right == neg2.expression
        }
      }
    }
    false
  }


  private def checkA9(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Conjunction]) {
        val conj: Conjunction = impl1.left.asInstanceOf[Conjunction]
        if (conj.right.isInstanceOf[ForAll]) {
          val forAll: ForAll = conj.right.asInstanceOf[ForAll]
          val x: Variable = forAll.variable
          if (forAll.expression.isInstanceOf[Implication]) {
            val impl2: Implication = forAll.expression.asInstanceOf[Implication]
            val ee: Expression = change(conj.left, new Zero, x)
            val fl1: Boolean = conj.left == change(impl1.right, new Zero, x)
            val fl2: Boolean = impl1.right == impl2.left
            val fl3: Boolean = impl2.right == change(impl1.right, new Apostrophe(x), x)
            return fl1 && fl2 && fl3
          }
        }
      }
    }
    return false
  }

  private def checkA8(e: Expression): Boolean = {
    if (e.isInstanceOf[Equals]) {
      val eq: Equals = e.asInstanceOf[Equals]
      if (eq.left.isInstanceOf[Times] && eq.right.isInstanceOf[Plus]) {
        val t1: Times = eq.left.asInstanceOf[Times]
        val p: Plus = eq.right.asInstanceOf[Plus]
        if (t1.left.isInstanceOf[Variable] && t1.right.isInstanceOf[Apostrophe]) {
          val ap: Apostrophe = t1.right.asInstanceOf[Apostrophe]
          if (ap.expression.isInstanceOf[Variable] && p.left.isInstanceOf[Times]) {
            val t2: Times = p.left.asInstanceOf[Times]
            return (t1.left == p.right) && (t1.left == t2.left) && (t1.right == t2.right)
          }
        }
      }
    }
    return false
  }


  private def checkA7(e: Expression): Boolean = {
    if (e.isInstanceOf[Equals]) {
      val eq: Equals = e.asInstanceOf[Equals]
      if (eq.right.isInstanceOf[Zero] && eq.left.isInstanceOf[Times]) {
        val p: Times = eq.left.asInstanceOf[Times]
        return p.right.isInstanceOf[Zero] && p.left.isInstanceOf[Variable]
      }
    }
    return false
  }

  private def checkA6(e: Expression): Boolean = {
    if (e.isInstanceOf[Equals]) {
      val eq: Equals = e.asInstanceOf[Equals]
      if (eq.right.isInstanceOf[Variable] && eq.left.isInstanceOf[Plus]) {
        val p: Plus = eq.left.asInstanceOf[Plus]
        return p.right.isInstanceOf[Zero] && (p.left == eq.right)
      }
    }
    return false
  }

  private def checkA5(e: Expression): Boolean = {
    if (e.isInstanceOf[Equals]) {
      val eq: Equals = e.asInstanceOf[Equals]
      if (eq.left.isInstanceOf[Plus] && eq.right.isInstanceOf[Apostrophe]) {
        val p1: Plus = eq.left.asInstanceOf[Plus]
        val ap1: Apostrophe = eq.right.asInstanceOf[Apostrophe]
        if (ap1.expression.isInstanceOf[Plus]) {
          val p2: Plus = ap1.expression.asInstanceOf[Plus]
          if (p1.left.isInstanceOf[Variable] && p1.right.isInstanceOf[Apostrophe]) {
            val ap2: Apostrophe = p1.right.asInstanceOf[Apostrophe]
            if (ap2.expression.isInstanceOf[Variable]) {
              return (ap2.expression == p2.right) && (p1.left == p2.left)
            }
          }
        }
      }
    }
    return false
  }

  private def checkA4(e: Expression): Boolean = {
    if (e.isInstanceOf[Negation]) {
      val neg: Negation = e.asInstanceOf[Negation]
      if (neg.expression.isInstanceOf[Equals]) {
        val eq: Equals = neg.expression.asInstanceOf[Equals]
        if (eq.left.isInstanceOf[Apostrophe]) {
          val ap: Apostrophe = eq.left.asInstanceOf[Apostrophe]
          return (eq.right.isInstanceOf[Zero] && ap.expression.isInstanceOf[Variable])
        }
      }
    }
    return false
  }


  private def checkA3(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl: Implication = e.asInstanceOf[Implication]
      if (impl.left.isInstanceOf[Equals]) {
        val eq1: Equals = impl.left.asInstanceOf[Equals]
        if (impl.right.isInstanceOf[Equals]) {
          val eq2: Equals = impl.right.asInstanceOf[Equals]
          if (eq1.left.isInstanceOf[Apostrophe]) {
            val a1: Apostrophe = eq1.left.asInstanceOf[Apostrophe]
            if (eq1.right.isInstanceOf[Apostrophe]) {
              val a2: Apostrophe = eq1.right.asInstanceOf[Apostrophe]
              if (eq2.right.isInstanceOf[Variable] && eq2.left.isInstanceOf[Variable]) {
                return (eq2.right == a2.expression) && (eq2.left == a1.expression)
              }
            }
          }
        }
      }
    }
    return false
  }

  private def checkA2(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Equals]) {
        val eq1: Equals = impl1.left.asInstanceOf[Equals]
        if (impl1.right.isInstanceOf[Implication]) {
          val impl2: Implication = impl1.right.asInstanceOf[Implication]
          if (impl2.left.isInstanceOf[Equals]) {
            val eq2: Equals = impl2.left.asInstanceOf[Equals]
            if (impl2.right.isInstanceOf[Equals]) {
              val eq3: Equals = impl2.right.asInstanceOf[Equals]
              if (eq1.left.isInstanceOf[Variable] && eq1.right.isInstanceOf[Variable] && eq2.right.isInstanceOf[Variable]) {
                val fl1: Boolean = eq1.left == eq2.left
                val fl2: Boolean = eq1.right == eq3.left
                val fl3: Boolean = eq2.right == eq3.right
                return fl1 && fl2 && fl3
              }
            }
          }
        }
      }
    }
    return false
  }

  private def checkA1(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl: Implication = e.asInstanceOf[Implication]
      if (impl.left.isInstanceOf[Equals]) {
        val eq1: Equals = impl.left.asInstanceOf[Equals]
        if (impl.right.isInstanceOf[Equals]) {
          val eq2: Equals = impl.right.asInstanceOf[Equals]
          if (eq2.left.isInstanceOf[Apostrophe]) {
            val a1: Apostrophe = eq2.left.asInstanceOf[Apostrophe]
            if (eq2.right.isInstanceOf[Apostrophe]) {
              val a2: Apostrophe = eq2.right.asInstanceOf[Apostrophe]
              if (eq1.right.isInstanceOf[Variable] && eq1.left.isInstanceOf[Variable]) {
                return (eq1.right == a2.expression) && (eq1.left == a1.expression)
              }
            }
          }
        }
      }
    }
    return false
  }


  private def axiom11(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl: Implication = e.asInstanceOf[Implication]
      if (impl.right.isInstanceOf[Exists]) {
        val exists: Exists = impl.right.asInstanceOf[Exists]
        var expr: Expression = Main.getExchange(exists.expression, impl.left)
        if (expr == null) {
          expr = exists.variable
        }
        if (change(exists.expression, expr, exists.variable) == impl.left) {
          return true
        }
      }
    }
    false
  }

  private def axiom10(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl: Implication = e.asInstanceOf[Implication]
      if (impl.left.isInstanceOf[ForAll]) {
        val forall: ForAll = impl.left.asInstanceOf[ForAll]
        var expr: Expression = Main.getExchange(forall.expression, impl.right)
        if (expr == null) {
          expr = forall.variable
        }
        if (change(forall.expression, expr, forall.variable) == impl.right) {
          return true
        }
      }
    }
    false
  }
}