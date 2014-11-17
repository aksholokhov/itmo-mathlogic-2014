/**
 * Created by Шолохов on 16.11.2014.
 */
object Axioms {
  private def axiom0(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.right.isInstanceOf[Implication]) {
        val impl2: Implication = impl1.right.asInstanceOf[Implication]
        return impl1.left.equals(impl2.right)
      }
    }
    return false
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
    return false
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
    return false
  }

  private def axiom3(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Conjunction]) {
        val conj1: Conjunction = impl1.left.asInstanceOf[Conjunction]
        return ((impl1.right == conj1.left))
      }
    }
    return false
  }

  private def axiom4(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Conjunction]) {
        val conj1: Conjunction = impl1.left.asInstanceOf[Conjunction]
        return ((impl1.right == conj1.right))
      }
    }
    return false
  }

  private def axiom5(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.right.isInstanceOf[Disjunction]) {
        val disj1: Disjunction = impl1.right.asInstanceOf[Disjunction]
        return ((impl1.left == disj1.left))
      }
    }
    return false
  }

  private def axiom6(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.right.isInstanceOf[Disjunction]) {
        val disj1: Disjunction = impl1.right.asInstanceOf[Disjunction]
        return ((impl1.left == disj1.right))
      }
    }
    return false
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
    return false
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
                var fl1: Boolean = neg2.e == impl2.left
                fl1 = fl1 && (neg2.e == impl4.left)
                val fl2: Boolean = impl2.right == neg1.e
                return (fl1 && fl2)
              }
            }
          }
        }
      }
    }
    return false
  }

  private def axiom9(e: Expression): Boolean = {
    if (e.isInstanceOf[Implication]) {
      val impl1: Implication = e.asInstanceOf[Implication]
      if (impl1.left.isInstanceOf[Negation]) {
        val neg1: Negation = impl1.left.asInstanceOf[Negation]
        if (neg1.e.isInstanceOf[Negation]) {
          val neg2: Negation = neg1.e.asInstanceOf[Negation]
          return impl1.right == neg2.e
        }
      }
    }
    return false
  }

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
    return -1
  }
}
