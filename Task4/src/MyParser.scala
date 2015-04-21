import scala.collection.mutable.ArrayBuffer

class MyParser {

  private var expr: String = null

  def this(expr: String) {
    this()
    this.expr = expr.replaceAll("->", ">")
  }

  private def impl(l: Int, r: Int): Expression = {
    if ((r == l)&(l == 0)) return null
    var balance = 0
    for (i <- l until r) {
      expr(i) match {
        case '(' => balance += 1
        case ')' => balance -= 1
        case '>' if balance == 0 => return new Implication(disj(l, i), impl(i+1, r))
        case _ => ;
      }
    }
    disj(l, r)
  }

  private def disj(l: Int, r: Int): Expression = {
    var balance = 0
    var index = -1
    for (i <- l until r) {
      expr(i) match {
        case '(' => balance += 1
        case ')' => balance -= 1
        case '|' if balance == 0 => index = i
        case _ => ;
      }
    }
    if (index != -1) {
      return new Disjunction(disj(l, index), conj(index + 1, r))
    }
    conj(l, r)
  }

  private def conj(l: Int, r: Int): Expression = {
    var balance = 0
    var index = -1
    for (i <- l until r) {
      expr(i) match {
        case '(' => balance += 1
        case ')' => balance -= 1
        case '&' if balance == 0 => index = i
        case _ => ;
      }
    }
    if (index != -1) {
      return new Conjunction(conj(l, index), unary(index + 1, r))
    }
    unary(l, r)
  }

  private def unary(l: Int, r: Int): Expression = {
    println(expr);
    if (expr(l) == '!') {
      return new Negation(unary(l + 1, r))
    }
    if (expr(l) == '(' && expr(r - 1) == ')') {
      var fl = true
      var balance = 1

      for (i <- l+1 until r-1) {
        expr(i) match {
          case '(' => balance += 1
          case ')' => balance -= 1
          case _ if balance == 0 => fl = false
          case _ => ;
        }
        if (fl) {
          return impl(l+1, r-1)
        }
      }
    }
    if (expr(l) == '@') {
      val v = vari(l + 1, r).asInstanceOf[Variable]
      return new ForAll(v, unary(l + 1 + v.toString().length, r))
    }
    if (expr(l) == '?') {
      val v = vari(l + 1, r).asInstanceOf[Variable]
      return new Exists(v, unary(l + 1 + v.toString().length, r))
    }
    pred(l, r)
  }

  private def pred(l: Int, r: Int): Expression = {
    if (Character.isAlphabetic(expr(l)) && Character.isUpperCase(expr(l))) {
      var name = "" + expr(l)
      var index = l + 1
      while (index < r && (Character.isDigit(expr(index)) ||
        (Character.isAlphabetic(expr(index)) &&
          Character.isUpperCase(expr(index))))) {
        name += expr(index)
        index += 1
      }

      val terms = new ArrayBuffer[Expression]
<<<<<<< HEAD
      if (index < r && expr(index) == '(' && expr(r - 1) == ')') {
=======
      index += 1
      var ll = index
      while (index <= r - 1) {
        if (expr(index) == ',' || index == r - 1) {
          terms += term(ll, index)
          ll = index + 1
        }
>>>>>>> f5ac5e8df08c865a777a4b1bc2a07a81eacd8225
        index += 1
        var ll = index
        var balance = 0;
        while (index <= r - 1) {
         /* if (expr(index) == ',' || index == r - 1) {
            val internTerm = term(ll, index)
            terms += internTerm
            ll = index + 1
          }
          index += 1 */
          val c = expr(index)
          c match {
            case _ if index == r - 1 =>  {
              val internTerm = term(ll, index)
              terms += internTerm
              ll = index + 1
            }
            case '(' => balance += 1
            case ')' => balance -= 1
            case ',' if balance == 0 => {
              val internTerm = term(ll, index)
              terms += internTerm
              ll = index + 1
            }

            case _ => ;
          }
          index += 1;
        }
      }
      return new Predicate(name, terms)
    }
    else {
      for (i <- l until r) {
        if (expr(i) == '=') return new Equals(term(l, i), term(i+1, r))
      }
    }
    null
  }

  private def term(l: Int, r: Int): Expression = {
    var index = -1
    var balance = 0

    for (i <- l until r) {
      expr(i) match {
        case '(' => balance += 1
        case ')' => balance -= 1
        case '+' if balance == 0 => index = i
        case _ => ;
      }
    }

    if (index != -1) {
      return new Plus(term(l, index), plus(index + 1, r))
    }
    plus(l, r)
  }

  private def plus(l: Int, r: Int): Expression = {
    var index = -1
    var balance = 0

    for (i <- l until r) {
      expr(i) match {
        case '(' => balance += 1
        case ')' => balance -= 1
        case '*' if balance == 0 => index = i
        case _ => ;
      }
    }

    if (index != -1) {
      return new Times(plus(l, index), times(index + 1, r))
    }
    times(l, r)
  }

  private def times(l: Int, r: Int): Expression = {
    if (expr(r - 1) == '\'') {
      return new Apostrophe(times(l, r - 1))
    }
    if (expr(l) == '0') {
      return new Zero
    }
    if (expr(l) == '(' && expr(r - 1) == ')') {
      return term(l + 1, r - 1)
    }
    val name = vari(l, r).asInstanceOf[Variable].name
    //println("var: " + name+ " " + name.length + " "+ l + " " + r)
<<<<<<< HEAD
=======
    if (l + name.length == r) {
      return new Variable(name)
    }
>>>>>>> f5ac5e8df08c865a777a4b1bc2a07a81eacd8225
    val terms = new ArrayBuffer[Expression]
    var index = l + name.length

<<<<<<< HEAD
    if (index < r && expr(index) == '(' && expr(r - 1) == ')') {
     // println("terms: " + expr(index))

=======
    println("terms: " + expr(index))

    while (index <= r) {
      if (expr(index) == ',' || index == r) {
        terms += term(ll, index)
        ll = index + 1
      }
>>>>>>> f5ac5e8df08c865a777a4b1bc2a07a81eacd8225
      index += 1
      var ll = index

      for (i <- index until r) {
        if (expr(i) == ',' || i == r - 1) {
          terms += term(ll, i)
          ll = i + 1
        }
      }
    }
    println("Pred: " + name + "(" + terms+ ")")

    if (terms.length == 0) {
      new Variable(name)
    } else {
      new Function(name, terms)
    }
<<<<<<< HEAD
=======
    println("Pred: " + name + "(" + terms+ ")")
    new Predicate(name, terms)
>>>>>>> f5ac5e8df08c865a777a4b1bc2a07a81eacd8225
  }

  private def vari(l: Int, r: Int): Expression = {
    var name = ""
    var index = l
    while (index < r && (Character.isDigit(expr(index)) ||
      (Character.isAlphabetic(expr(index)) &&
        Character.isLowerCase(expr(index))))) {
      name += expr.charAt(index)
      index += 1
    }
    new Variable(name)
  }

  def parse: Expression = impl(0, expr.length)

}