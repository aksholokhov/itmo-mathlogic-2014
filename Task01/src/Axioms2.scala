/**
 * Created by Шолохов on 17.11.2014.
 */
object Axioms2 {
  def check1(e: Expression): Boolean = {
  e match {
    case Implication(e1, e2) => {
      e2 match {
        case Implication(e21, e22) => {
          if (e1.equals(e22)) return true
        }
        case _ =>
      }
    }
    case _ =>
  }
    return false
  }
}
