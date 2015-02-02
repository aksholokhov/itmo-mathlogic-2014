/**
 * Created by Шолохов on 16.11.2014.
 */
abstract class Expression {
  def evaluate (values : Map[String, Boolean]) : Boolean
}
