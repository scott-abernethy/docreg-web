package vvv.docreg.util

import net.liftweb.json._

/**
 * Fixes lame compile warning with Lift's JValue class, where withFilter method does not exist yet, using filter method instead.
 */
object JValueWithFilter {

  implicit class LiftJValueWithFilter(self: JValue) extends JValueWithFilter(self, _ => true)

}
 
/**
 * Fixes lame compile warning with Lift's JValue class, where withFilter method does not exist yet, using filter method instead.
 */
class JValueWithFilter(self: JValue, p: JValue => Boolean) {

  def map[T](f: JValue => T): List[T] = 
    self.filter(p).map(f)

  def flatMap[T](f: JValue => List[T]): List[T] =
    self.filter(p).flatMap(f)

  def foreach(f: JValue => Unit): Unit =
    self.filter(p).foreach(f)

  def withFilter(q: JValue => Boolean): JValueWithFilter =
    new JValueWithFilter(self, x => p(x) && q(x))

}
