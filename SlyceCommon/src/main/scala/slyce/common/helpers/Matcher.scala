package slyce.common.helpers

trait Matcher[I, O] {

  def unapply(i: I): Option[O]

}
