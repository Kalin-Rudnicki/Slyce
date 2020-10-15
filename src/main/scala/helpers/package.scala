import scalaz.-\/
import scalaz.\/
import scalaz.Scalaz.ToEitherOps
import scalaz.\/-

package object helpers {

  implicit class TraverseOps[E, R](obj: List[List[E] \/ R]) {

    def traverseErrs: List[E] \/ List[R] =
      obj.foldLeft(Nil.right: List[E] \/ List[R]) {
        case (\/-(res), \/-(todo)) =>
          (todo :: res).right
        case (\/-(_), -\/(todo)) =>
          todo.left
        case (res @ -\/(_), \/-(_)) =>
          res
        case (-\/(res), -\/(todo)) =>
          (res ::: todo).left
      }

  }

}
