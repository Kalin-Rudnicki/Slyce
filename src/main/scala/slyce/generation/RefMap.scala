package slyce.generation

import scala.annotation.tailrec
import scala.collection.mutable.{Map => MMap}

class RefMap[K, V] {
  
  private val map: MMap[K, RefMap.Ref[V]] = MMap()
  
  def obtainRef(k: K): RefMap.Ref[V] =
    map.getOrElseUpdate(k, RefMap.Ref())
  
  def set(k: K, v: V): Unit =
    map.getOrElseUpdate(k, RefMap.Ref()).ref = Some(v)
  
  def get(k: K): RefMap.Result[V] =
    map.get(k).fold(RefMap.Result.Missing)(_.get)
  
  
  def loop(f: (K, RefMap.Ref[V], RefMap[K, V]) => Unit): RefMap[K, V] = {
    @tailrec
    def loop2: Unit =
      map.find(t => t._2.ref.isEmpty) match {
        case Some(value) =>
          f(value._1, value._2, this)
          loop2
        case None =>
      }
      
    loop2
    this
  }
  
}

object RefMap {
  
  class Ref[T] {
    
    private[RefMap] var ref: Option[T] = None
    
    def get: Result[T] =
      ref.fold(Result.Uninitialized)(Result.Present(_))
    
  }
  
  sealed trait Result[+T] {
    
    def toOption: Option[T] =
      this match {
        case Result.Missing => 
          None
        case Result.Uninitialized =>
          None
        case Result.Present(value) =>
          Some(value)
      }
    
  }
  
  object Result {
    
    case object Missing extends Result[Nothing]
    
    case object Uninitialized extends Result[Nothing]
    
    case class Present[T](value: T) extends Result[T]
    
  }
  
}
