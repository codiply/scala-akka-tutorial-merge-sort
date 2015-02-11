package actormergesort

import akka.actor._

object Merger {
  case class Merge(list1: List[Int], list2: List[Int])
  case class MergeResult(merged: List[Int])
  
  def props() = Props(classOf[Merger])
}

class Merger() extends Actor {
  import Merger._
  import scala.annotation.tailrec
  
  def receive = {
      case Merge(list1, list2) =>
      val merged = merge(list1, list2)
      sender ! MergeResult(merged)
  }
  
  private def merge(list1: List[Int], list2: List[Int]) = mergeRec(list1, list2, Nil)
  
  @tailrec
  private def mergeRec(list1: List[Int], list2: List[Int], revAcc: List[Int]): List[Int] = {
    (list1, list2) match {
      case (hd1::tl1, hd2::tl2) =>
        if (hd1 <= hd2) mergeRec(tl1, list2, hd1::revAcc) else mergeRec(list1, tl2, hd2::revAcc)
      case (hd1::tl1, Nil) => mergeRec(tl1, list2, hd1::revAcc)
      case (Nil, hd2::tl2) => mergeRec(list1, tl2, hd2::revAcc)
      case _ => revAcc.reverse
    }
  }
}
