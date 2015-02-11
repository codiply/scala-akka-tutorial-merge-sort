package actormergesort

import akka.actor._

object Sorter {
  case class Sort(array: Array[Int])
  case class SortResult(sorted: List[Int])
  
  def props() = Props(classOf[Sorter])
}

class Sorter() extends Actor {
  import Sorter._
  import Merger.Merge
  import Merger.MergeResult
  
  var sorted1: Option[List[Int]] = None
  var parent: ActorRef = context.system.deadLetters
  
  def receive = idle
  
  val idle: Receive = {
    case Sort(array) =>
      if (array.length < 2) {
        sender ! SortResult(array.toList)
      } else {
        parent = sender
      
        val subSorter1 = context.actorOf(props(), self.path.name + "-1")
        val subSorter2 = context.actorOf(props(), self.path.name + "-2")
        
        val subArray1 = array.slice(0, array.length / 2)
        val subArray2 = array.slice(array.length / 2, array.length)
        
        subSorter1 ! Sort(subArray1)
        subSorter2 ! Sort(subArray2)
        
        context.become(waitingSubSorts)
      }
  }
  
  val waitingSubSorts: Receive = {
    case SortResult(sorted) =>
      sorted1 match {
        case None =>
          sorted1 = Some(sorted)
        case Some(s1) =>
          val merger = context.actorOf(Merger.props(), "merger-for-" + self.path.name)
          merger ! Merge(s1, sorted)
          context.become(waitingMerge)
      }
  }
  
  val waitingMerge: Receive = {
    case MergeResult(merged) =>
      parent ! SortResult(merged)
  }
}