package actormergesort

import akka.actor._
import akka.pattern.ask
import akka.util.Timeout
import scala.concurrent.duration._
import scala.util.Random

object Runner2 extends App {
  import Sorter._
  
  val rnd = new Random()
    
  val array = (1 to 30).map {_ => rnd.nextInt(100) }.toArray
  println("Unsorted: " + array.mkString("[",",","]"))

  val system = ActorSystem()
  val sorter = system.actorOf(Sorter.props(), "sorter")
      
  implicit val timeout = Timeout(25.seconds)     
  // Use system's dispatcher ExecutionContext for the future.
  import system.dispatcher 
      
  val future = sorter ? Sort(array)
      
  future.map { 
    case SortResult(sorted) =>
      println("Sorted: " + sorted.mkString("[",",","]"))
      system.shutdown()
  }
}