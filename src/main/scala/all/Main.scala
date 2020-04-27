package all
import scala.annotation.tailrec


object Main {


  def find[A](in: List[A], value: A, index: Int = 0): Int = in match {
    case first :: _ if first == value => index
    case _ :: rest => find(rest, value, index + 1)
    case Nil => -1
  }

  @tailrec
  def find_min(in: List[Int], min: Int = 0, index: Int = -1): Int = index match {
    case -1 => in match {
      case Nil => throw new IllegalArgumentException()
      case first :: _ => find_min(in, first, 0)
    }
    case _ => in match {
      case Nil => min
      case first :: rest if first < min => find_min(rest, first, index + 1)
      case _ :: rest => find_min(rest, min, index + 1)
    }

  }

  def find_m(in: List[Int], min: Int = 0): Int = min match{
    case first :: rest if first < min => find_m(rest, first)
    case _ :: rest => find_m(rest, min)
  }

def main(args: Array[String]) = println(find_min(List(13, 20, -50, -3, -5, 5, 50)))
}
