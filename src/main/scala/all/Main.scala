package all
import scala.annotation.tailrec


object Main {

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

  def min (a: Int, b: Int): Int = a - b match{
    case x if x > 0 => b
    case _ => a
  }

  def find_m(in: List[Int]): Int = in match {
    case a :: b :: rest => min(min(a, b), find_m(rest))
    case a :: rest  => rest match {
      case c :: _ => min(a, c)
      case Nil => a
    }

  }

def main(args: Array[String]) {
  println(find_min(List(13, 20, -50, -3, -5, 5, 50)))
  println(find_m(List(13, 20, -50, -3, -5, 5, 50)))
}
}
