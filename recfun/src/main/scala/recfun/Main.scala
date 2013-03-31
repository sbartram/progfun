package recfun
import common._
import scala.collection.mutable.Stack

object Main {

    //    def main(args: Array[String]) {
    //        println("Pascal's Triangle")
    //        for (row <- 0 to 10) {
    //            for (col <- 0 to row)
    //                print(pascal(col, row) + " ")
    //            println()
    //        }
    //    }

    /**
     * Exercise 1
     */
    def pascal(c: Int, r: Int): Int = { 0 }

    def main(args: Array[String]) {
        println("result(T): " + balance("".toList))
        println("result(F): " + balance(")".toList))
        println("result(T): " + balance("()".toList))
        println("result(F): " + balance("())".toList))
        println("result(F): " + balance("(()(".toList))
        println("result(T): " + balance("()()".toList))
        println("result(T): " + balance("(()())".toList))
        println("result(F): " + balance("())(".toList))
    }

    /**
     * Exercise 2
     */
    def balance(chars: List[Char]): Boolean = {

        def m(cs: List[Char], stack: Int): Boolean = {
            if (stack < 0)
                false
            else if (cs.isEmpty)
                stack == 0
            else if (cs.head == '(')
                m(cs.tail, stack + 1)
            else // ')'
                m(cs.tail, stack - 1)
        }

        m(chars.filter(c => c == '(' || c == ')'), 0)
    }

    /**
     * Exercise 3
     */
    def countChange(money: Int, coins: List[Int]): Int = {

        def c(m: Int, cs: List[Int]): Int = {
            m match {
                case 0 => 1
                case i if i > 0 && !cs.isEmpty => c(i - cs.head, cs) + c(i, cs.tail)
                case _ => 0
            }
        }
        c(money, coins.sortWith((a, b) => a > b))
    }
}
