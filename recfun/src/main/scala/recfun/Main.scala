package recfun

object Main {

    def main(args: Array[String]) {
        println("Pascal's Triangle")
        for (row <- 0 to 10) {
            for (col <- 0 to row)
                print(pascal(col, row) + " ")
            println()
        }
    }

    /**
     * Exercise 1
     */
    def pascal(c: Int, r: Int): Int = {
        c match {
            case 0 => 1
            case i if i > r => 0
            case _ => pascal(c - 1, r - 1) + pascal(c, r - 1)
        }
    }

    /**
     * Exercise 2
     */
    def balance(chars: List[Char]): Boolean = {

        def balIter(cs: List[Char], stackCount: Int): Boolean = {
            if (stackCount < 0)
                false
            else if (cs.isEmpty)
                stackCount == 0
            else if (cs.head == '(')
                balIter(cs.tail, stackCount + 1)
            else // ')'
                balIter(cs.tail, stackCount - 1)
        }

        balIter(chars.filter(c => c == '(' || c == ')'), 0)
    }

    /**
     * Exercise 3
     */
    def countChange(money: Int, coins: List[Int]): Int = {

        def countIter(m: Int, cs: List[Int]): Int = {
            m match {
                case 0 => 1
                case i if i > 0 && !cs.isEmpty => countIter(i - cs.head, cs) + countIter(i, cs.tail)
                case _ => 0
            }
        }
        countIter(money, coins.sortWith((a, b) => a > b))
    }
}
