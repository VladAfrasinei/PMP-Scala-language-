package Lab4 

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.library.atomic.discrete._

object Ex4 {
	def main(args: Array[String]) {
        val die1 = FromRange(1, 7)
        val die2 = FromRange(1, 7)
        val die3 = FromRange(1,7)
        val total = Apply(die1, die2, die3, (i1: Int, i2: Int, i3: Int) => i1 + i2+i3)

        println(VariableElimination.probability(total, 11))
        
        total.addCondition((i: Int) => i > 8)
        println(VariableElimination.probability(die1, 6))
        
        total.addCondition((i: Int) => i < 10)
        println(VariableElimination.probability(die2, 3))

        total.addCondition((i: Int) => i == 9)
        println(VariableElimination.probability(die3, 4))

        def doubles = {
        val die4 = FromRange(1, 7)
        val die5 = FromRange(1, 7)
                die4 === die5
                }
        val jail = doubles && doubles && doubles
        println(VariableElimination.probability(jail, true))


        val numberOfSides1 =
        Select(0.2 -> 4, 0.2 -> 6, 0.2 -> 8, 0.2 -> 12, 0.2 -> 20)
        val roll1 = Chain(numberOfSides1, ((i: Int) => FromRange(1, i + 1)))
        val numberOfSides2 =
        Select(0.2 -> 4, 0.2 -> 6, 0.2 -> 8, 0.2 -> 12, 0.2 -> 20)
        val roll2 = Chain(numberOfSides2, ((i: Int) => FromRange(1, i + 1)))
        def stickinessConstraint(sidesPair: (Int, Int)) =
        if (sidesPair._1 == sidesPair._2) 1.0 else 0.5
        val pairOfSides = ^^(numberOfSides1, numberOfSides2)
         pairOfSides.addConstraint(stickinessConstraint)
        println(VariableElimination.probability(roll2, 7))
	}
}