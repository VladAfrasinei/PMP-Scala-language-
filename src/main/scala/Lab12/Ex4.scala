package Lab12

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._

object Ex4 {
	def main(args: Array[String]) {
		val x0 = Apply(Normal(0.75, 0.3), (d: Double) => d.max(0).min(1))
        val y0 = Apply(Normal(0.4, 0.2), (d: Double) => d.max(0).min(1))
        val x = Flip(x0)
        val y = Flip(y0)
        val z = If(x === y, Flip(0.8), Flip(0.2))
 		val veAnswer = VariableElimination.probability(y, true)
         //Se cere pana la 10 milioane la ex 4
        for { i <- 10000 to 10000000 by 10000 } {
            var totalSquaredError = 0.0
            for { j <- 1 to 100 } {
                Universe.createNew()
                val x = Flip(0.8)
                val y = Flip(0.6)
                val z = If(x === y, Flip(0.9), Flip(0.1))
                z.observe(false)
                val mh = MetropolisHastings(i, ProposalScheme.default, y)
                mh.start()
                val mhAnswer = mh.probability(y, true)
                val diff = veAnswer - mhAnswer
                totalSquaredError += diff × diff
                }
            val rmse = math.sqrt(totalSquaredError / 100)
            println(i + " samples: RMSE = " + rmse)
            }
	}
}