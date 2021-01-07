package Lab12

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._


object MyChartApp extends App with scalax.chart.module.Charting {
  val data = for (i <- 1 to 5) yield (i,i)
  val chart = XYLineChart(data)
  chart.saveAsPNG("/tmp/chart.png")
}

object Ex1 {
	def main(args: Array[String]) {
		val x0 = Apply(Normal(0.75, 0.3), (d: Double) => d.max(0).min(1))
        val y0 = Apply(Normal(0.4, 0.2), (d: Double) => d.max(0).min(1))
        val x = Flip(x0)
        val y = Flip(y0)
        val z = If(x === y, Flip(0.8), Flip(0.2))
 		val veAnswer = VariableElimination.probability(y, true)
 		for { i <- 1000 to 10000 by 1000 } {
 			var totalSquaredError = 0.0
 			for { j <- 1 to 100 } {
 				val imp = Importance(i, y)
 				imp.start()
 				val impAnswer = imp.probability(y, true)
 				val diff = veAnswer - impAnswer
				//La celelalte exercitii ar fi asemanator asa ca din lipsa de timp am pus doar aici
				val data =  yield (impAnswer,diff)
				val chart = XYLineChart(data)
 				totalSquaredError += diff × diff
 				}
 		val rmse = math.sqrt(totalSquaredError / 100)
 		println(i + " samples: RMSE = " + rmse)
 		}
	}
}