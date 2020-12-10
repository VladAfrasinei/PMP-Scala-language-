package Lab10

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Ex1 {
	val times=10
	val initial=Universe.createNew()
	val procentajInvestie=0.2
	Constant(100000){"capital", initial}
	Constant(0){"profit", initial}
	Constant(0){"investment", initial}
	//Le am dat de tip float deoarece am considerat ca aceste valori pot fi si cu virgula
	def transition(capital: Int):
		(Element[(Int, Int, Int)]) ={
    //Vom spune ca investia curenta este acel procentaj din capital
	val newinvestment=capital*procentajInvestie
	//Vom spune ca sansa sa avem profit pozitiv este de 50%
	val sansaProfit=Flip(0.5)
	if(sansaProfit)
		val newprofit=1.1*newinvestment //Daca avem profit pozitiv, vom avea 110% din investment(vom castiga 10%)
	else
		val newprofit=0.9*newinvestment//Daca e negativ vom pierde 10%
	newcapital=capital-newinvestment+newprofit
	^^(newcapital, newinvestment, newprofit)
	
	
}
	def nextUniverse(previous: Universe): Universe = {
		val next=Universe.createNew()
		val previouscapital=previous.get[Int]("capital")
		val state=Chain(previouscapital, transition)
		Apply(state, (s:(Float,Float,Float))=> s._1) ("capital",next)
		Apply(state, (s:(Float,Float,Float))=> s._2) ("investment",next)
		Apply(state, (s:(Float,Float,Float))=> s._3) ("profit",next)
	}
	def main(args: Array[String]) {
		//Pentru investment 20%
		val capitalObservation=List(Some(100000),None,None,None,None,None,None,None,None,None)
		val alg=ParticleFilter(initial,nextUniverse,100)
		alg.start()
		for  (i <- 1 to 10)
		{
			val e={scoreObservation(i) match{
						case None=>List()
						case Some(n)->List(NamedEvidence("capital"),Observation(n))
				}
			}
			alg.advanceTime(e)
			print("time"+time+"=")
			println("expected="+alg.currentExpectation("capital",(i:Int)=>i))
		}
		val algorithm = Importance(1000, test)
		algorithm.start()
		println(algorithm.probability(test, "Test"))
	}
	//Pentru investment 50%
	//Ideea este ca ar fi trebuit sa schimb si functiile de mai sus, nu pot aloca asa o valoare, dar avand in vedere ca doar asta se schimba am spus sa arat ideea macar
	procentajInvestie=0.5
	//Dupa este la fel ca mai sus doar ca cu alta denumire
		val capitalObservation2=List(Some(100000),None,None,None,None,None,None,None,None,None)
		val alg2=ParticleFilter(initial,nextUniverse,100)
		alg.start()
		for  (i <- 1 to 10)
		{
			val e={scoreObservation(i) match{
						case None=>List()
						case Some(n)->List(NamedEvidence("capital"),Observation(n))
				}
			}
			alg.advanceTime(e)
			print("time"+time+"=")
			println("expected="+alg.currentExpectation("capital",(i:Int)=>i))
		}
		val algorithm = Importance(1000, test)
		algorithm.start()
		println(algorithm.probability(test, "Test"))
	}
	//Pentru investment 5%
	procentajInvestie=0.05//Tot asa am zis sa arat ideea
		val capitalObservation3=List(Some(100000),None,None,None,None,None,None,None,None,None)
		val alg3=ParticleFilter(initial,nextUniverse,100)
		alg.start()
		for  (i <- 1 to 10)
		{
			val e={scoreObservation(i) match{
						case None=>List()
						case Some(n)->List(NamedEvidence("capital"),Observation(n))
				}
			}
			alg.advanceTime(e)
			print("time"+time+"=")
			println("expected="+alg.currentExpectation("capital",(i:Int)=>i))
		}
		val algorithm = Importance(1000, test)
		algorithm.start()
		println(algorithm.probability(test, "Test"))
	}
}