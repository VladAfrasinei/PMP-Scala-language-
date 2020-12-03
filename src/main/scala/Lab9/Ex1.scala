package Lab9

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Ex1 {
	val chapters=10
	val initial=Universe.createNew()
	Constant(3){"score", initial}
	Constant(8){"learned", initial}
	Constant(5){"difficulty", initial}
	//Le am dat de tip float deoarece am considerat ca aceste valori pot fi si cu virgula
	def transition(learned: Float, difficulty: Float):
(Element[(Float, Float, Float)]) ={
	//Vom pune o probabilitate ca elevul sa invete si acest capitol
	val learnedChance=Flip(0.95)
	if(learnedChance)
		val newlearnead=learned-difficulty/10 //Daca a invatat, avand in vedere ca dificultatea este mai grea, va invata mai putin decat tura trecuta.Am ales sa fac eu o euristica in care fiecar learned va fi cat a invatat si tura trecuta din care scadem dificulty pe 10, iar dificulty de la capitol la capitol are valori intre 1 si 10
	else
		val newlearnead=learned*0,5-difficulty/10//Daca nu am invatat, tot asa scadeam difficulty pe 10, dar din jumatate din cat a invatat in universul trecut, ajutandu l ce stia capitolul trecut
	val newdifficulty=difficulty+1//Creste dificultatea de la capitol la capitol
	//Inca un motiv pt care am ales Float este ca scoreul pana la urma nu este mereu intreg
	val score=0,8*newlearned+0,2*newdifficulty//Vom spune ca va optine nota ca 80% din cat a invatat +20% din dificultate
	^^(newlearnead, newdifficulty, score)//Vom returna aceasta tripleta
}
	def nextUniverse(previous: Universe): Universe = {
		val next=Universe.createNew()
		val previouslearned=previous.get[Float]("learned")
		val previousdifficulty=previous.get[Float]("difficulty")
		val state=Chain(previouslearned,previousdifficulty, transition)
		Apply(state, (s:(Float,Float,Float))=> s._1) ("learned",next)
		Apply(state, (s:(Float,Float,Float))=> s._2) ("difficulty",next)
		Apply(state, (s:(Float,Float,Float))=> s._3) ("score",next)
	}
	def main(args: Array[String]) {
		val scoreObservation=List(Some(10),Some(10),Some(9),None,None,None,None,None,None,None)
		val alg=ParticleFilter(initial,nextUniverse,100)
		alg.start()
		for  (i <- 1 to 10)
		{
			val e={scoreObservation(i) match{
						case None=>List()
						case Some(n)->List(NamedEvidence("score"),Observation(n))
				}
			}
			alg.advanceTime(e)
			print("Chapter"+time+"=")
			println("learning="+alg.currentExpectation("score",(i:Float)=>i))
		}
		val algorithm = Importance(1000, test)
		algorithm.start()
		
		println(algorithm.probability(test, "Test"))
	}
}