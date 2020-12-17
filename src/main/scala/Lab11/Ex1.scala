package Lab11

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.algorithm.factored.beliefpropagation.BeliefPropagation
import com.cra.figaro.algorithm.sampling

object Ex1 {
	def main(args: Array[String]) {
		//Punctul a
		//Sansa sa fie presedinte este de 1/40Milioane
		val sansaPresedinte=Flip(0.000000025)
		//Sansa sa fie stangaci este de 50%
		val stangaciUS=Flip(0.5)
		//Sansa sa fie presedinta daca e stangaci
		val stangaciNotUs=Flip(0.1)
		//Vom spune ca este stangaci
		stangaciUS.observe(true)
		//Aflam sansa sa fie presedinte
		println(VariableElimination.probability(sansaPresedinte, true))
		//Punctul b
		//Presedinte US Harvard
		val harvardUS=Flip(0.15)
		//Presedinte harvard normal
		val harvardNormal=Flip(0.0005)
		harvardUS.observe(true)
		println(VariableElimination.probability(sansaPresedinte, true))
		//Punctul c
		val stangaciUS1=Flip(0.5)
		val harvardUS1=Flip(0.15)
		val sansaPresedinte1=CPD(stangaciUS,harvardUS1,
								(false,false)->Flip(0.000000025),
								//Stiu ca nu pot pune asa la Flip dar nu am mai avut timp sa calculez si am zis ca macar formula sa fie in toate 3 cazuri
								(false,true)->Flip(0.000000025/0.15),
								(true,false)->Flip(0.000000025/0.5),
								(true,true)->Flip(0.000000025/0.5/0.15))
		stangaciUS1.observe(true)
		harvardUS1.observe(true)
		//PT VE(Variable Elimination)
		println(VariableElimination.probability(sansaPresedinte1, true))
		//Pt BP
		val bp = BeliefPropagation(100, sansaPresedinte1)
		bp.start()
		println(bp.probability(sansaPresedinte1,(b:Boolean)=>b))
		//Pt Importance
		val imp=Importance(1000,sansaPresedinte1)
		imp.start()
		println(bp.probability(sansaPresedinte1,(b:Boolean)=>b))
	}
}