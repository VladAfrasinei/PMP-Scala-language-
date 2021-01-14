package Partial1

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._

object Ex1 {
	def main(args: Array[String]) {
		//Avem 90% sansa sa pune alarma
		val alarma= Flip(0.9)
		//Va trebui sa facem cele 2 cazuri
		//Daca alarma este true, are sansa de 90% sa nu adoarma la loc, iar daca e false(nu a setat alarma) ai o sansa de 10% sa te trezesti la timp(treaz sa fie true) 
		val treaz= Chain(alarma,
			(a: Boolean) =>
				if (a) Flip(0.9)
				else Flip(0.1))
		//Avem o sansa de 80% sa ajunga la timp autobuzul
		val autobuz=Flip(0.8)
		//Vom face cele 4 cazuri din tabel
		val final = RichCPD(treaz, autobuz,
      		(OneOf(true), OneOf(true)) -> Flip(0.1),
      		(OneOf(true), OneOf(false)) -> Flip(0.3),
      		(OneOf(false), OneOf(true)) -> Flip(0.2)),
			(OneOf(false), OneOf(false)) -> Flip(0.9)) 
	//punctul a
	//Va trebui sa punem pe false deoarece spune ca a adormit si la noi asta face variabila treaz
	treaz.observe(false)
	//vrem sa vedem care sunt sansele sa ajunga la timp
	println(VariableElimination.probability(final, true))
	treaz.unobserve()
	//punctul b
	//Vom da ca am ajuns la serviciu
	final.observe(true)
	//Vom vedea care este probabilitatea ca alarma sa fi fost setata
	println(VariableElimination.probability(alarma, true))
	//Punctul c
	//Vom da ca alarma a sunat
	alarma.observe(true)
	//Vrem sa stim care este probabilitatea sa fi adormit la loc, adica treaz sa fie false
	println(VariableElimination.probability(treaz, false))
	
	}
}