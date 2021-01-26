package Partial2

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.language.{Select, Apply, Constant, Element, Chain, Universe}
import com.cra.figaro.library.compound.{If, CPD, RichCPD, OneOf, *, ^^}
import com.cra.figaro.algorithm.factored.VariableElimination


object Ex1 {
	//Aceasta va fi clasa de start de unde vom pleca cu probabilitate de 0.5 in Insorit, 0.3 in Innorat si 0.2 in Ploios dupa cum zice si modelul
	class Start{
		val tip=Select(0.5 -> 'Insorit, 0.3 -> 'Innorat, 0.2 -> 'Ploios)
	}
	//Acum vom face o clasa care va afla in functie de ora trecuta ora la care suntem acum, va face procentajele pentru ora urmatoare dupa diagrama si va afla daca ia sau nu umbrela
	class Vreme(val vr:Vreme)
	{
		//In aceasta clasa va trebui sa aflam cum va fi in ora urmatoare vreme si daca ia sau nu umbrela
		val vreme=CPD(vr.tip),
				'Insorit -> Select(0.6 -> 'Insorit, 0.3 -> 'Innorat, 0.1 -> 'Ploios), //Daca este Insorit vremea la aceasta ora, in urmatoare ora cu sansa de 0.6 poate ramane Insorit, cu sansa de 0.3 poate fi Innorat iar cu 0.1 Ploios
				'Innorat -> Select(0.15 -> 'Insorit, 0.5 -> 'Innorat, 0.35 -> 'tot), //Daca acum este vremea Innorat, in urmatoare ora vremea poate ramane Innorat cu 0.5, poate devine Insorita cu sansa de 0.15 sau poate devine Ploios cu sansa de 0.35
				'Ploios -> Select(0.15 -> 'Insorit, 0.4 -> 'Innorat, 0.45 -> 'Ploios)) //Daca acum vremea este Ploios, in urmatoarea ora poate ramane vremea asa cu sansa de 0.45, poate fi innorat cu sansa de 0.4 sau poate fi inosirt cu sansa de 0.15
		//In aceasta variabila in functie de tipul vremii vom afla daca ia sau nu umbrela.
		val umbrela=CPD(vr.tip),
				'Insorit -> Select(0.15 -> 'IaUmbrela, 0.85 -> 'NuIaUmbrela), //Daca este Insorit vremea atunci are sansa de 15% sa ia umbrela =>85% sansa sa nu o ia
				'Innorat -> Select(0.65 -> 'IaUmbrela, 0.35 -> 'NuIaUmbrela), //Daca acum este vremea Innorat, are o sansa de 65% sa ia umbrela si 35% sa nu o ia
				'Ploios -> Select(0.75 -> 'IaUmbrela, 0.25 -> 'NuIaUmbrela)) //Daca acum vremea este Ploios, are o sansa de 75% sa ia umbrela si o sansa de 25% sa nu o ia(Acest caz exista deoarece vremea se poate schimba)
		
	}
	def main(args: Array[String]) {
		//Trebuie o monitorizare de 5 ore la puncutl a
		val hours=5
		val weather: Array[Element[Symbol]] = Array.fill(chapters)(Constant('Insorit))
		//Vom face acest lucru pentru a afla prima vreme de la Start(Cea de la pasul 0)
		val Firstweather = new Start()
		//Vom pune la inceput prima vreme
		weather(0)=Firstweather
		//Afisam vremea de la pasul 0
		println(Firstweather)
		for { hour <- 1 until hours }
		{	//Vom face acest lucru pentru a avea in tip vremea care a fost ora trecuta si o vom apela ca constructor la clasa vreme
			val tip=new Vreme(weather(hour-1))
			//Afisam vremea de la aceasta ora
			println(tip.vreme)
		}
		//Punctul b
		//Vom pune vremea Innorat in ultimele 2 ore
		weather[4].vreme.setCondition((s: String) => s = Innorat)
		weather[5].vreme.setCondition((s: String) => s = Innorat)
		//Aflam care este posibilitatea ca la ora 6 sa ia umbrela daca a fost inmorat ultimele 2 ore(ora 4 si 5)
		println(VariableElimination.probability(weather(6).umbrela, 'IaUmrbela))
		//Puncutl c
		weather[4].vreme.removeConditions()
		weather[5].vreme.removeConditions()
		//Vom seta ca la ora 5 nu ia umbrela
		weather[5].umbrela.setCondition((s: String) => s = NuIaUmbrela)
		//Daca la ora 5 nu luam umbrela, vom afla care e probabilitatea ca la ora 2(acum 3 ore) sa fi plouat(sa fie ploios)
		println(VariableElimination.probability(weather(2).vreme, 'Ploios))

	}
}
