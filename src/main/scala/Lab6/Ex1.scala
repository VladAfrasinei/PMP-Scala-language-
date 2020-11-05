package Lab6
import com.cra.figaro.library.atomic.{discrete,continuous}
import com.cra.figaro.language._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.library.compound._
import com.cra.figaro.language.{Element}
import com.cra.figaro.library.atomic.discrete
import com.cra.figaro.language.Chain
import com.cra.figaro.library.compound.{RichCPD, OneOf, *}
import com.cra.figaro.language.{Flip, Constant, Apply, Element}
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound._
import scala.collection.mutable.Set
import scala.language.implicitConversions

object tennis{

class Tenis(
	probP1ServeWin: Double, probP1Winner: Double, probP1Error: Double, //declararea variabilelor
	probP2ServeWin: Double, probP2Winner: Double, probP2Error: Double) {

	def rally(firstShot: Boolean, player1: Boolean): Element[Boolean] = {
		val pWinner = if (firstShot && player1) probP1ServeWin //daca player1 da prima lovitura rezulta probabilitatea ca player1 sa serveasca primul
		else if (firstShot && !player1) probP2ServeWin //daca player2 da prima lovitura probabilitatea ca player2 sa serveasca primul
		else if (player1) probP1Winner //daca pleyer1 serveste rezulta probabilitatea ca el sa castige
		else probP2Winner //daca pleyer2 serveste rezulta probabilitatea ca el sa castige
		val pError = if (player1) probP1Error else probP2Error //probabilitatea ca player1 sau player2 sa loveasca eronat(sa dea in fileu)
		val winner = Flip(pWinner)
		val error = Flip(pError)
		//daca player 2 
		If(winner, Constant(player1),//primul player castiga
		If(error, Constant(!player1),//al doilea player rateaza serva
		rally(false, !player1)))//player2 nu va servi
		}

	def game(p1Serves: Boolean, p1Points: Element[Int], p2Points: Element[Int]): Element[Boolean] = {
		val p1WinsPoint = rally(true, p1Serves) //probabilitatea ca player1 sa castige punct , daca el serveste
		val newP1Points = Apply(p1WinsPoint, p1Points, (wins: Boolean, points: Int) =>if (wins) points + 1 else points)//se adauga un punct playerului 1 daca castiga
		val newP2Points = Apply(p1WinsPoint, p2Points, (wins: Boolean, points: Int) =>if (wins) points else points + 1)//se adauga un punct playerului 2 daca castiga
		val p1WinsGame = Apply(newP1Points, newP2Points, (p1: Int, p2: Int) =>p1 >= 4 && p1 - p2 >= 2)//se verifica scorul, daca player1 are 4 puncte si player2 are cel putin cu 2 puncte mai putin
		val p2WinsGame = Apply(newP2Points, newP1Points, (p2: Int, p1: Int) => p2 >= 4 && p2 - p1 >= 2)//se verifica scorul, daca player2 are 4 puncte si player1 are cel putin cu 2 puncte mai putin
		val gameOver = p1WinsGame || p2WinsGame //se termina jocul daca unul din playeri castiga
		If(gameOver, p1WinsGame, game(p1Serves, newP1Points, newP2Points))//se incepe un nou joc cu noile puncte si daca player 1 castiga el va servi
	}

	def play(p1Serves: Boolean, p1Sets: Element[Int], p2Sets: Element[Int],p1Games: Element[Int], p2Games: Element[Int]): Element[Boolean] = {
		val p1WinsGame = game(p1Serves, Constant(0), Constant(0))//player 1 castiga daca serverste
		val newP1Games =Apply(p1WinsGame, p1Games, p2Games,(wins: Boolean, p1: Int, p2: Int) => if (wins) {if (p1 >= 5) 0 else p1 + 1} else {if (p2 >= 5) 0 else p1}) //Se incepe un nou meci, si se verifica daca p1 castiga si are peste 5 puncte din set, atunci ia un punct(castiga un game), daca p2 castiga acest set si are peste 5 puncte atunci p2 ia un punct(castiga un game) iar p1 ramane la fel
		val newP2Games =Apply(p1WinsGame, p1Games, p2Games,(wins: Boolean, p1: Int, p2: Int) =>if (wins) {if (p1 >= 5) 0 else p2} else {if (p2 >= 5) 0 else p2 + 1}) //La fel ca mai sus doar ca pentru p2
		val newP1Sets =Apply(p1WinsGame, p1Games, p1Sets,(wins: Boolean, games: Int, sets: Int) =>if (wins && games == 5) sets + 1 else sets) //Se verifica daca p1 castiga  si are 5 puncte, i se adauga inca un set castigat
		val newP2Sets =Apply(p1WinsGame, p2Games, p2Sets,(wins: Boolean, games: Int, sets: Int) =>if (!wins && games == 5) sets + 1 else sets) //La fel ca mai sus doar ca pt p2
		val matchOver =Apply(newP1Sets, newP2Sets, (p1: Int, p2: Int) =>p1 >= 2 || p2 >= 2) //Se verifica daca meciul este castigat, daca macar un player are cel putin 2 
		If(matchOver,Apply(newP1Sets, (sets: Int) => sets >= 2),play(!p1Serves, newP1Sets, newP2Sets, newP1Games, newP2Games))
	}
	play(true, Constant(0), Constant(0), Constant(0), Constant(0))
}

def main(args:Array[String]){
	
}
}