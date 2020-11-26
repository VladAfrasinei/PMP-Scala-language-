package Lab8

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
//O sa fac ca finance sa tina de human resources, o sa spun ca ei cer o finantare
//Research and Develop o sa tina de finantare, cu cat e mai mare cu atat mai bine
//Product o sa tina de research, cu cat muncesc mai mult cu atat e product ul mai mare
//Sales tine de product, cu cat se produce mai mult cu atat se poate vinde mai mult
object Ex1 {
	abstract class Department
	{
		//O sa pune ca avem un numar random de angajati intre 10 si 100
		val angajati=FromRange(10,100)
		//O sa alegem o valoare random intre 100 si 1000 de angajat
		val bugetDeAngajat=FromRange(100,1000)
		//bugetul total al acelui departament
		val bugetTotal=Apply(angajati,bugetDeAngajat,(i1: Int, i2: Int)=>i1*i2)
		
	}
	class ResearchAndDevelopment(val angajati,val bugetTotal) extends Department(val angajati,val bugetTotal)
	{
			var obj=new Finance()
			//Folosim aceeasi euristica
			val eficienta=Apply(obj.returnEficienta(),(i:Int) =>i*10)
			def returnEficienta(eficienta)=eficienta
	}
	class Production(val angajati,val bugetTotal) extends Department(val angajati,val bugetTotal){
		  //Vom face o sansa de reusita a producerii productului de 80%
		  val reusesc=Flip(0.8)
		  var obj=new ResearchAndDevelopment()
		  val eficientaProduct=Apply(obj.returnEficienta(),(i:Int) =>i*10)
		  //Daca vom reusi sa l producem, vom avea eficienta product, daca nu va da mesajul
		  val eficienta=Chain(reusesc, (side:Boolean) =>
		  	if(!reusesc)
			  {Constant("Nu am reusit sa l producem")}
			else
				eficientaProduct)
		  def returnEficienta(eficienta)=eficienta
	}
	class Sales(val angajati,val bugetTotal) extends Department(val angajati,val bugetTotal){
		//Vom spune ca daca l au produs il pot face vinde o sansa de 95%
		val reusesc=Flip(0.95)
		var obj=new ResearchAndDevelopment()
		val eficientaSales=Apply(obj.returnEficienta(),(i:Int) =>i*10)
		val eficienta=Chain(reusesc, (side:Boolean) =>
		  	if(!reusesc)
			  {Constant("Nu am reusit sa l vindem")}
			else
				eficientaSales)
		  def returnEficienta(eficienta)=eficienta
	}
	class HumanResources(val angajati,val bugetTotal) extends Department(val angajati,val bugetTotal){

			//Facem o euroistica simpla de eficienta, unde va fi 10*bugettotal
			val eficienta = Apply(bugetTotal, (s:Int) => s*10)
			//Facem o functie care va returna eficienta
			def returnEficienta(eficienta)=eficienta

	}
	class Finance(val angajati,val bugetTotal) extends Department(val angajati,val bugetTotal){

			var obj=new HumanResources()
			//Folosim aceeasi euristica
			val eficienta=Apply(obj.returnEficienta(),(i:Int) =>i*10)
			def returnEficienta(eficienta)=eficienta 
	}

	def main(args: Array[String]) {
		var obj1=new HumanResources()
		var obj2=new Finance()
		var obj3=new ResearchAndDevelopment()
		var obj4=new Product()
		var obj5=new Sales()
		println(obj1.returnEficienta)
		println(obj2.returnEficienta)
		println(obj3.returnEficienta)
		println(obj4.returnEficienta)
		println(obj5.returnEficienta)
	}
}