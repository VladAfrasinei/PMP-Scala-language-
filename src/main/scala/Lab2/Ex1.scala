package Lab2

object ex1
{

abstract class Persoana
{
    
}

class Student(val nume: String, val prenume: String, val materii: (Array[String], Int) ) extends Persoana
{
    val anstudent: Int=1
    def set_nume(nume1: String)
    {
        this.nume=nume1
    }
    def set_an(an1: Int)
    {
        this.an=an1
    }
    def get_nota(materie1: String)
    {
        for(m1 : materii)
            if(m1._1==materi1)
                return m1._2
    }
    def add_nota(materie1: String, nota1: Int)
    {
        val materie2:(Array[String],Int)=("PMP",10)
        materii+=materie2
        
    }
}

class Profesor(val nume: String, val prenume: String, val materie: String) extends Persoana
{

}

def studentianx(val studenti:Array[String], val an1:Int, val allstudents:Array[Student])
{
    for(s1 : allstudents)
        for(s2 : studenti)
            if(s1==s2.nume&&an1==s2.an)
                println(s2)

}

def main(args: Array[String]) {
		val materii = Array("PMP", ".NET", "IMR","ISSA","3D")
        
	}

}