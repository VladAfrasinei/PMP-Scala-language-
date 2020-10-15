package Lab3

import com.cra.figaro.algorithm.factored._
import com.cra.figaro.language._
import com.cra.figaro.library.compound._

object Covid {

     def main(args: Array[String]) {
         val febra=Flip(0.04)
         val tuse=Flip(0.06)

        val sansecovid=CPD(febra,tuse,
                            (false,false)-> Flip(0.001),
                            (true,false)->  Flip(0.5),
                            (false,true)->  Flip(0.3),
                            (true,true)-> Flip(0.9))

        val pozitivcovid=CPD(sansecovid,
                            false-> Flip(0.2),
                            true->Flip(0.8))
        

 val alg = VariableElimination(febra,tuse,sansecovid)
 alg.start()
 println("Probability of febra: " + alg.probability(febra,true))
 alg.kill
 tuse.observe(false)
 febra.observe(false)
 val alg1 =  VariableElimination(sansecovid)
 alg1.start()
println("Probability of asimptomatic: " + alg1.probability(sansecovid,true))
alg1.kill
 
 }
} 