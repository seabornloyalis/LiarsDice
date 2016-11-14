import com.cra.figaro.language._ 
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.discrete._
import scala.collection.mutable.Map

object ProblemB{ 
   def main(args: Array[String]) { 
      val d6 = FromRange(1,7)
      val d8 = FromRange(1,9)
      val d6d8 = Apply(d6, d8, (i: Int, j: Int) => i+j)
      val sampleHelloWorld = VariableElimination(d6d8)
      sampleHelloWorld.start()
      println("Probability of 11 on a d6 and d8:")
      println(sampleHelloWorld.probability(d6d8, 11))
      sampleHelloWorld.stop()
      sampleHelloWorld.kill()
   }
} 

object ProblemF{ 
   def main(args: Array[String]) { 
      val d6 = FromRange(1,7)
      val nheads = Chain(d6, (i: Int) => Binomial(i, 0.5))
      val sampleHelloWorld = VariableElimination(nheads)
      sampleHelloWorld.start()
      println("Probability of 3 heads given d6 flips")
      println(sampleHelloWorld.probability(nheads, 3))
      sampleHelloWorld.stop()
      sampleHelloWorld.kill()
   }
} 

object ProblemP{ 
   def main(args: Array[String]) {
      val d6 = FromRange(1,7)
      val d8 = FromRange(1,9)
      val d12 = FromRange(1, 13)
      val d20 = FromRange(1, 21)
      val bowl1 = Select(14.0 -> d20, 14.0 -> d12, 9.0 -> d8, 9.0 -> d6)
      val bowl2 = Select(14.0 -> d20, 14.0 -> d12, 9.0 -> d8, 9.0 -> d6)
      
      //with replacement
      val sumGt16 = Apply(Chain(bowl1, (x: Element[Int]) => x), 
                           Chain(bowl2, (y: Element[Int]) => y),
                           (x: Int, y: Int) => ((x + y) > 16))
      val sheet = VariableElimination(sumGt16)
      sheet.start()
      val probRightMark = sheet.probability(sumGt16, true)
      println("Average num on right:")
      println(probRightMark * 30)
      sheet.stop()
      sheet.kill()
   }
}

object ProblemQ{
   def main(args: Array[String]) {
      val payouts = Array(1.00, 0.50, 0.25, -0.10)
      val d6 = FromRange(1,7)
      val d8 = FromRange(1,9)
      val d12 = FromRange(1, 13)
      val d20 = FromRange(1, 21)
      val bowl1 = Select(14.0 -> d20, 14.0 -> d12, 9.0 -> d8, 9.0 -> d6)
      val bowl2 = Select(14.0 -> d20, 14.0 -> d12, 9.0 -> d8, 9.0 -> d6)

      val pairDist = Apply(bowl1, bowl2,
                              (x: Element[Int], y: Element[Int]) => (x,y))
      val fullDist = Apply(Chain(bowl1, (x: Element[Int]) => x), 
                           Chain(bowl2, (y: Element[Int]) => y),
                           (x: Int, y: Int) => {   if (x + y > 16) "gt"
                                                   else if (x + y == 16) "e"
                                                   else "lt"})

      var winnings = 0.0
      val try_nums = 20

      for (tries <- 1 to try_nums) {
         println("\nTrial #" + tries + ":")
         bowl1.generate()
         bowl2.generate()
         val die1 = bowl1.value
         val die2 = bowl2.value
         val mark = Apply(die1, die2, (x: Int, y: Int) => { if (x + y > 16) "gt"
                                                            else if (x + y == 16) "e"
                                                            else "lt"})
         val marker = Importance(mark)
         var sheet = new Array[String](30)

         for (i <- 0 to 29) {
            marker.sample()._2.values.foreach{ k =>
               k match {
                  case s: String => sheet(i) = s
                  case _ => throw new ClassCastException
               }
            }
         }
         
         var probMap = Map((d6,d6) -> 0.0, (d6,d8) -> 0.0, (d6,d12) -> 0.0, (d6,d20) -> 0.0,
                           (d8,d8) -> 0.0, (d8,d12) -> 0.0,(d8,d20) -> 0.0, (d12,d12) ->0.0, 
                           (d12,d20) ->0.0,(d20,d20) ->0.0)
         for (i <- 0 to 29) {
            fullDist.observe(sheet(i)) 
            val samp = Importance(2000, pairDist)
            samp.start()
            for (((k1,k2),v) <- probMap) {
               var pr = samp.probability(pairDist, (k1,k2))
               if (pr == 0.0)
                  pr = -10.0
               if (k1 == k2)
                  probMap((k1,k2)) = v + pr
               else if (k1 != k2)
                  probMap((k1,k2)) = v + pr * 2
            }
            samp.kill()
            fullDist.unobserve()
         }
         //println(probMap)

         var top_probs = Array(0.0, 0.0, 0.0)
         var guesses = new Array[(FromRange,FromRange)](3)
         var curr_prob = 0.0
         var curr_pair = (d6,d6)
         var temp_prob = 0.0
         var temp_pair = (d6,d6)
         for (((k1,k2),v) <- probMap) {
            curr_prob = v
            curr_pair = (k1,k2)
            for (j <- 0 to 2) {
               if (curr_prob > top_probs(j)) {
                  temp_prob = top_probs(j)
                  temp_pair = guesses(j)
                  top_probs(j) = curr_prob
                  guesses(j) = curr_pair
                  curr_prob = temp_prob
                  curr_pair = temp_pair
               }
            }
         }
         println("Guesses: " + guesses(0))
         println("Guesses: " + guesses(1))
         println("Guesses: " + guesses(2))
         var bet = 0.0;
         for (gNum <- 0 to 3) {
            if (gNum < 3) {
               if (guesses(gNum)._1 == die1 && guesses(gNum)._2 == die2)
                  bet = payouts(gNum)
               else if (guesses(gNum)._1 == die2 && guesses(gNum)._2 == die1)
                  bet = payouts(gNum)
            }
            else if (bet == 0.0)
               bet = payouts(gNum)
         }
         println("Actual: " + die1 + die2)
         println("Won $" + bet)
         winnings += bet
      }
      println("\nAggregate Result:")
      println("Average win = $" + winnings / try_nums)
   }
}