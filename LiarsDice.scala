import com.cra.figaro.language._ 
import com.cra.figaro.algorithm.factored._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.atomic.discrete._
import scala.collection.mutable.Map

object LiarsDiceManager {
	var num_players = 2
	var players : Array[Player] = Array()
	var dice = 5
	var max_fails = 3
	def main(args : Array[String]) {
		if (args.length < 1) {
			println("Format:")
			println("runMain LiarsDiceManager ?humanplayer #players #dice #max_fails")
			return
		}
		if (1 <= args.length) {
			if (2 < args.length)
				num_players = args(1).toInt
			if (3 < args.length)
				dice = args(2).toInt
			if (4 < args.length)
				max_fails = args(3).toInt
			players = Array.tabulate(num_players)((i: Int) => new Player(i, dice, num_players))
			if (args(0) == "y") {
				players(0) = new HumanPlayer(0, dice,num_players)
			} else if (args(0) != "n") {
				println("First argument must be y/n : whether a human is playing")
				return
			}
			var playAgain = true
			while (playAgain) {
				playGame()
				println("Would you like to play again? y/n")
				val response = Console.readLine
				if (response == "n")
					playAgain = false
			}
		}
	}
	def playGame() {
		//TODO: beginning game init
		val remaining = remainingPlayers
		while (remaining > 1) {
			playHand(remaining)
		}
		val winner = nextPlayerId(-1)
		println("Player " + winner + " won!")
	}

	// TODO: different starters
	def playHand(remaining : Int) {
		var bidCalled = false
		var currPlayer = 1
		var currBid = (0,0)
		var bidString = ""
		var tempStrs = Array("")
		for (i <- 0 to players.length - 1) {
			players(i).beginHand(remaining)
		}
		bidString = players(0).startBid
		tempStrs = bidString.split(" ")
		currBid = (tempStrs(0).toInt, tempStrs(1).toInt)
		while (!bidCalled) {
			currPlayer = nextPlayerId(currPlayer)
			bidString = players(currPlayer).receiveBid(currBid)
			if (bidString == "call") {
				bidCalled = true
				
			}
			else {
				tempStrs = bidString.split(" ")
				currBid = (tempStrs(0).toInt, tempStrs(1).toInt)
			}
		}
		for (i <- 0 to players.length - 1) {
			players(i).endHand
		}
	}

	def remainingPlayers :Int = {
		var remaining = 0
		for (i <- 0 to players.length - 1) {
			if (players(i).getFailures < max_fails)
				remaining += 1
		}
		return remaining
	}

	def nextPlayerId(id : Int) : Int = {
		var curr = id + 1
		while (curr != id) {
			if (curr > num_players)
				curr = 0
			if (players(curr).getFailures < max_fails)
				return curr
			else
				curr += 1
		}
		return -1
	}
	def previousPlayerId(id : Int) : Int = {
		var curr = id - 1
		while (curr != id) {
			if (curr < 0)
				curr = num_players - 1
			if (players(curr).getFailures < max_fails)
				return curr
			else
				curr -= 1
		}
		return -1
	} 
}

class Player(idv : Int, sizev : Int, num_players : Int) {
	val id = idv
	val hand_size = sizev
	val d6 = FromRange(1,7)
	var myDice : Array[Element[Int]] = Array.fill(hand_size)(FromRange(1,7))
	var myHand : Array[Int] = new Array(hand_size)
	var allDice : Array[Array[Element[Int]]] = Array.ofDim(num_players, hand_size)
	var failures = 0
	var bidHistory : List[(Int,(Int,Int))] = List()
	var handHistory : List[(Int,Array[Int])] = List()
	var currentRound = 0
	def startBid : String = {
		return "0 0"
	}
	def receiveBid(bid : (Int, Int)) : String = {
		if (false)
			return "call"
		else
			return "0 0"
	}
	def incrementFailure = {failures += 1}
	def getFailures : Int = {return failures}
	def resetFailures = {failures = 0}
	def getHand : Array[Int] = {return myHand}
	def beginHand(curr_players : Int) = {
		for (i <- 0 to hand_size - 1) {
			d6.generate
			myHand(i) = d6.value
			myDice(i).observe(myHand(i))
		}
		allDice(id) = myDice
	}
	def endHand = {
		handHistory = (currentRound, myHand) :: handHistory
		currentRound += 1
		for (i <- 0 to hand_size - 1) {
			myDice(i).unobserve
		}
	}
	def getBidHistory : List[(Int,(Int, Int))]= {return bidHistory}
	def getHandHistory : List[(Int,Array[Int])]= {return handHistory}
}

class HumanPlayer (idv : Int, sizev : Int, num_players : Int)
	extends Player(idv : Int, sizev : Int, num_players : Int) {
	override def startBid : String = {
		return "0 0"
	}
	override def receiveBid(bid : (Int, Int)) : String = {
		return "0 0"
	}
}