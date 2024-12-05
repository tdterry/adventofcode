// Day07

import java.io.File

val handValues = "AKQJT98765432"
val handValues2 = "AKQT98765432J"

typealias Hand = Pair<String, Int>

fun scoreHand(hand: String): Int {
    val cards: MutableMap<Char, Int> = mutableMapOf()
    hand.forEach {
        val card = it
        cards[card] = cards.getOrDefault(card, 0) + 1
    }

    return when {
        cards.values.contains(5) -> 6
        cards.values.contains(4) -> 5
        cards.values.contains(3) && cards.values.contains(2) -> 4
        cards.values.contains(3) -> 3
        else -> cards.filter { it.value == 2 }.size
    }
}

fun scoreHand2(hand: String): Int {
    var maxScore = 0
    for (c in handValues) {
        val testHand = hand.replace('J', c)
        val score = scoreHand(testHand)
        if (score > maxScore) {
            maxScore = score
        }
    }

    return maxScore
}

fun tieBreaker(values: String, hand1: String, hand2: String): Int {
    val card1Values = hand1.map(values::indexOf)
    val card2Values = hand2.map(values::indexOf)
    val firstDiff = card1Values.zip(card2Values).filter { it.first != it.second }.first()
    return firstDiff.second.compareTo(firstDiff.first)
}

fun sort(it1: Hand, it2: Hand): Int {
    val score1 = scoreHand(it1.first)
    val score2 = scoreHand(it2.first)
    return if (score1 == score2) {
        tieBreaker(handValues, it1.first, it2.first)
    } else {
        score1.compareTo(score2)
    }
}

fun sort2(it1: Hand, it2: Hand): Int {
    val score1 = scoreHand2(it1.first)
    val score2 = scoreHand2(it2.first)
    return if (score1 == score2) {
        tieBreaker(handValues2, it1.first, it2.first)
    } else {
        score1.compareTo(score2)
    }
}

fun main(args: Array<String>) {
    val input = File("input07.txt").readLines()

    val hands = input.map {
        val (hand, bid) = it.split(" ")
        Pair(hand, bid.toInt())
    }.sortedWith(::sort)

    val winnings = hands.foldIndexed(0) { i, acc, pair ->
        val (_, bid) = pair
        acc + (i+1) * bid
    }

    println("Winnings 1: $winnings")

    val hands2 = hands.sortedWith(::sort2)
    val winnings2 = hands2.foldIndexed(0) { i, acc, pair ->
        val (_, bid) = pair
        acc + (i+1) * bid
    }
    println("Winnings 2: $winnings2")
}
