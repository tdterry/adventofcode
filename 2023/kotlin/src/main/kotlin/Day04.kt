// Day04

import java.io.File

fun main(args: Array<String>) {
    val input = File("input04.txt").readLines()
    val lineRegex = Regex("""^Card\s+(\d+): (.*)$""")

    var total = 0.0
    val matches: MutableMap<Int, Int> = mutableMapOf();
    for (line in input) {
        println(line)
        val match = lineRegex.find(line)
        if (match != null) {
            println(match.groupValues[1])

            val game = match.groupValues[1].toInt()
            val parts = match.groupValues[2].split("|").map { it.trim() }

            val spaces = Regex("""\s+""")
            val winningNumbers = spaces.split(parts[0]).map { it.toInt() }
            val cardNumbers = spaces.split(parts[1]).map { it.toInt() }

            val count = cardNumbers.count(winningNumbers::contains)

            val cardCount = matches.getOrDefault(game, 0) + 1
            matches[game] = cardCount
            for (j in 1..count) {
                matches[game + j] = matches.getOrDefault(game + j, 0) + cardCount
            }

            val value = if (count > 0) Math.pow(2.0, (count - 1).toDouble()) else 0.0

            total += value
            println(value)
        }

    }

    println(total)
    println(matches)
    println(matches.values.sum())

}
