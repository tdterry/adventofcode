// Day06

import java.io.File


fun main(args: Array<String>) {
    val input = File("input06.txt").readLines()

    val times: MutableList<Double> = mutableListOf()
    val distances: MutableList<Double> = mutableListOf()
    var bigTime = 0.0
    var bigDistance = 0.0
    Regex("""Time:\s+(.+)""").find(input[0])?.let { match ->
        Regex("""\s+""").split(match.groupValues[1]).forEach { time ->
            times += time.toDouble()
        }
        print("times:")
        println(times)

        match.groupValues[1].filter { it.isDigit() }.let { digits ->
            bigTime = digits.toDouble()
        }
    }

    Regex("""Distance:\s+(.+)""").find(input[1])?.let { match ->
        Regex("""\s+""").split(match.groupValues[1]).forEach { dist ->
            distances += dist.toDouble()
        }
        print("distances:")
        println(distances)

        match.groupValues[1].filter { it.isDigit() }.let { digits ->
            bigDistance = digits.toDouble()
        }
    }

    println("bigTime: ${bigTime}")
    println("bigDist: ${bigDistance}")

    var total = 1.0
    for (i in 0 until times.size) {
        println("time: ${times[i]}")
        println("distance: ${distances[i]}")

        val min = (times[i] - Math.sqrt(times[i] * times[i] - 4 * distances[i])) / 2
        val max = (times[i] + Math.sqrt(times[i] * times[i] - 4 * distances[i])) / 2

        println("min: $min")
        println("max: $max")

        val range = Math.floor(max - 0.000001) - Math.ceil(min + 0.000001) + 1
        println("range: $range")
        total *= range
    }

    val bigMin = (bigTime - Math.sqrt(bigTime * bigTime - 4 * bigDistance)) / 2
    val bigMax = (bigTime + Math.sqrt(bigTime * bigTime - 4 * bigDistance)) / 2

    println("bigMin: $bigMin")
    println("bigMax: $bigMax")

    val bigRange = Math.floor(bigMax - 0.000001) - Math.ceil(bigMin + 0.000001) + 1
    println("bigRange: $bigRange")

    println(total.toLong())
    println(bigRange.toLong())
}
