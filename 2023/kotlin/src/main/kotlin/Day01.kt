// Day01

import java.io.File

fun main() {
    val input = File("input01.txt").readLines()
    println("Hello World!")

    for (line in input) {
        println(line)
        var first = 0
        var last = 0
        for (c in line) {
            if (c >= '0' && c <= '9') {
                if (first == 0) {
                    first = c.code - '0'.code
                }
                last = c.code - '0'.code
            }
        }

        println(first * 10 + last)
    }
}
