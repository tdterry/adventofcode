// Day11

import java.io.File
import kotlin.math.exp

fun main(args: Array<String>) {
    val input = File("input11.txt").readLines()

    val universe = mutableListOf<String>()
    val expandedRows = mutableListOf<Int>()
    val expandedCols = mutableListOf<Int>()
    input.forEachIndexed { j, row ->
        if (row.all { it == '.' }) {
            expandedRows.add(j)
        }
        universe.add(row)
    }

    for (i in 0 until input[0].length) {
        if (input.all { it[i] == '.' }) {
            expandedCols.add(i)
        }
    }

    println(expandedRows)
    println(expandedCols)

    universe.forEach(::println)

    val galaxies = mutableListOf<Pair<Long, Long>>()
    for (y in 0 until universe.size) {
        for (x in 0 until universe[y].length) {
            if (universe[y][x] == '#') {
                galaxies.add(Pair(x.toLong(), y.toLong()))
            }
        }
    }

    println(galaxies)
    var total1 = 0L
    var total2 = 0L
    for (i in 0 until galaxies.size) {
        for (j in i + 1 until galaxies.size) {
            val (x1, y1) = galaxies[i]
            val (x2, y2) = galaxies[j]
            val (minX, maxX) = if (x1 < x2) Pair(x1, x2) else Pair(x2, x1)
            val (minY, maxY) = if (y1 < y2) Pair(y1, y2) else Pair(y2, y1)

            var dist1: Long = maxX - minX + maxY - minY
            dist1 += 1L * expandedRows.filter { it in minY..maxY }.size
            dist1 += 1L * expandedCols.filter { it in minX..maxX }.size
            total1 += dist1

            var dist2: Long = maxX - minX + maxY - minY
            dist2 += 999999L * expandedRows.filter { it in minY..maxY }.size
            dist2 += 999999L * expandedCols.filter { it in minX..maxX }.size
            total2 += dist2
        }
    }

    println(total1)
    println(total2)
}
