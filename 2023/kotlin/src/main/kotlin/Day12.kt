// Day12

import java.io.File

fun match(patten: String, candidate: String): Boolean {
    if (patten.length != candidate.length) {
        return false
    }

    for (i in patten.indices) {
        if (patten[i] != '?' && patten[i] != candidate[i]) {
            return false
        }
    }

    return true
}

fun fit(pattern: String, piece: String, pos: Int): Boolean {
    if (pos + piece.length > pattern.length) {
        return false
    }

    for (i in piece.indices) {
        if (pattern[pos + i] != '?' && pattern[pos + i] != piece[i]) {
            return false
        }
    }

    return true
}

var totalLines = 0
fun solutions(pattern: String, pieces: List<String>, offset: Int, candidate: String): Int {
    if (pattern.isEmpty() || pieces.isEmpty() || offset >= pattern.length - pieces.sumOf { it.length - 1 }) {
        return 0
    }

    val piece = pieces[0]
    val total = if (fit(pattern.substring(offset), piece, 0)) {
//        println("${" ".repeat(offset)+piece} at $offset")
        if (pieces.size == 1) {
            val s = candidate + piece + ".".repeat(pattern.length-piece.length-candidate.length)
            val ok = pattern.substring(offset+piece.length, pattern.length).all { it != '#' }
            if (ok && !match(pattern, s)) {
                println(pattern)
                println("$s (mismatch)")
            }
//            totalLines += 1
            if (ok) 1 else 0
        } else {
            solutions(pattern, pieces.subList(1, pieces.size), offset+piece.length-1, candidate + piece.substring(0, piece.length-1))
        }
    } else {
        0
    }

    if (pattern[offset+1] == '#') {
        // can't put a piece here
        return total
    }

    if (totalLines > 1000000) {
        return total
    }

    return total + solutions(pattern, pieces, offset+1, "$candidate.")
}

fun main(args: Array<String>) {
    val input = File("input12.txt").readLines()

    var total = 0
    input.forEachIndexed { j, row ->

        var pattern = ""
        var ranges = mutableListOf<Int>()
        Regex("([.#?]+) ([0-9,]+)").find(row)?.let {
            pattern = it.groupValues[1]
            ranges = it.groupValues[2].split(",").map { it.toInt() }.toMutableList()
        }

//        pattern = ".$pattern." // pad with empty pots
        pattern = "." + List(5){pattern}.joinToString("?") + "." // pad with empty pots
        ranges = List(5){ranges}.flatten().toMutableList()

        val pieces = ranges.map {
            (listOf(".") + List(it) { "#" } + listOf(".")).joinToString("")
        }

        println("$j: $row")
        println("$pattern $ranges")
        val n = solutions(pattern, pieces, 0, "")
        println("solutions: $n")

        total += n

//        val qCount = row.count { it == '?' }
//        for (i in 0..<(1 shl qCount)) {
//            val bin = Integer.toBinaryString(i).padStart(qCount, '0')
//            var k = 0
//            val candidate = pattern.map {
//                when (it) {
//                    '?' -> bin[k++]
//                    '.' -> '0'
//                    '#' -> '1'
//                    else -> it
//                }
//            }
////            println(candidate.joinToString(""))
//
//            var inRange = false
//            var counts = mutableListOf<Int>()
//            for (c in candidate) {
//                if (c == '1') {
//                    if (inRange) {
//                        counts[counts.size-1]++
//                    } else {
//                        counts.add(1)
//                    }
//                    inRange = true
//                } else {
//                    inRange = false
//                }
//            }
//
//            if (arrayEquals(counts, ranges)) {
//            println("counts: $counts, ranges: $ranges")
//                println("${candidate.joinToString("")} match")
//                total++
//            }
//        }
    }

    println("total: $total")
}
