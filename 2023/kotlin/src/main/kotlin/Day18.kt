// Day18

import java.io.File

class Segment(val left: Long, val right: Long, val y: Long) {
    override fun toString(): String {
        return "Segment2($left, $right, $y)"
    }
}

fun cmpSegment2(a: Segment, b: Segment): Int {
    if (a.y < b.y || a.y == b.y && a.left < b.left) return -1
    if (a.y > b.y || a.y == b.y && a.left > b.left) return 1
    return 0
}

class Row {
    var segments = listOf<Segment>()

    fun length(): Long {
        return this.segments.map { it.right - it.left + 1 }.sum()
    }

    fun addSegment(segment: Segment): Long {
//        println("addSegment current=${this.segments} new=$segment")
        var found = false
        // look for segments that shrink the current row
        this.segments = this.segments.map {
            if (it.left == segment.left && it.right == segment.right) {
//                println("found full segment: $it")
                found = true
                listOf()
            } else if (it.left == segment.left) {
//                println("found left match segment: $it")
                found = true
                listOf(Segment(segment.right, it.right, 0L))
            } else if (it.right == segment.right) {
//                println("found right match segment: $it")
                found = true
                listOf(Segment(it.left, segment.left, 0L))
            } else if (segment.left > it.left && segment.right < it.right) {
//                println("found interior match segment: $it")
                found = true
                listOf(Segment(it.left, segment.left, 0L), Segment(segment.right, it.right, 0L))
            } else {
                listOf(it)
            }
        }.flatten()

        var extra = 0L
        if (!found) {
//            println("adding new segment: $segment")
            extra = segment.right - segment.left + 1
            this.segments =
                (this.segments + listOf(Segment(segment.left, segment.right, 0L))).sortedWith(::cmpSegment2)
        }

//        println("cleaning up segments: ${this.segments}")
        this.segments = this.segments.fold(listOf()) { acc, s ->
            if (acc.isEmpty()) {
                listOf(s)
            } else {
                val last = acc.last()
                if (last.right == s.left) {
                    extra -= 1
                    acc.dropLast(1) + listOf(Segment(last.left, s.right, 0L))
                } else {
                    acc + listOf(s)
                }
            }
        }

//        println("cleaned up segments: ${this.segments}")

        return extra
    }

    override fun toString(): String {
        return "Row(${this.segments})"
    }
}

fun main(args: Array<String>) {
    val input = File("input18.txt").readLines()

    var x = 0L
    var y = 0L
    var segments = listOf<Segment>()
    input.forEachIndexed { j, row ->
        Regex("""(\w) (\d+) \(#([0-9a-f]{6})\)""").findAll(row).forEach { match ->
            val (op, arg, color) = match.destructured
            var dir = op
            var len = arg.toLong()

            // part 2
            len = color.substring(0, 5).toLong(radix=16)
            dir = when(color[5]) {
                '0' -> "R"
                '1' -> "D"
                '2' -> "L"
                '3' -> "U"
                else -> "X"
            }

            when(dir) {
                "U" -> y -= len
                "D" -> y += len
                "L" -> {
                    segments = segments + Segment(x - len, x, y)
                    x -= len
                }
                "R" -> {
                    segments = segments + Segment(x, x + len, y)
                    x += len
                }
            }
        }
    }

    segments = segments.sortedWith(::cmpSegment2)

    var total = 0L
    val row = Row()
//    println("row: $row")
    var lastY = segments[0].y
    for (s in segments) {
//        println("${s.y}")
        if (s.y != lastY) {
//            println("area since last row: ${s.y - lastY} * ${row.length()}")
            total += (s.y - lastY) * row.length()
        }

//        println("adding segment: $s")
        val extra = row.addSegment(s)
        lastY = s.y
//        println("extra: $extra")
        total += extra
//        println("row: $row")
//        println("cumulative area: $total2")
    }

    println("area: $total")
}
