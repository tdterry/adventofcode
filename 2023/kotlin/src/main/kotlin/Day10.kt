// Day10

import util.Direction
import java.io.File

val directions = mapOf<Pair<Char, Direction>, Direction>(
    Pair('|', Direction.UP) to Direction.UP,
    Pair('|', Direction.DOWN) to Direction.DOWN,
    Pair('-', Direction.LEFT) to Direction.LEFT,
    Pair('-', Direction.RIGHT) to Direction.RIGHT,
    Pair('L', Direction.DOWN) to Direction.RIGHT,
    Pair('L', Direction.LEFT) to Direction.UP,
    Pair('J', Direction.DOWN) to Direction.LEFT,
    Pair('J', Direction.RIGHT) to Direction.UP,
    Pair('7', Direction.UP) to Direction.LEFT,
    Pair('7', Direction.RIGHT) to Direction.DOWN,
    Pair('F', Direction.UP) to Direction.RIGHT,
    Pair('F', Direction.LEFT) to Direction.DOWN,
)

fun move(pos: Pair<Int, Int>, dir: Direction): Pair<Int, Int> {
    return when (dir) {
        Direction.UP -> Pair(pos.first, pos.second - 1)
        Direction.DOWN -> Pair(pos.first, pos.second + 1)
        Direction.LEFT -> Pair(pos.first - 1, pos.second)
        Direction.RIGHT -> Pair(pos.first + 1, pos.second)
    }
}

fun main(args: Array<String>) {
    val map = mutableMapOf<Pair<Int, Int>, Char>()
    val input = File("input10.txt").readLines()
    var start: Pair<Int, Int>? = null
    var maxX = 0
    var maxY = 0
    input.forEachIndexed { y, line ->
        line.forEachIndexed { x, char ->
            if (char == 'S') {
                start = Pair(x, y)
            }
            map[Pair(x, y)] = char
            print(char)
            maxX = x
        }
        println()
        maxY = y
    }

    var maxDist = 0
    var maxPath = listOf<Pair<Int, Int>>()
    var initialDir: Direction? = null
    var finalDir: Direction? = null
    listOf(Direction.DOWN, Direction.LEFT, Direction.UP, Direction.RIGHT).forEach { startDir ->
        println("starting at ${start} ${startDir}")

        var pos = move(start!!, startDir)
        var dir = startDir
        if (map[pos] == null) {
            return@forEach
        }

        var dist = 1
        val path = mutableListOf<Pair<Int, Int>>(pos)
        while (pos != start) {
//            println("at ${pos} ${dir} ${map[pos]}")

            val nextDir = directions[Pair(map[pos], dir)]

//            println("checking ${pos} (${map[pos]}) ${dir} -> ${nextDir}")

            nextDir ?: break

            pos = move(pos, nextDir)
            path.add(pos)
            dir = nextDir
            dist += 1
        }

        if (pos == start) {
//            println("found a loop ${dist}")
            if (dist > maxDist) {
                maxDist = dist
                maxPath = path
                initialDir = startDir
                finalDir = dir
            }
        }
    }

    println("initial: ${initialDir} final: ${finalDir}")

    map[start!!] = when (Pair(initialDir!!, finalDir!!)) {
        Pair(Direction.DOWN, Direction.DOWN) -> '|'
        Pair(Direction.UP, Direction.UP) -> '|'

        Pair(Direction.RIGHT, Direction.RIGHT) -> '-'
        Pair(Direction.LEFT, Direction.LEFT) -> '-'

        Pair(Direction.DOWN, Direction.LEFT) -> 'F'
        Pair(Direction.RIGHT, Direction.UP) -> 'F'

        Pair(Direction.LEFT, Direction.DOWN) -> 'J'
        Pair(Direction.UP, Direction.RIGHT) -> 'J'

        Pair(Direction.DOWN, Direction.RIGHT) -> '7'
        Pair(Direction.LEFT, Direction.UP) -> '7'

        Pair(Direction.RIGHT, Direction.DOWN) -> 'L'
        Pair(Direction.UP, Direction.LEFT) -> 'L'

        else -> '?'
    }

    println("${start} is ${map[start!!]}")

    println(maxDist / 2)

    val edges = maxPath.toSet()
//    println(edges)

    var area = 0
    println("checking area")
    println("${maxX}, $maxY")
    for (y in 0..maxY) {
        var lastCorner: Char? = null
        var inside = false
        for (x in 0..maxX) {
            val pos = Pair(x, y)
            val char = map[pos] ?: ' '

            if (edges.contains(pos)) {
                print(char)
//                print("edge ${pos} ${char} ${inside} (${lastCorner})")
                if (char == '|') {
                    inside = !inside
                } else if (lastCorner == 'L' && char == '7'
                    || lastCorner == 'F' && char == 'J'
                ) {
                    inside = !inside
                    lastCorner = null
                } else if (lastCorner == null && listOf('L', '7', 'F', 'J').contains(char)) {
                    lastCorner = char
                } else if (lastCorner != null && listOf('L', '7', 'F', 'J').contains(char)) {
                    lastCorner = null
                }
            } else {
                val char = when (inside) {
                    true -> 'i'
                    false -> 'o'
                }
                if (inside) {
                    area += 1
                }
                print("${char}")
            }
        }
        println()
    }

    println(area)

//    println(map)
//    println(start)
}
