// Day17
import util.Direction
import java.io.File

data class Position(val x: Int, val y: Int) {
    override fun toString(): String {
        return "($x, $y)"
    }

    fun move(dir: Direction): Position {
        return when (dir) {
            Direction.UP -> Position(x, y - 1)
            Direction.DOWN -> Position(x, y + 1)
            Direction.RIGHT -> Position(x + 1, y)
            Direction.LEFT -> Position(x - 1, y)
        }
    }

    fun back(dir: Direction): Position {
        return when (dir) {
            Direction.UP -> Position(x, y + 1)
            Direction.DOWN -> Position(x, y - 1)
            Direction.RIGHT -> Position(x - 1, y)
            Direction.LEFT -> Position(x + 1, y)
        }
    }
}

data class Node(val pos: Position, val dir: Direction, val rep: Int)

fun part1(map: Map<Position, Int>, sizeX: Int, sizeY: Int) {
    // part 1
    println("part 1")

    var unvisited = mutableSetOf(
        Node(Position(0,0), Direction.RIGHT, 0),
    )

    val best = mutableMapOf<Node, Int>()
    unvisited.forEach {
        best[it] = 0
    }

    val visited = mutableSetOf<Node>()
//    println("unvisited: $unvisited")
    while (unvisited.isNotEmpty()) {
//        println("visited: ${visited.size}")
        println("unvisited: ${unvisited.size}")
        val current = unvisited.minBy { best[it] ?: Int.MAX_VALUE }
        unvisited.remove(current)

//        println("current: $current, best: ${best[current]}")
        for (dir in Direction.entries.filter { it != current.dir.opposite() }) {
            val neighbor = Node(current.pos.move(dir), dir, if (dir == current.dir) current.rep + 1 else 1)
//            println("maybe neighbor: $neighbor ")
            if (!map.contains(neighbor.pos) || visited.contains(neighbor) || neighbor.rep > 3) {
                continue
            }

            if (best[current]!! + map[neighbor.pos]!! < (best[neighbor] ?: Int.MAX_VALUE)) {
                best[neighbor] = best[current]!! + map[neighbor.pos]!!
            }

//            println("neighbor: $neighbor best: ${best[neighbor]}")

            unvisited.add(neighbor)
        }

        visited.add(current)
    }



    best.filter { it.key.pos == Position(sizeX - 1, sizeY - 1) && best[it.key]!! < Int.MAX_VALUE }
        .forEach(::println)

    println(best.filter { it.key.pos == Position(sizeX - 1, sizeY - 1) }.map { best[it.key] ?: Int.MIN_VALUE }.min())


}

fun part2(map: Map<Position, Int>, sizeX: Int, sizeY: Int) {
    // part 2
    println("part 2")

    var unvisited = mutableSetOf(
        Node(Position(0, 0), Direction.RIGHT, 0),
    )

    val best = mutableMapOf<Node, Int>()
    unvisited.forEach {
        best[it] = 0
    }

    val visited = mutableSetOf<Node>()
    while (unvisited.isNotEmpty()) {
//        println("visited: ${visited.size}")
        println("unvisited: ${unvisited.size}")
        val current = unvisited.minBy { best[it] ?: Int.MAX_VALUE }
        unvisited.remove(current)

//        println("current: $current, best: ${best[current]}")
        for (dir in Direction.entries.filter { it != current.dir.opposite() }) {
            val steps = if (dir == current.dir && current.rep != 0) 1 else 4
            var weight = 0
            var pos = current.pos

            var ok = true
            for (i in 1..steps) {
                pos = pos.move(dir)
                if (!map.contains(pos)) {
                    ok = false
                    break
                }
                weight += map[pos]!!
            }

            if(!ok) continue

            val neighbor = Node(pos, dir, if (dir == current.dir && current.rep != 0) current.rep + 1 else 4)
//            println("maybe neighbor: $neighbor ")
            if (visited.contains(neighbor) || neighbor.rep > 10) {
                continue
            }

            if (best[current]!! + weight < (best[neighbor] ?: Int.MAX_VALUE)) {
                best[neighbor] = best[current]!! + weight
            }

//            println("neighbor: $neighbor best: ${best[neighbor]}")

            unvisited.add(neighbor)
        }

        visited.add(current)
    }
    println(best.filter { it.key.pos == Position(sizeX - 1, sizeY - 1) }.map { best[it.key] ?: Int.MIN_VALUE }.min())


}

fun main(args: Array<String>) {
    val input = File("input17.txt").readLines()

    val map = mutableMapOf<Position, Int>()
    val sizeX = input[0].length
    val sizeY = input.size
    input.forEachIndexed { j, line ->
        line.forEachIndexed { i, ch ->
            map[Position(i, j)] = ch.digitToInt()
        }
    }

//    part1(map, sizeX, sizeY)
    part2(map, sizeX, sizeY)

}
