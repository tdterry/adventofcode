// Day21
import util.Direction
import java.io.File

fun main(args: Array<String>) {
    val input = File("input21-sample.txt").readLines()

    // part 1
    println("part 1")

    val garden = mutableMapOf<Position, Char>()
    var startPosition = Position(0, 0)
    var sizeX = input[0].length
    var sizeY = input.size
    input.forEachIndexed { j, line ->
        line.forEachIndexed { i, ch ->
            garden[Position(i, j)] = ch
            if (ch == 'S') {
                garden[Position(i, j)] = '.'
                startPosition = Position(i, j)
            }
        }
    }

    val dist = mutableMapOf<Position, Int>()
    dist[startPosition] = 0
    val visited = mutableSetOf<Position>()
    val unvisited = mutableSetOf(startPosition)

    println(garden)
    println("startPosition: $startPosition")
    while (unvisited.isNotEmpty()) {
        val current = unvisited.minBy { dist[it] ?: Int.MAX_VALUE }
        unvisited.remove(current)
        println("current: $current")
        val currentDist = dist[current]!!

        for (dir in Direction.values()) {
            val next = current.move(dir)
            if (visited.contains(next) || garden[next] != '.') {
                continue
            }

            val nextDist = currentDist + 1
            if (nextDist < dist[next] ?: Int.MAX_VALUE) {
                dist[next] = nextDist
            }

            unvisited.add(next)
        }

        visited.add(current)
    }

    println(dist.filter { it.value <= 64 && it.value % 2 == 0 }.size)

// part 2
    println("part 2")
    dist.clear()
    dist[startPosition] = 0
    visited.clear()
    unvisited.clear()
    unvisited.add(startPosition)
    val steps = 1000
    var i = 0
    while (unvisited.isNotEmpty()) {
        val current = unvisited.minBy { dist[it] ?: Int.MAX_VALUE }
        unvisited.remove(current)
        val currentDist = dist[current]!!
        if (currentDist >= steps) {
            visited.add(current)
            continue
        }

        if (currentDist > i) {
            println(i)
            i = currentDist
        }

        for (dir in Direction.entries) {
            val next = current.move(dir)
            val nextDist = currentDist + 1

            if (visited.contains(next) || garden[Position(
                    (next.x % sizeX + sizeX) % sizeX,
                    (next.y % sizeY + sizeY) % sizeY,
                )] != '.') {
                continue
            }

            if (nextDist < (dist[next] ?: Int.MAX_VALUE)) {
                dist[next] = nextDist
            }

                unvisited.add(next)
        }

        visited.add(current)
    }
    println(dist.filter { it.value <= steps && (dist[it.key]!! % 2) == (steps % 2) }.size)
}
