// Day16
import util.Direction
import java.io.File

class Cell(val ch: Char, var dirs: MutableSet<Direction> = mutableSetOf())

class Beam(val x: Int, val y: Int, val dir: Direction)

fun calcEnergized(map: Map<Pair<Int, Int>, Cell>, x: Int, y: Int, dir: Direction): Int {
    map.values.forEach { it.dirs.clear() }
    val beams = mutableListOf(Beam(x, y, dir))
    while (beams.isNotEmpty()) {
        val beam = beams.removeAt(0)

        if (!map.contains(Pair(beam.x, beam.y))) {
            // beam went off the map
            continue
        }

        val cell = map[Pair(beam.x, beam.y)]!!

        if (cell.dirs.contains(beam.dir)) {
            // beam already went through this cell in this direction
            continue
        }

        cell.dirs.add(beam.dir)
        if (cell.ch == '.') {
            val next = when (beam.dir) {
                Direction.UP -> Beam(beam.x, beam.y - 1, beam.dir)
                Direction.DOWN -> Beam(beam.x, beam.y + 1, beam.dir)
                Direction.LEFT -> Beam(beam.x - 1, beam.y, beam.dir)
                Direction.RIGHT -> Beam(beam.x + 1, beam.y, beam.dir)
            }
            beams.add(next)
        } else if (cell.ch == '\\') {
            val next = when (beam.dir) {
                Direction.UP -> Beam(beam.x - 1, beam.y, Direction.LEFT)
                Direction.DOWN -> Beam(beam.x + 1, beam.y, Direction.RIGHT)
                Direction.LEFT -> Beam(beam.x, beam.y - 1, Direction.UP)
                Direction.RIGHT -> Beam(beam.x, beam.y + 1, Direction.DOWN)
            }
            beams.add(next)
        } else if (cell.ch == '/') {
            val next = when (beam.dir) {
                Direction.UP -> Beam(beam.x + 1, beam.y, Direction.RIGHT)
                Direction.DOWN -> Beam(beam.x - 1, beam.y, Direction.LEFT)
                Direction.LEFT -> Beam(beam.x, beam.y + 1, Direction.DOWN)
                Direction.RIGHT -> Beam(beam.x, beam.y - 1, Direction.UP)
            }
            beams.add(next)
        } else if (cell.ch == '|') {
            val nexts = when (beam.dir) {
                Direction.UP -> listOf(Beam(beam.x, beam.y - 1, beam.dir))
                Direction.DOWN -> listOf(Beam(beam.x, beam.y + 1, beam.dir))
                Direction.LEFT -> listOf(Beam(beam.x, beam.y-1, Direction.UP), Beam(beam.x, beam.y+1, Direction.DOWN))
                Direction.RIGHT -> listOf(Beam(beam.x, beam.y-1, Direction.UP), Beam(beam.x, beam.y+1, Direction.DOWN))
            }
            beams.addAll(nexts)
        } else if (cell.ch == '-') {
            val nexts = when (beam.dir) {
                Direction.UP -> listOf(Beam(beam.x - 1, beam.y, Direction.LEFT), Beam(beam.x + 1, beam.y, Direction.RIGHT))
                Direction.DOWN -> listOf(Beam(beam.x - 1, beam.y, Direction.LEFT), Beam(beam.x + 1, beam.y, Direction.RIGHT))
                Direction.LEFT -> listOf(Beam(beam.x-1, beam.y, beam.dir))
                Direction.RIGHT -> listOf(Beam(beam.x+1, beam.y, beam.dir))
            }
            beams.addAll(nexts)
        }
    }

    return map.values.filter { it.dirs.isNotEmpty() }.size

}

fun main(args: Array<String>) {
    val input = File("input16.txt").readLines()

    // part 1
    println("part 1")

    val map = mutableMapOf<Pair<Int, Int>, Cell>()
    input.forEachIndexed { j, line ->
        line.forEachIndexed { i, ch ->
            map[Pair(i, j)] = Cell(ch)
        }
    }

    println("Energized: ${calcEnergized(map, 0, 0, Direction.RIGHT)}")

    // part 2
    println("part 2")

    var maxEnergized = 0
    for (y in 0..<input.size) {
        val energized = calcEnergized(map, 0, y, Direction.RIGHT)
        if (energized > maxEnergized) {
            maxEnergized = energized
        }

        val energized2 = calcEnergized(map, input[0].length - 1, y, Direction.LEFT)
        if (energized2 > maxEnergized) {
            maxEnergized = energized2
        }
    }

    for (x in 0..<input[0].length) {
        val energized = calcEnergized(map, x, 0, Direction.DOWN)
        if (energized > maxEnergized) {
            maxEnergized = energized
        }

        val energized2 = calcEnergized(map, x, input.size - 1, Direction.UP)
        if (energized2 > maxEnergized) {
            maxEnergized = energized2
        }
    }

    println("Max energized: ${maxEnergized}")
}
