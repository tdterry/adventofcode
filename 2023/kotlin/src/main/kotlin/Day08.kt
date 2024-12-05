// Day08

import util.lcm
import java.io.File


fun main(args: Array<String>) {
    val input = File("input08.txt").readLines()

    val instructions = input.first()
    val map = mutableMapOf<String, Pair<String, String>>()
    input.drop(2).forEach {
        Regex("""(\w+) = \((\w+), (\w+)\)""").find(it)?.let { match ->
            map[match.groupValues[1]] = Pair(match.groupValues[2], match.groupValues[3])
//            println("${match.groupValues[1]} -> ${match.groupValues[2]}, ${match.groupValues[3]}")
        }
    }

    println(instructions)
    println(map)

    var current = "AAA"
    var steps = 0L
    var i = 0
    while (current != "ZZZ") {
        current = when (instructions[i]) {
            'L' -> map[current]!!.first
            'R' -> map[current]!!.second
            else -> current
        }
        i = (i + 1) % instructions.length
        steps += 1
    }

    println(steps)

    var startNodes = map.keys.filter { n -> n[n.length - 1] == 'A' }
    println("startingNodes: $startNodes")

    val visited = mutableMapOf<Pair<String, Int>, Long>()
    val stepsList = mutableListOf<Long>()

    for (start in startNodes) {
        var node = start
        i = 0
        steps = 0
        var firstZ: String? = null
        var firstZSteps: Long? = null
        var loopSteps: Long? = null
        while (true) {
            if (node[node.length - 1] == 'Z') {
                firstZ = node
                firstZSteps = steps
                stepsList.add(steps)
            }
            if (visited.contains(Pair(node, i))) {
                println("visited $node, $i, $steps (loop= ${steps - visited[Pair(node, i)]!!})")
                loopSteps = steps - visited[Pair(node, i)]!!
                break
            }

            visited[Pair(node, i)] = steps
            println("$node, $i, $steps")
            node = when (instructions[i]) {
                'L' -> map[node]!!.first
                'R' -> map[node]!!.second
                else -> node
            }
            steps += 1
            i = (i + 1) % instructions.length
        }
    println("firstZ: $firstZ, $firstZSteps")
    println("loopSteps: $loopSteps")
    }
    println("stepsList: $stepsList")
    println(stepsList.fold(1L) { acc, i -> lcm(acc, i) })
}
