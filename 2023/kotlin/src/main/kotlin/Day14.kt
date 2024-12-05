// Day14
import java.io.File

class Dish(val cycle: Long, val map: Map<Pair<Int, Int>, Char>, val maxX: Int, val maxY: Int) {
    override fun toString(): String {
        val sb = StringBuilder()
        for (y in 0..<maxY) {
            for (x in 0..<maxX) {
                sb.append(map[Pair(x, y)])
            }
            sb.append("\n")
        }
        return sb.toString()
    }

    fun doCycle(): Dish {
        var m = rollNorth(map, maxX, maxY)
        m = rollWest(m, maxX, maxY)
        m = rollSouth(m, maxX, maxY)
        m = rollEast(m, maxX, maxY)

        return Dish(cycle + 1, m, maxX, maxY)
    }

    override fun equals(other: Any?): Boolean {
        if (other is Dish) {
            return map == other.map
        }
        return false
    }

    override fun hashCode(): Int {
        return map.hashCode()
    }
}

fun rollNorth(map: Map<Pair<Int, Int>, Char>, maxX: Int, maxY: Int): Map<Pair<Int, Int>, Char> {
    val newMap = mutableMapOf<Pair<Int, Int>, Char>()
    for (y in 0..<maxY) {
        for (x in 0..<maxX) {
            newMap[Pair(x, y)] = map[Pair(x, y)]!!
            if (newMap[Pair(x, y)] == 'O') {
                var k = y - 1
                while (k >= 0 && newMap[Pair(x, k)] == '.') {
                    newMap[Pair(x, k)] = 'O'
                    newMap[Pair(x, k + 1)] = '.'
                    k -= 1
                }
            }
        }
    }
    return newMap
}

fun rollWest(map: Map<Pair<Int, Int>, Char>, maxX: Int, maxY: Int): Map<Pair<Int, Int>, Char> {
    val newMap = mutableMapOf<Pair<Int, Int>, Char>()
    for (y in 0..<maxY) {
        for (x in 0..<maxX) {
            newMap[Pair(x, y)] = map[Pair(x, y)]!!
            if (newMap[Pair(x, y)] == 'O') {
                var k = x - 1
                while (k >= 0 && newMap[Pair(k, y)] == '.') {
                    newMap[Pair(k, y)] = 'O'
                    newMap[Pair(k + 1, y)] = '.'
                    k -= 1
                }
            }
        }
    }
    return newMap
}

fun rollSouth(map: Map<Pair<Int, Int>, Char>, maxX: Int, maxY: Int): Map<Pair<Int, Int>, Char> {
    val newMap = mutableMapOf<Pair<Int, Int>, Char>()
    for (y in maxY - 1 downTo 0) {
        for (x in 0..<maxX) {
            newMap[Pair(x, y)] = map[Pair(x, y)]!!
            if (newMap[Pair(x, y)] == 'O') {
                var k = y + 1
                while (k < maxY && newMap[Pair(x, k)] == '.') {
                    newMap[Pair(x, k)] = 'O'
                    newMap[Pair(x, k - 1)] = '.'
                    k += 1
                }
            }
        }
    }
    return newMap

}

fun rollEast(map: Map<Pair<Int, Int>, Char>, maxX: Int, maxY: Int): Map<Pair<Int, Int>, Char> {
    val newMap = mutableMapOf<Pair<Int, Int>, Char>()
    for (y in 0..<maxY) {
        for (x in maxX - 1 downTo 0) {
            newMap[Pair(x, y)] = map[Pair(x, y)]!!
            if (newMap[Pair(x, y)] == 'O') {
                var k = x + 1
                while (k < maxX && newMap[Pair(k, y)] == '.') {
                    newMap[Pair(k, y)] = 'O'
                    newMap[Pair(k - 1, y)] = '.'
                    k += 1
                }
            }
        }
    }
    return newMap

}

fun mapEqual(map1: Map<Pair<Int, Int>, Char>, map2: Map<Pair<Int, Int>, Char>): Boolean {
    for (pair in map1.keys) {
        if (map1[pair] != map2[pair]) {
            return false
        }
    }
    return true
}


fun main(args: Array<String>) {
    val input = File("input14.txt").readLines()

    // part 1
    println("part 1")

    val map = mutableMapOf<Pair<Int, Int>, Char>()
    var maxX = input[0].length
    var maxY = input.size
    for (j in 0..<input.size) {
        for (i in 0..<input[j].length) {
            print(input[j][i])
            map[Pair(i, j)] = input[j][i]

        }
        println()
    }

    val newMap = rollNorth(map, maxX, maxY)
    println("weight: ${weight(newMap, maxX, maxY)}")

    var dish = Dish(0, map, maxX, maxY)
    val dishes = mutableSetOf(dish)

    dish = dish.doCycle()
    println("cycle: ${dish.cycle} weight:${weight(dish.map, maxX, maxY)}")
    if (dishes.contains(dish)) {
        println("found cycle")
        printMap(dish.map, maxX, maxY)
        for (d in dishes) {
            if (d == dish) {
                println("found match at ${d.cycle}")
                printMap(d.map, maxX, maxY)
            }
        }
    } else {
        dishes.add(dish)
    }

    var done = false
    while (!done) {
        dish = dish.doCycle()

        println("cycle: ${dish.cycle} weight:${weight(dish.map, maxX, maxY)}")
        if (dishes.contains(dish)) {
            println("found cycle")
//            printMap(dish.map, maxX, maxY)
            for (d in dishes) {
                if (d == dish) {
                    println("found match at ${d.cycle}")
//                    printMap(d.map, maxX, maxY)

                    println("cycle diff: ${dish.cycle - d.cycle}")
                    val remainder = (1000000000L - dish.cycle) % (dish.cycle - d.cycle)
                    println("remainder: ${remainder}")
                    for (d2 in dishes) {
                        if (d2.cycle == d.cycle + remainder) {
                            println("1B is at ${d2.cycle}")
                            println("weight: ${weight(d2.map, maxX, maxY)}")
                            done = true
                        }
                    }
                }
            }
        } else {
            dishes.add(dish)
        }
    }
}

fun weight(map: Map<Pair<Int, Int>, Char>, maxX: Int, maxY: Int): Int {
    var weight = 0
    for (j in 0..<maxY) {
        for (i in 0..<maxX) {
            if (map[Pair(i, j)] == 'O') {
                weight += maxY - j
            }
        }
    }
    return weight
}

fun printMap(map: Map<Pair<Int, Int>, Char>, maxX: Int, maxY: Int) {
    for (j in 0..<maxY) {
        for (i in 0..<maxX) {
            print(map[Pair(i, j)])
        }
        println()
    }
}
