// Day15
import java.io.File

class Lens(val name: String, val value: Int) {
    override fun toString(): String {
        return "[$name $value]"
    }
}

fun hash(s: String): Int {
    var h = 0
    for (c in s) {
        h = ((h+c.code) * 17) % 256
    }
    return h
}
fun main(args: Array<String>) {
    val input = File("input15.txt").readLines()

    // part 1
    println("part 1")

    var total = 0L
    input.forEach { line ->
        line.split(",").forEach {
            println("$it: ${hash(it)}")
            total += hash(it)
        }
    }

    println(total)

    // part 2
    println("part 2")

    val boxes = arrayOfNulls<MutableList<Lens>>(256)
    input[0].split(",").forEach {
        Regex("""(\w+)=(\d+)""").find(it)?.let {
            val (name, value) = it.destructured
            val h = hash(name)
            if (boxes[h] == null) {
                boxes[h] = mutableListOf(Lens(name, value.toInt()))
            } else {
                var found = false
                boxes[h]!!.forEachIndexed { i, lens ->
                    if (lens.name == name) {
                        boxes[h]!![i] = Lens(name, value.toInt())
                        found = true
                    }
                }
                if (!found) {
                    boxes[h]!!.add(Lens(name, value.toInt()))
                }
            }
        }

        Regex("""(\w+)-""").find(it)?.let {
            val (name) = it.destructured
            val h = hash(name)
            boxes[h] = boxes[h]?.filter { it.name != name }?.toMutableList()
        }
    }

    val power = boxes.filterNotNull().mapIndexed() { i, box ->
        box.mapIndexed { j, lens ->
            lens.value * (j + 1)
        }.sum() * (i + 1)
    }.sum()

    println(power)
}
