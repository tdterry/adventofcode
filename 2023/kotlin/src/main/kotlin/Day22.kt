// Day22
import java.io.File
import kotlin.math.max
import kotlin.math.min

data class Block(val n: Int, val x1: Int, val y1: Int, val z1: Int, val x2: Int, val y2: Int, val z2: Int) {
    fun contains(x: Int, y: Int, z: Int): Boolean {
        return x in x1..x2 && y in y1..y2 && z in z1..z2
    }

    fun below(other: Block): Boolean {
        return z2 < other.z1 && (min(x2, other.x2) >= max(x1, other.x1) && min(y2, other.y2) >= max(y1, other.y1))
    }

    fun supports(other: Block): Boolean {
        return below(other) && z2 + 1 == other.z1
    }

    fun moveTo(z: Int): Block {
        return Block(n, x1, y1, z, x2, y2, z + z2-z1)
    }

}

fun main(args: Array<String>) {
    val input = File("input22.txt").readLines()

    // part 1
    println("part 1")

    var blocks = mutableListOf<Block>()
    var n = 0
    input.forEachIndexed { j, line ->
        Regex("""(\d+),(\d+),(\d+)~(\d+),(\d+),(\d+)""").find(line)?.let {
            val (x1, y1, z1, x2, y2, z2) = it.destructured
            blocks.add(Block(n, x1.toInt(), y1.toInt(), z1.toInt(), x2.toInt(), y2.toInt(), z2.toInt()))
            n++
        }
    }

    for (block in blocks) {
        println(block)
    }

    blocks.sortBy { it.z1 }
    val below = mutableMapOf<Block, MutableList<Block>>()
    for (i in 0 until blocks.size) {
        for (j in i + 1 until blocks.size) {
            if (blocks[i].below(blocks[j])) {
                println("block ${blocks[i].n} below ${blocks[j].n}")
                below.getOrPut(blocks[j]) { mutableListOf() }.add(blocks[i])
            }
        }
    }

    // move down
    val newBlocks = mutableMapOf<Int, Block>()
    for (b in blocks) {
        val blocksBelow = below.getOrElse(b) { mutableListOf() }

        println("moving down $b: blocks below ${blocksBelow.map{it.n}}")
        val zBelow = blocksBelow.fold(0) { acc, block -> max(acc, newBlocks[block.n]!!.z2) } + 1
        println("moving $b to $zBelow")
        newBlocks[b.n] = b.moveTo(zBelow)

    }

    blocks = newBlocks.values.toMutableList().sortedBy { it.z1 }.toMutableList()
    val supports = mutableMapOf<Int, MutableList<Block>>()
    val supportedBy = mutableMapOf<Int, MutableList<Block>>()
    for (i in 0 until blocks.size) {
        supports[blocks[i].n] = mutableListOf<Block>()
        for (j in i + 1 until blocks.size) {
            if (blocks[i].supports(blocks[j])) {
                println("block ${blocks[i].n} supports ${blocks[j].n}")
                supports.getOrPut(blocks[i].n) { mutableListOf() }.add(blocks[j])
                supportedBy.getOrPut(blocks[j].n) { mutableListOf() }.add(blocks[i])
            }
        }
    }


    for (newBlock in newBlocks) {
        println(newBlock)
    }

    supports.forEach { (t, u) ->
        println("$t supports $u")
    }

    val canRemove = mutableListOf<Int>()
    val cantRemove = mutableListOf<Int>()
    for ((k, v) in supports) {
        if (v.all { supportedBy[it.n]!!.size != 1 }) {
            println("can remove $k")
            canRemove.add(k)
        } else {
            println("can't remove $k")
            cantRemove.add(k)
        }
    }

    println(canRemove)
    println(canRemove.size)

    // part 2
    println("part 2")

    var total = 0
    for (b in cantRemove) {
        println("removing $b")
        val removed = mutableSetOf(b)
        val toProcess = mutableListOf(b)
        while (toProcess.isNotEmpty()) {
            val v = toProcess.removeAt(0)
            supports[v]!!.forEach {
                if (supportedBy[it.n]!!.all { removed.contains(it.n) }) {
                    removed.add(it.n)
                    toProcess.add(it.n)
                }
            }
        }
        println("removing $b causes ${removed.size-1} blocks to fall")
        total += removed.size-1
    }

    println(total)
}
