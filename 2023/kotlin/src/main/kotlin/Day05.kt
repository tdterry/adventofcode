// Day05

import java.io.File

class SeedMove(val dest: Long, val src: Long, val len: Long)
class SeedUpdates(val updates: MutableList<SeedRange>, val keeps: MutableList<SeedRange>)

class SeedRange(val start: Long, val len: Long) {
    fun intersect(other: SeedRange): SeedRange? {
        val start = Math.max(this.start, other.start)
        val end = Math.min(this.start + this.len, other.start + other.len)
        if (start < end) {
            return SeedRange(start, end - start)
        }
        return null
    }

    fun update(move: SeedMove): SeedUpdates {
        val intersect = this.intersect(SeedRange(move.src, move.len)) ?: return SeedUpdates(mutableListOf(), mutableListOf(this))

        val updates = SeedUpdates(mutableListOf(), mutableListOf())
        if (intersect.start > this.start) {
            updates.keeps += SeedRange(this.start, intersect.start - this.start)

            if (intersect.start + intersect.len < this.start + this.len) {
                updates.updates += SeedRange(move.dest - move.src + intersect.start, intersect.len)
                updates.keeps += SeedRange(intersect.start + intersect.len, this.start + this.len - intersect.start - intersect.len)
            } else {
                updates.updates += SeedRange(move.dest - move.src + intersect.start, this.start + this.len - intersect.start)
            }
        } else {
            if (intersect.start + intersect.len < this.start + this.len) {
                updates.updates += SeedRange(move.dest - move.src + this.start, intersect.start + intersect.len - this.start)
                updates.keeps += SeedRange(intersect.start + intersect.len, this.start + this.len - intersect.start - intersect.len)
            } else {
                updates.updates += SeedRange(move.dest - move.src + this.start, this.len)
            }
        }

        return updates
    }

    override fun toString(): String {
        return "SeedRange[$start, $len]"
    }
}

fun main(args: Array<String>) {
    val input = File("input05.txt").readLines()

    var seeds: MutableList<Long> = mutableListOf()
    val updateSeeds: MutableList<Long> = mutableListOf()
    var keepSeeds: MutableList<Long> = mutableListOf()

    var seedRanges: MutableList<SeedRange> = mutableListOf()
    val updateRanges: MutableList<SeedRange> = mutableListOf()
    var keepRanges: MutableList<SeedRange> = mutableListOf()

    input.withIndex().forEach { (y, line) ->
        println("$y: $line")

        Regex("seeds: (.*)").find(line)?.let { match ->
            seeds += match.groupValues[1].split(" ").map { it.toLong() }
            println("initial seeds: $seeds")

            for (i in 0..< seeds.size/2) {
                seedRanges += SeedRange(seeds[i*2], seeds[i*2+1])
            }
        }

        Regex("""^(\d+) (\d+) (\d+)$""").find(line)?.let { match ->
            val dest = match.groupValues[1].toLong()
            val src = match.groupValues[2].toLong()
            val range = match.groupValues[3].toLong()

            for (s in seeds) {
                if (s >= src && s < src + range) {
                    updateSeeds += dest + (s - src)
                } else {
                    keepSeeds += s
                }
            }

            for (s in seedRanges) {
                val update = s.update(SeedMove(dest, src, range))
                updateRanges += update.updates
                keepRanges += update.keeps
            }

            println("updateSeeds: $updateSeeds")
            println("keepSeeds: $keepSeeds")

            seeds = keepSeeds
            keepSeeds = mutableListOf()

            println("updateRanges: $updateRanges")
            println("keepRanges: $keepRanges")

            seedRanges = keepRanges
            keepRanges = mutableListOf()
        }

        Regex(".* map:").find(line)?.let { match ->
            seeds += updateSeeds
            updateSeeds.clear()

            seedRanges += updateRanges
            updateRanges.clear()

            println("seeds: $seeds")
            println("seedRanges: $seedRanges")
        }
    }

    seeds += updateSeeds
    seedRanges += updateRanges
    println("seeds: $seeds")
    println("seedRanges: $seedRanges")

    println(seeds.min())
    println((seedRanges.map { it.start }).min())
}
