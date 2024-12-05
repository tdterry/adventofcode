// Day09

import java.io.File

fun prev(nums: List<Long>): Long {
    return when (nums.all { n -> n == 0L }) {
        true -> 0
        false -> {
            val deltas = nums.drop(1).mapIndexed { i, n -> n - nums[i] }
            nums[0] - prev(deltas)
        }
    }
}

fun next(nums: List<Long>): Long {
    return when (nums.all { n -> n == 0L }) {
        true -> 0
        false -> {
            val deltas = nums.drop(1).mapIndexed { i, n -> n - nums[i] }
            return nums[nums.size - 1] + next(deltas)
        }
    }
}

fun main(args: Array<String>) {
    val input = File("input09.txt").readLines()
    var totalNext = 0L
    var totalPrev = 0L
    input.forEachIndexed { y, line ->
        val nums = line.split(" ").map { it.trim().toLong() }
        val p = prev(nums)
        val n = next(nums)

        println("$y:$nums: ${p} ${n}")
        totalPrev += p
        totalNext += n
    }

    println("totalNext: $totalNext")
    println("totalPrev: $totalPrev")
}
