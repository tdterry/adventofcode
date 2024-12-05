// Day13
import java.io.File
import kotlin.math.min

fun stringDelta(a: String, b: String): Int {
    if (a.length != b.length) {
        throw Exception("length mismatch")
    }

    var nErrors = 0
    for (i in 0 until a.length) {
        if (a[i] != b[i]) {
            nErrors++
        }

    }

    return nErrors
}

fun main(args: Array<String>) {
    val input = File("input13.txt").readText().trim().split("\n\n")

    // part 1
    println("part 1")

    var rowReflection = 0
    var colReflection = 0
    input.forEachIndexed { g, grid ->
//        println(g)
//        println(grid)
//        println("---")

        val rows = grid.split("\n")
//        println("rows")
//        rows.forEach(::println)
//        println("---")

        val cols = mutableMapOf<Int, String>()
        for (i in 0 until rows[0].length) {
            for (j in 0 until rows.size) {
                cols[i] = cols.getOrDefault(i, "") + rows[j][i]
            }
        }

//        println("cols")
//        cols.forEach(::println)
//        println("---")

        for (i in 0..<rows.size - 1) {
            var rowMatch = true
            for (j in 0..min(i, rows.size - i - 2)) {
                if (rows[i - j] != rows[i + j + 1]) {
                    rowMatch = false
                }
            }

            if (rowMatch) {
                rowReflection += i+1
            }
        }

        for (i in 0..<cols.size - 1) {
            var colMatch = true
            for (j in 0..min(i, cols.size - i - 2)) {
                if (cols[i - j] != cols[i + j + 1]) {
                    colMatch = false
                }
            }

            if (colMatch) {
                colReflection += i+1
            }
        }

    }
        println("rowReflection: $rowReflection")
        println("colReflection: $colReflection")
        println(rowReflection*100 + colReflection)

    // part 2
    println("part 2")

    rowReflection = 0
    colReflection = 0
    input.forEachIndexed { g, grid ->
//        println(g)
//        println(grid)
//        println("---")

        val rows = grid.split("\n")
//        println("rows")
//        rows.forEach(::println)
//        println("---")

        val cols = mutableMapOf<Int, String>()
        for (i in 0 until rows[0].length) {
            for (j in 0 until rows.size) {
                cols[i] = cols.getOrDefault(i, "") + rows[j][i]
            }
        }

//        println("cols")
//        cols.forEach(::println)
//        println("---")

        for (i in 0..<rows.size - 1) {
            var nErrors = 0
            for (j in 0..min(i, rows.size - i - 2)) {
                nErrors += stringDelta(rows[i - j], rows[i + j + 1])
            }

            if (nErrors == 1) {
                rowReflection += i+1
            }
        }

        for (i in 0..<cols.size - 1) {
            var nErrors = 0
            for (j in 0..min(i, cols.size - i - 2)) {
                nErrors += stringDelta(cols[i - j]!!, cols[i + j + 1]!!)
            }

            if (nErrors == 1) {
                colReflection += i+1
            }
        }

    }
    println("rowReflection: $rowReflection")
    println("colReflection: $colReflection")
    println(rowReflection * 100 + colReflection)
}
