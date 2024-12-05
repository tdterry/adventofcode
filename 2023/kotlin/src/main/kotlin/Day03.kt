import java.io.File

fun main(args: Array<String>) {
    val input = File("input03.txt").readLines()
    val board: MutableMap<Pair<Int, Int>, Char> = mutableMapOf()
    var width = 0
    var height = 0

    var gearPairs: MutableMap<Pair<Int, Int>, List<Int>> = mutableMapOf()
    input.withIndex().forEach { (y, line) ->
        height = y
        line.withIndex().forEach { (x, char) ->
            board[Pair(x, y)] = char
            width = x
        }
    }

    for (y in 0..height) {
        board[Pair(-1, y)] = '.'
        board[Pair(width + 1, y)] = '.'
    }
    for (x in 0..width) {
        board[Pair(x, -1)] = '.'
        board[Pair(x, height + 1)] = '.'
    }
    board[Pair(-1, -1)] = '.'
    board[Pair(width + 1, -1)] = '.'
    board[Pair(-1, height + 1)] = '.'
    board[Pair(width + 1, height + 1)] = '.'

    for (y in 0..height + 1) {
        for (x in 0..width + 1) {
            print(board[Pair(x, y)])
        }
        println()
    }


    var inNum = false
    var startX = 0
    var endX: Int
    var num = 0
    var total = 0
    for (y in 0..height) {
        for (x in 0..width + 1) {
            val char = board.getOrDefault(Pair(x, y), '.')
            val isDigit = "0123456789".contains(char)
            if (!inNum && isDigit) {
                num = 0
                inNum = true;
                startX = x;
            }

            if (inNum && isDigit) {
                num = num * 10 + char.digitToInt()
            } else if (inNum && !isDigit) {
                inNum = false;
                endX = x - 1;
                print("found $startX..$endX,$y num=$num ")

                var touchesSymbol = false;
                for (j in listOf(y - 1, y + 1)) {
                    for (i in startX - 1..endX + 1) {
                        if (board[Pair(i, j)] != '.') {
                            touchesSymbol = true;
                        }

                        if (board[Pair(i, j)] == '*') {
                            gearPairs[Pair(i, j)] = gearPairs.getOrDefault(Pair(i, j), listOf()) + listOf(num)
                        }
                    }
                }

                if (board[Pair(startX - 1, y)] != '.' || board[Pair(endX + 1, y)] != '.') {
                    touchesSymbol = true;
                }

                if (board[Pair(startX - 1, y)] == '*') {
                    gearPairs[Pair(startX - 1, y)] = gearPairs.getOrDefault(Pair(startX - 1, y), listOf()) + listOf(num)
                }

                if (board[Pair(endX + 1, y)] == '*') {
                    gearPairs[Pair(endX+1, y)] = gearPairs.getOrDefault(Pair(endX+1, y), listOf()) + listOf(num)
                }

                println("touchesSymbol=$touchesSymbol")

                if (touchesSymbol) {
                    total += num
                }
            }

        }
    }
    println(total)

    println(gearPairs.values.filter { it.size == 2 }.map { it[0] * it[1] }.sum())
}
