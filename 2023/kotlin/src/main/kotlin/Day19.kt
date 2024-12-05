// Day19

import java.io.File

enum class Operator {
    LT, GT, NOOP
}

class Condition(val variable: String, val op: Operator, val Value: Long, val next: String) {
    override fun toString(): String {
        return "Condition($variable, $op, $Value, $next)"
    }
}

class Rule(val id: String, val conditions: List<Condition>) {
    override fun toString(): String {
        return "Rule($id, $conditions)"
    }

    fun applyTo(part: Part): List<Part> {
        if (part.settled) {
            return listOf(part)
        }

        var tmpPart: Part? = Part(part.ruleId, part.attrs)

        val parts = mutableListOf<Part>()
        conditions.forEach {
            println("applying condition: $it to $tmpPart")
            var newPart: Part? = null
            if (it.op == Operator.NOOP) {
                // nothing left to do
                newPart = Part(it.next, tmpPart!!.attrs)
                tmpPart = null
            } else if (part.attrs.contains(it.variable)) {
                val range = tmpPart!!.attrs[it.variable]!!
                println("range: $range")
                if (it.op == Operator.LT) {
                    // condition is <
                    if (range.max <= it.Value) {
                        // entire range is less than the condition
                        println("range is less than condition")
                        newPart = Part(it.next, tmpPart!!.attrs)
                        tmpPart = null
                    } else if (range.min < it.Value) {
                        // range is partially less than the condition
                        println("range is partially less than condition")
                        var newAttrs = tmpPart!!.attrs.toMutableMap()
                        newAttrs[it.variable] = Range(range.min, it.Value - 1)
                        newPart = Part(it.next, newAttrs)

                        newAttrs = tmpPart!!.attrs.toMutableMap()
                        newAttrs[it.variable] = Range(it.Value, range.max)
                        tmpPart = Part(tmpPart!!.ruleId, newAttrs)
                    } else {
                        // range is greater than the condition, nothing to do
                    }
                } else {
                    // condition is >
                    if (range.min >= it.Value) {
                        // entire range is greater than the condition
                        println("range is greater than condition")
                        newPart = Part(it.next, tmpPart!!.attrs)
                        tmpPart = null
                    } else if (range.max > it.Value) {
                        // range is partially greater than the condition
                        println("range is partially greater than condition")
                        var newAttrs = tmpPart!!.attrs.toMutableMap()
                        newAttrs[it.variable] = Range(it.Value + 1, range.max)
                        newPart = Part(it.next, newAttrs)

                        newAttrs = tmpPart!!.attrs.toMutableMap()
                        newAttrs[it.variable] = Range(range.min, it.Value)
                        tmpPart = Part(tmpPart!!.ruleId, newAttrs)
                    } else {
                        // range is less than the condition, nothing to do
                    }
                }
            } else {
                // keep going
            }

            if (newPart != null) {
                parts.add(newPart)
            }


            if (tmpPart == null) {
                return parts
            }
        }
        return parts
    }
}

class Range(val min: Long, val max: Long) {
    override fun toString(): String {
        return "Range($min, $max)"
    }
}

class Part(val ruleId: String, val attrs: Map<String, Range>) {
    val settled: Boolean = ruleId == "A" || ruleId == "R"
    val accepted: Boolean = ruleId == "A"

    override fun toString(): String {
        return "Part(ruleId=$ruleId, settled=$settled, accepted=$accepted, $attrs)"
    }
}

fun main(args: Array<String>) {
    val input = File("input19.txt").readLines()

    var blankLine = input.indexOf("")
    val rules = mutableMapOf<String, Rule>()

    input.subList(0, blankLine).forEach { line ->
        println(line)
        var rule: Rule? = null
        Regex("""(\w+)\{(.*)\}""").find(line).let { match ->
            val id = match!!.groupValues[1]
            val conditions = match.groupValues[2].split(",").map { cond ->
                var condition: Condition? = null
                if (!cond.contains(":")) {
                    condition = Condition("", Operator.NOOP, 0, cond)
                } else {
                    Regex("""(\w)([<>])(\d+):(\w+)""").find(cond).let { match ->
                        val (variable, op, value, next) = match!!.destructured
                        val operator = when (op) {
                            "<" -> Operator.LT
                            ">" -> Operator.GT
                            else -> Operator.NOOP
                        }
                        condition = Condition(variable, operator, value.toLong(), next)
                    }
                }

//                println(condition)
                condition
            }.filterNotNull()

            rules[id] = Rule(id, conditions)
        }
    }

    println("rules: $rules")

    val parts = input.subList(blankLine + 1, input.size).map(::parsePart)

    println("parts: $parts")

    var total = 0L
    for (part in parts) {
        println("part: $part")
        var id = "in"
        while (id != "R" && id != "A") {
            val rule = rules[id]!!
            println("rule: $rule")
            for (cond in rule.conditions) {
                if (cond.op == Operator.NOOP) {
                    id = cond.next
                    break
                } else if (!part.containsKey(cond.variable)) {
                    continue
                } else {
                    val value = part[cond.variable]!!
                    if (cond.op == Operator.LT && value < cond.Value || cond.op == Operator.GT && value > cond.Value) {
                        id = cond.next
                        break
                    }
                }
            }
            println("goto: $id")
        }

        if (id == "R") {
            println("reject: $part")
        } else {
            println("accept: $part")
            total += part.values.sum()
        }
    }

    println("total: $total")

    // part 2
    println("part 2")
    var parts2 = listOf(
        Part(
            "in",
            mapOf(
                "x" to Range(1, 4000),
                "m" to Range(1, 4000),
                "a" to Range(1, 4000),
                "s" to Range(1, 4000)
            )
        )
    )

    val settledParts = mutableListOf<Part>()
    while (!parts2.isEmpty()) {
//    for (i in 1..10) {
        println("parts: $parts2")

        val newParts = mutableListOf<Part>()
        for (p in parts2) {
            val rule = rules[p.ruleId]!!
            println("   part: $p")
            println("   rule: $rule")

            rule.applyTo(p).forEach {
                println("new part: $it")
                if (it.settled) {
                    settledParts.add(it)
                } else {
                    newParts.add(it)
                }
            }
        }

        println("settled parts: $settledParts")
        println("new parts: $newParts")

        parts2 = newParts
    }

    var total2 = 0L
    println("settled parts:")
    settledParts.forEach {
        println("  $it")
    }

    println("accepted parts:")
    settledParts.filter { it.accepted }.forEach {
        println("  $it")
        total2 += it.attrs.values.map{ it.max - it.min + 1 }.fold(1L) { acc, x -> acc * x }
    }


    println("total2: $total2")

}

fun parsePart(line: String): Map<String, Long> {
    val part = mutableMapOf<String, Long>()
    Regex("""\{(.*)\}""").findAll(line).forEach { match ->
        val attrs = match.groupValues[1].split(",")
        attrs.forEach {
            val (key, value) = it.split("=")
            part[key] = value.toLong()
        }
    }
    return part
}
