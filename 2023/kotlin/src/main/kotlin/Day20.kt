// Day20
import util.lcm
import java.io.File


abstract class Module(val name: String, val outputs: List<String> = listOf()) {
    abstract fun processPulse(i: Long, pulse: Pulse): List<Pulse>
}

class DefaultModule(name: String): Module(name, listOf()) {
    override fun processPulse(i: Long, pulse: Pulse): List<Pulse> {
        return listOf()
    }

    override fun toString(): String {
        return "DefaultModule(name=$name)"
    }
}

class Broadcast(outputs: List<String>) : Module("broadcaster", outputs) {
    override fun toString(): String {
        return "Broadcast(outputs=$outputs)"
    }

    override fun processPulse(i: Long, pulse: Pulse): List<Pulse> {
        return outputs.map { Pulse("broadcast", it, pulse.pulse) }
    }

}

class FlipFlop(name: String, outputs: List<String>) : Module(name, outputs) {
    var on = false
    override fun processPulse(i: Long, pulse: Pulse): List<Pulse> {
        if (pulse.pulse == PulseType.HIGH) {
            return listOf()
        }

        on = !on
        val out = when (on) {
            true -> PulseType.HIGH
            false -> PulseType.LOW
        }
        return outputs.map { Pulse(name, it, out) }
    }

    override fun toString(): String {
        return "FlipFlop(name=$name, on=$on, outputs=$outputs)"
    }
}

class Conjunction(name: String, outputs: List<String>) : Module(name, outputs) {
    val inputs = mutableMapOf<String, PulseType>()
    override fun processPulse(i: Long, pulse: Pulse): List<Pulse> {
        inputs[pulse.from] = pulse.pulse

        val out = when (inputs.values.all { it == PulseType.HIGH }) {
            true -> PulseType.LOW
            false -> PulseType.HIGH
        }

        return outputs.map { Pulse(name, it, out) }
    }

    override fun toString(): String {
        return "Conjunction(name=$name, inputs=$inputs, outputs=$outputs)"
    }
}

enum class PulseType {
    HIGH,
    LOW
}

class Pulse(val from: String, val to: String, val pulse: PulseType) {
    override fun toString(): String {
        return "Pulse(from='$from', to='$to', pulse=$pulse)"
    }
}

fun main(args: Array<String>) {
    val input = File("input20.txt").readLines()

    val machine = mutableMapOf<String, Module>()
    input.forEach { line ->
        println(line)
        Regex("""(.+) -> (.+)""").matchEntire(line)?.let { match ->
            val (from, to) = match.destructured
            val outputs = to.split(", ")
            if (from[0] == '%') {
                machine[from.substring(1)] = FlipFlop(from.substring(1), outputs)
            } else if (from[0] == '&') {
                machine[from.substring(1)] = Conjunction(from.substring(1), outputs)
            } else {
                machine[from] = Broadcast(outputs)
            }
        }
    }

    println(machine)

    val allOutputs = machine.values.flatMap { it.outputs }.toSet()
    for (output in allOutputs) {
        machine.getOrPut(output) { DefaultModule(output) }
    }
    for (module in machine.values) {
        module.outputs.forEach { output ->
            if (machine[output]!! is Conjunction) {
                (machine[output]!! as Conjunction).inputs[module.name] = PulseType.LOW
            }
        }
    }

    println(machine)

    val allPulses = mutableListOf<Pulse>()
    for (press in 1L..1000L) {
        println("press=$press")
        var activePulses = mutableListOf(Pulse("button", "broadcaster", PulseType.LOW))
        var i = 0
        while (activePulses.isNotEmpty()) {
            val pulsesSent = mutableListOf<Pulse>()
            for (p in activePulses) {
                allPulses += p
                val out = machine[p.to]!!.processPulse(press, p)
                pulsesSent += out
            }
            println("$i: pulsesSent=$pulsesSent")
            activePulses = pulsesSent
            i++
        }
    }

    var lowCount = 0L
    var highCount = 0L

    allPulses.forEach { pulse ->
        if (pulse.pulse == PulseType.LOW) {
            lowCount++
        } else {
            highCount++
        }
    }

    println("lowCount=$lowCount")
    println("highCount=$highCount")

    println(lowCount * highCount)

    println("part 2")

    // reset
    for (m in machine.values) {
        if (m is FlipFlop) {
            m.on = false
        } else if (m is Conjunction) {
            m.inputs.forEach {
                m.inputs[it.key] = PulseType.LOW
            }
        }
    }
    var press = 0L
    var done = false
    val search = mutableMapOf(
        "fz" to 0L,
        "xf" to 0L,
        "hn" to 0L,
        "mp" to 0L,
    )
    while (!done) {
        press++
        var activePulses = mutableListOf(Pulse("button", "broadcaster", PulseType.LOW))
        var i = 0
        while (activePulses.isNotEmpty()) {
            val pulsesSent = mutableListOf<Pulse>()
            for (p in activePulses) {
//                allPulses += p
                val out = machine[p.to]!!.processPulse(press, p)
                pulsesSent += out

                out.forEach {
                    if (it.to == "xn" && it.pulse == PulseType.HIGH) {
                        println("$press from=${it.from} to=${it.to} press=$press")
                        search[it.from] = press
                    }
                }

                if (search.values.all { it > 0 }) {
                    done = true
                }

                if (out.any { it.to == "rx" && it.pulse == PulseType.LOW }) {
                    done = true
                }
            }
//            println("$i: pulsesSent=$pulsesSent")
            activePulses = pulsesSent
            i++
        }
    }

    println(search)
    println(search.values.fold(1L) { acc, i -> lcm(acc, i) })

    println("press=$press")
}
