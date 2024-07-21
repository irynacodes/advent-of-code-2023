package org.example

import java.io.File

/**
 * @author Iryna Lazouskaya
 */

var INSTRUCTIONS: String = ""
val JUMPS: MutableMap<String, Pair<String, String>> = HashMap()

val STARTING_NODE = "AAA"
val ENDING_NODE = "ZZZ"
val OPERATIONALIZATIONS: Map<Char, (Pair<String, String>) -> String> = mapOf(
    'L' to { it.first },
    'R' to { it.second }
)

fun main() {
    processInput(readInput("input.txt"))
    println(followInstructions(STARTING_NODE, 0))
}

fun followInstructions(startingNode: String, steps: Int): Int {
    var currentNode = startingNode
    var stepCount = steps

    for (instruction in INSTRUCTIONS) {
        // as pointed out in the root README, i rely on the input file being of the specified structure
        // that's what excuses the use of the !! operator below (otherwise this would really smell)
        currentNode = OPERATIONALIZATIONS[instruction]!!.invoke(JUMPS[currentNode]!!)
        stepCount++
    }

    if (currentNode != ENDING_NODE)
        return followInstructions(currentNode, stepCount)

    return stepCount
}

fun processInput(input: String) {
    INSTRUCTIONS = input.split("\n\n")[0]
    val jumps = input.split("\n\n")[1].trim().split("\n")

    val regex = Regex("""([A-Z]+)\s=\s\(([A-Z]+),\s([A-Z]+)\)""")
    for (jump in jumps) {
        val (source, destl, destr) = regex.find(jump)!!.destructured
        JUMPS[source] = Pair(destl, destr)
    }
}

fun readInput(fileName: String): String =
    File(fileName).readText()
