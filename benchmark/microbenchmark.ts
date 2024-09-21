#!./node_modules/.bin/sucrase-node
/* eslint-disable no-console */
import { initParser } from "../src/parser/state"

function main(): void {
    const benchmark = process.argv[2] || "all"
    console.log(`Running microbenchmark ${benchmark}`)
    if (benchmark === "all" || benchmark === "hasPredecingLineBreak") {
        const { state } = initParser("let x\nx++;", false, false, false)
        state.next()
        state.next()
        state.next()
        runMicrobenchmark(
            "hasPredecingLineBreak",
            () => {
                state.hasPrecedingLineBreak()
                state.hasPrecedingLineBreak()
                state.hasPrecedingLineBreak()
                state.hasPrecedingLineBreak()
                state.hasPrecedingLineBreak()
            },
            1000000,
        )
    }
}

function runMicrobenchmark(name: string, runTrial: () => void, times: number = 100): void {
    // Run before starting the clock to warm up the JIT, caches, etc.
    for (let i = 0; i < 10; i++) {
        runTrial()
    }
    console.time(name)
    for (let i = 0; i < times; i++) {
        runTrial()
    }
    console.timeEnd(name)
}

main()
