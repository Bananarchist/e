module Main exposing (main)

import Benchmark.Runner exposing (BenchmarkProgram, program)
import Benchmark 
import Normalize

main : BenchmarkProgram
main =
    program <|
        Benchmark.compare "Normalize"
            "normalize 1"
            (\() -> Normalize.normalize1 181)
            "normalize 2"
            (\() -> Normalize.normalize2 181)

        
