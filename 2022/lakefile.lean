import Lake
open Lake DSL

package aoc22

@[default_target]
lean_lib Aoc22 

@[default_target]
lean_exe aoc22 {
  root := `Main
}

require std from git "https://github.com/leanprover/std4" @ "main"
