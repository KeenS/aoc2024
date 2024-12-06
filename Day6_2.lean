partial def readAll (input: IO.FS.Stream) (acc: List (List Char)): IO (Array (Array Char)) := do
  let l ← input.getLine
  let l := l.dropRight 1
  if l == ""
  then return acc.reverse.map (List.toArray) |>.toArray
  else readAll input (l.toList ::acc)

inductive Dir: Type where
  | up | right | down | left
deriving Repr, BEq

def Dir.diff: Dir -> (Int × Int)
  | up => (0, -1)
  | right => (1, 0)
  | down => (0, 1)
  | left => (-1, 0)

def Dir.next: Dir -> Dir
  | up => right
  | right => down
  | down => left
  | left => up

partial def loop (input: Array (Array Char)) (pos1: Nat × Nat × Dir) (pos2: Nat × Nat × Dir): Bool :=
  let size_x := input.get! 0 |>.size
  let size_y := input.size
  let rec next (pos: Nat × Nat × Dir): Option (Nat × Nat × Dir) :=
    let (x, y, dir) := pos
    let (dx, dy) := dir.diff
    let (nx, ny) := (x + dx, y + dy)
    if nx < 0 || size_x <= nx || ny < 0 || size_y <= ny
    then Option.none
    else
      let nx := nx.toNat
      let ny := ny.toNat
      match input.get! ny |>.get! nx with
      | '#' => Option.some ( x,  y, dir.next)
      |  _  => Option.some (nx, ny, dir)
  match (next pos1, next pos2 |>.bind next) with
  | (.some pos1, .some pos2) => if pos1 == pos2 then true else loop input pos1 pos2
  | _                        => false

def solve (input: Array (Array Char)): Nat :=
  let start_y := input.findIdx? (fun line => line.contains '^') |>.get!
  let start_x := input.get! start_y |>.getIdx? '^' |>.get!
  let start := (start_x, start_y, Dir.up)
  let size_x := input.get! 0 |>.size
  let size_y := input.size
  List.iota size_y |>.map (· - 1) |>.map (fun y =>
    let line := input.get! y
    List.iota size_x |>.map (· - 1) |>.map (fun x =>
      let line := line.set! x '#'
      let input := input.set! y line
      loop input start start
    )
  ) |>.flatten |> List.count true


def main: IO Unit := do
  let stdin ← IO.getStdin
  let list ← readAll stdin []
  let answer := solve list
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
