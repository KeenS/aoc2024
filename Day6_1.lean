partial def readAll (input: IO.FS.Stream) (acc: List (List Char)): IO (Array (Array Char)) := do
  let l ← input.getLine
  let l := l.dropRight 1
  if l == ""
  then return acc.reverse.map (List.toArray) |>.toArray
  else readAll input (l.toList ::acc)

inductive Dir: Type where
  | up | right | down | left

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

partial def loop (input: Array (Array Char)) (pos: Nat × Nat) (dir: Dir): Array (Array Char) :=
  let size_x := input.get! 0 |>.size
  let size_y := input.size
  let (x, y) := pos
  let (dx, dy) := dir.diff
  let (nx, ny) := (x + dx, y + dy)
  let line := input.get! y
  let line := line.set! x 'X'
  let input := input.set! y line
  if nx < 0 || size_x <= nx || ny < 0 || size_y <= ny
  then input
  else
    match input.get! ny.toNat |>.get! nx.toNat with
    | '#' => loop input (x, y) dir.next
    | _   => loop input (nx.toNat, ny.toNat) dir

def solve (input: Array (Array Char)): Nat :=
  let start_y := input.findIdx? (fun line => line.contains '^') |>.get!
  let start_x := input.get! start_y |>.getIdx? '^' |>.get!
  loop input (start_x, start_y) Dir.up
    |>.map (·.toList.count ('X'))
    |>.toList
    |>.sum


def main: IO Unit := do
  let stdin ← IO.getStdin
  let list ← readAll stdin []
  let answer := solve list
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
