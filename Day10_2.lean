partial def readAllAux (input: IO.FS.Stream) (acc: List (Array Nat)): IO (Array (Array Nat)) := do
  let line ← input.getLine
  let line := line.dropRight 1
  if line == ""
  then return acc.reverse.toArray
  else readAllAux input (line.toList.toArray.map (fun c => c.toString.toNat!) ::acc)

def readAll (input: IO.FS.Stream): IO (Array (Array Nat)) :=
  readAllAux input []

def listTrailHeads(input: Array (Array Nat)): List (Nat × Nat) :=
  input.toList.enum |> List.foldl (fun acc (x, line) =>
    line.toList.enum |> List.foldl (fun acc (y, c) =>
      if c == 0 then (x, y)::acc else acc
    ) acc
  ) []

partial def walk (input: Array (Array Nat)) (pos: List (Nat × Nat)): List (Nat × Nat) :=
  let next := pos.flatMap (fun (x, y) =>
    let c := input.get! x |>.get! y
    if c == 9 then [(c, (x, y))]
    else
      [(0, 1), (1, 0), (0, -1), (-1, 0)].filterMap (fun ((dx, dy): Int × Int) =>
        let (x, y) := ((x + dx).toNat, (y + dy).toNat)
        input.get? x |>.bind (·.get? y) |>.filter (· == (c+1)) |>.map (fun c₁ => (c₁, (x, y)))
      )
  )
  if next.get? 0 |>.map (·.fst == 9) |>.getD true
  then next.map (·.snd)
  else walk input (next.map (·.snd))

def calcScore (input: Array (Array Nat)) (head: Nat × Nat): Nat :=
  walk input [head] |>.length


def solve(input: Array (Array Nat)): Nat :=
  let heads := listTrailHeads input
  heads.map (calcScore input ·)  |>.sum

-- #eval listTrailHeads #[#[0, 1, 2, 3],
--               #[1, 2, 3, 4],
--               #[8, 7, 6, 5],
--               #[9, 8, 7, 6]]

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let answer := solve input
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
