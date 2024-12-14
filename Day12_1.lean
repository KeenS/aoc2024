import Std.Data.HashMap
import Std.Data.HashSet

partial def readAll (input: IO.FS.Stream) : IO (Array (Array Char)) :=
  let rec loop (acc: List (Array Char)): IO (Array (Array (Char))) := do
    let line ← input.getLine |>.map String.trim
    if line == ""
    then return acc.reverse |> List.toArray
    else loop  (line.toList.toArray :: acc)
  loop []

partial def detectBlock(input: Array (Array Char)) (acc: (Std.HashSet (Int × Int) × Nat)) (pos: Int × Int): (Std.HashSet (Int × Int) × Nat) :=
  let (area, perimeter) := acc
  if area.contains pos
  then acc
  else
    let (x, y) := pos
    let area := area.insert pos
    let e := input.get! y.toNat |>.get! x.toNat
    let diffs := [(-1, 0), (0, -1), (1, 0), (0, 1)]
    let neighbors := diffs.filterMap (λ (dx, dy) => do
      let x₁ ← (x + dx).toNat'
      let y₁ ← (y + dy).toNat'
      let line ← input.get? y₁
      let e₁ ← line.get? x₁
      if e₁ == e then return (x₁, y₁) else .none
    )
    --                                   \circ
    let newPlaces := neighbors.filter (not ∘ area.contains)
    let perimeter := perimeter + 4 - 2 * (neighbors.length - newPlaces.length)
    List.foldl (detectBlock input) (area, perimeter) newPlaces

def calcScore(input: Array (Array Char)) (start: Int × Int): (Std.HashSet (Int × Int) × Nat) :=
  let (area, perimeter) := detectBlock input (Std.HashSet.empty, 0) start
  (area, area.size * perimeter)

def solve (input: Array (Array Char)): Nat :=
  let doneArea := Std.HashSet.empty
  let ys := List.iota input.size |>.map (. - 1)
  let (_, score) := List.foldl (fun acc y =>
    let line := input.get! y
    let xs := List.iota line.size |>.map (. - 1)
    List.foldl (fun (doneArea, score) x =>
      if doneArea.contains (Int.ofNat x, Int.ofNat y)
      then (doneArea, score)
      else let (newArea, newScore) := calcScore input (x, y)
           (doneArea.union newArea, score + newScore)
    ) acc xs
  ) (doneArea, 0) ys
  score

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let answer := solve input
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
