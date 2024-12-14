import Std.Data.HashMap
import Std.Data.HashSet

partial def readAll (input: IO.FS.Stream) : IO (Array (Array Char)) :=
  let rec loop (acc: List (Array Char)): IO (Array (Array (Char))) := do
    let line ← input.getLine |>.map String.trim
    if line == ""
    then return acc.reverse |> List.toArray
    else loop  (line.toList.toArray :: acc)
  loop []

def countCornersAt(area: Std.HashSet (Int × Int)) (pos: Int × Int): Int :=
  let (x, y) := pos
  -- a c
  --  + <--- pos
  -- b d
  let a := area.contains (x-1, y-1) |>.toNat
  let b := area.contains (x-1, y  ) |>.toNat
  let c := area.contains (x  , y-1) |>.toNat
  let d := area.contains (x  , y  ) |>.toNat
  -- XX
  -- XX
  if a + b + c + d == 4  || a + b + c + d == 0 then 0
  -- XX
  -- XO
  else if a + b + c + d == 3 || a + b + c + d == 1 then 1
  -- XX
  -- OO
  else if (a == c && b == d) || (a == b && c == d) then 0
  -- OX
  -- XO
  else 2

partial def detectBlock(input: Array (Array Char)) (acc: (Std.HashSet (Int × Int) × Int)) (pos: Int × Int): (Std.HashSet (Int × Int) × Int) :=
  let (area, sides) := acc
  if area.contains pos
  then acc
  else
    let (x, y) := pos
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
    let fourPoints := [(0, 0), (0, 1), (1, 0), (1, 1)].map (fun (dx, dy) => (x+dx, y+dy))
    let oldCorners := fourPoints.map (countCornersAt area) |>.sum
    let area := area.insert pos
    let newCorners := fourPoints.map (countCornersAt area) |>.sum
    let sides := sides + (newCorners - oldCorners)
    List.foldl (detectBlock input) (area, sides) newPlaces

def calcScore(input: Array (Array Char)) (start: Int × Int): (Std.HashSet (Int × Int) × Int) :=
  let (area, perimeter) := detectBlock input (Std.HashSet.empty, 0) start
  (area, area.size * perimeter)

def solve (input: Array (Array Char)): Int :=
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
