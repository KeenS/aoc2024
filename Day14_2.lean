import Std.Data.HashMap
import Std.Data.HashSet

partial def readAll (input: IO.FS.Stream) : IO (List ((Int × Int) × (Int × Int))) :=
  let rec loop (acc: List ((Int × Int) × (Int × Int))): IO (List ((Int × Int) × (Int × Int))) := do
    let l ← input.getLine |>.map String.trim
    if l == "" then return acc.reverse else
    let l := l.stripPrefix "p="
    let px := l.takeWhile (fun c => c.isDigit || c == '-')
    let l := l.drop px.length
    let px := px.toInt!
    let l := l.stripPrefix ","
    let py := l.takeWhile (fun c => c.isDigit || c == '-')
    let l := l.drop py.length
    let py := py.toInt!
    let l := l.stripPrefix " v="
    let vx := l.takeWhile (fun c => c.isDigit || c == '-')
    let l := l.drop vx.length
    let vx := vx.toInt!
    let l := l.stripPrefix ","
    let vy := l.takeWhile (fun c => c.isDigit || c == '-')
    let l := l.drop vy.length
    let vy := vy.toInt!
    assert! l == ""
    loop (((px, py), (vx, vy)) ::acc)
  loop []

def tickN (n: Nat) (input: (Int × Int) × (Int × Int)): Int × Int :=
  -- given in the problem
  let (x, y) := (101, 103)
  let ((px, py), (vx, vy)) := input
  ((px + n*vx) % x, (py + n*vy) % y)


partial def printAll (output: IO.FS.Stream) (input: List ((Int × Int) × (Int × Int))) (n: Nat): IO Unit := do
  if n == 0 then return () else
  let robots := input.map (tickN n)
  let pic := Array.mkArray 103 (mkArray 103 ' ')
  let pic := List.foldl (fun pic (x, y) => pic.set! y.toNat ((pic.get! y.toNat).set! x.toNat '*')) pic robots
  let pic := pic.map (fun line =>  (Array.toList line).asString) |>.toList
  let pic := "\n".intercalate pic
  output.putStrLn pic
  output.putStrLn s!"Step {n}"
  IO.sleep 400
  printAll output input (n + 103)

def main: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let input ← readAll stdin
  -- 173
  printAll stdout input 276
  -- 379
