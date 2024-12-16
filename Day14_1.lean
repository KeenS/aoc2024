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

def tick100 (input: (Int × Int) × (Int × Int)): Int × Int :=
  -- given in the problem
  let (x, y) := (101, 103)
  let ((px, py), (vx, vy)) := input
  ((px + 100*vx) % x, (py + 100*vy) % y)


def solve (input: List ((Int × Int) × (Int × Int))): Int :=
  let mx := 50
  let my := 51
  let robots := input.map tick100
  let q1 := robots.countP (fun (x, y) => x < mx && y < my)
  let q2 := robots.countP (fun (x, y) => x < mx && my < y)
  let q3 := robots.countP (fun (x, y) => mx < x && y < my)
  let q4 := robots.countP (fun (x, y) => mx < x && my < y)
  q1 * q2 * q3 * q4


def main: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let input ← readAll stdin
  let answer := solve input
  stdout.putStrLn s!"{answer}"
