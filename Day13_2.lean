import Std.Data.HashMap
import Std.Data.HashSet

partial def readAll (input: IO.FS.Stream) : IO (List ((Int × Int) × (Int × Int) × (Int × Int))) :=
  let rec loop (acc: List ((Int × Int) × (Int × Int) × (Int × Int))): IO (List ((Int × Int) × (Int × Int) × (Int × Int))) := do
    let a ← input.getLine |>.map String.trim
    let a := a.stripPrefix "Button A: X+"
    let ax := a.takeWhile Char.isDigit
    let a := a.drop ax.length
    let a_x := ax.toInt!
    let a := a.stripPrefix ", Y+"
    let a_y := a.toInt!
    let b ← input.getLine |>.map String.trim
    let b := b.stripPrefix "Button B: X+"
    let bx := b.takeWhile Char.isDigit
    let b := b.drop bx.length
    let b_x := bx.toInt!
    let b := b.stripPrefix ", Y+"
    let b_y := b.toInt!
    let p ← input.getLine |>.map String.trim
    let p := p.stripPrefix "Prize: X="
    let px := p.takeWhile Char.isDigit
    let p := p.drop px.length
    let p_x := px.toInt! + 10000000000000
    let p := p.stripPrefix ", Y="
    let p_y := p.toInt!  + 10000000000000
    let blank ← input.getLine
    let acc := ((a_x, a_y), (b_x, b_y), (p_x, p_y)) :: acc
    if blank == "\n" then loop acc
    else return acc.reverse
  loop []

partial def calcCost (input: (Int × Int) × (Int × Int) × (Int × Int)): Int :=
    -- x₁n + x₂m = Px
    -- y₁n + y₂m = Py
  let ((x₁, y₁), (x₂, y₂), (px, py)) := input
  if y₁ == 0 then -- calc it
    let m := py / y₂
    let n := (px - m * x₂) / x₁
    if 0 <= n && 0 <= m && x₁*n + x₂*m == px && y₂*m == py then 3*n + m else 0
  else if x₁ < y₁ then
     -- for the sake of simplicity
     calcCost ((y₁, x₁), (y₂, x₂), (py, px))
  else
    let r := x₁ / y₁
    assert! x₁ >= r*y₁
    calcCost ((x₁ - r*y₁, y₁), (x₂ - r*y₂, y₂), (px - r*py, py))

def solve (input: List ((Int × Int) × (Int × Int) × (Int × Int))): Int :=
  input.map (fun input =>
    let ((x₁, y₁), (x₂, y₂), (px, py)) := input
    let xgcd := x₁.gcd x₂
    let ygcd := y₁.gcd y₂
    if (px % xgcd != 0) || (py % ygcd != 0) then 0
    else calcCost input) |>.sum

def main: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let input ← readAll stdin
  let answer := solve input
  stdout.putStrLn s!"{answer}"
