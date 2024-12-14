import Std.Data.HashMap
import Std.Data.HashSet

partial def readAll (input: IO.FS.Stream) : IO (List ((Nat × Nat) × (Nat × Nat) × (Nat × Nat))) :=
  let rec loop (acc: List ((Nat × Nat) × (Nat × Nat) × (Nat × Nat))): IO (List ((Nat × Nat) × (Nat × Nat) × (Nat × Nat))) := do
    let a ← input.getLine |>.map String.trim
    let a := a.stripPrefix "Button A: X+"
    let ax := a.takeWhile Char.isDigit
    let a := a.drop ax.length
    let a_x := ax.toNat!
    let a := a.stripPrefix ", Y+"
    let a_y := a.toNat!
    let b ← input.getLine |>.map String.trim
    let b := b.stripPrefix "Button B: X+"
    let bx := b.takeWhile Char.isDigit
    let b := b.drop bx.length
    let b_x := bx.toNat!
    let b := b.stripPrefix ", Y+"
    let b_y := b.toNat!
    let p ← input.getLine |>.map String.trim
    let p := p.stripPrefix "Prize: X="
    let px := p.takeWhile Char.isDigit
    let p := p.drop px.length
    let p_x := px.toNat!
    let p := p.stripPrefix ", Y="
    let p_y := p.toNat!
    let blank ← input.getLine
    let acc := ((a_x, a_y), (b_x, b_y), (p_x, p_y)) :: acc
    if blank == "\n" then loop acc
    else return acc.reverse
  loop []

@[inline]
def reachTimes (button: (Nat × Nat)) (prize: Nat × Nat): Option Nat :=
  let ( x,  y) := button
  let (px, py) := prize
  let m := px / x
  if m*x == px && m*y == py
  then .some m
  else .none

def calcCost (input: (Nat × Nat) × (Nat × Nat) × (Nat × Nat)): Nat :=
  let ((x₁, y₁), (x₂, y₂), (px, py)) := input
  let xgcd := x₁.gcd x₂
  let ygcd := y₁.gcd y₂
  if (px % xgcd != 0) || (py % ygcd != 0) then 0
  else
    let x₁ := x₁ / xgcd
    let x₂ := x₂ / xgcd
    let px := px / xgcd
    let y₁ := y₁ / ygcd
    let y₂ := y₂ / ygcd
    let py := py / ygcd
    let nx := px / x₁
    let ny := py / y₁
    let n := nx.min ny
    let f := reachTimes (x₂, y₂)
    List.range (n+1)
      |>.filterMap (fun n =>  f (px - n*x₁, py - n*y₁) |>.map (n, ·))
      |>.map (fun (n, m) => 3 * n + m)
      |> List.foldl (fun acc e => match acc with | Option.none => .some e | .some v => .some (v.min e)) Option.none
      |>.getD 0

def solve (input: List ((Nat × Nat) × (Nat × Nat) × (Nat × Nat))): Nat :=
  input.map calcCost |>.sum

def main: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let input ← readAll stdin
  stdout.putStrLn s!"{input}"
  let answer := solve input
  stdout.putStrLn s!"{answer}"
