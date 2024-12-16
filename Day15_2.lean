import Std.Data.HashMap
import Std.Data.HashSet

partial def readAll (input: IO.FS.Stream) : IO ((Array (Array Char)) × List Char) :=
  let rec widen (c: Char): List Char :=
    match c with
    | '#' => ['#', '#']
    | 'O' => ['[', ']']
    | '.' => ['.', '.']
    | '@' => ['@', '.']
    |  c  => assert! false; [c, c]
  let rec readGoods (acc: List (Array Char)): IO (Array (Array (Char))) := do
    let line ← input.getLine |>.map String.trim
    if line == ""
    then do
      return acc.reverse |> List.toArray
    else readGoods  (line.toList.map widen |>.flatten |>.toArray |> (· :: acc))
  let rec readMoves(acc: List (List Char)): IO (List Char) := do
      let line ← input.getLine |>.map String.trim
      if line == ""
      then return acc.reverse.flatten
      else readMoves (line.toList :: acc)
  do
    let goods ← readGoods []
    let moves ← readMoves []
    return (goods, moves)

def findRobot(goods: Array (Array Char)): (Nat × Nat) :=
  List.range goods.size |>.findSome? (fun y =>
    let line := goods.get! y
    List.range line.size |>.findSome? (fun x =>
      if line.get! x == '@'
      then Option.some (x, y)
      else Option.none
    )
  ) |>.get!


def Array.g (goods: Array (Array Char)) (pos: Nat × Nat): Char :=
  let (x, y) := pos
  goods.get! y |>.get! x

def Array.s (goods: Array (Array Char)) (pos: Nat × Nat) (c: Char): Array (Array Char) :=
  let (x, y) := pos
  let line := goods.get! y
  line.set! x c |> goods.set! y

def left (pos: Nat × Nat): Nat × Nat :=
  let (x, y) := pos
  (x-1, y)

def right (pos: Nat × Nat): Nat × Nat :=
  let (x, y) := pos
  (x+1, y)

partial def canMove? (goods: Array (Array Char)) (pos: Nat × Nat) (diff: Int × Int): Bool :=
  let (x, y) := pos
  let (dx, dy) := diff
  let new := ((x+dx).toNat, (y+dy).toNat)
  let e := goods.g new
  if e == '#'
  then false
  else if e == '[' then canMove? goods new diff && (dy == 0 || canMove? goods (right new) diff)
  else if e == ']' then canMove? goods new diff && (dy == 0 || canMove? goods (left new) diff)
  else assert! e == '.'; true


partial def move! (goods: Array (Array Char)) (pos: Nat × Nat) (diff: Int × Int): Array (Array Char) :=
  let (x, y) := pos
  let (dx, dy) := diff
  let cur := goods.g pos
  let new := ((x+dx).toNat, (y+dy).toNat)
  let e := goods.g new
  if e == '[' then
    let goods := move! goods new diff
    let goods := if dx == 0 then move! goods (right new) diff else goods
    (goods.s pos '.').s new cur
  else if e == ']' then
    let goods := move! goods new diff
    let goods := if dx ==0 then move! goods (left new) diff else goods
    (goods.s pos '.').s new cur
  else assert! e == '.';
    (goods.s pos '.').s new cur

partial def move (goods: Array (Array Char)) (pos: Nat × Nat) (diff: Int × Int): (Array (Array Char) × (Nat × Nat)) :=
  let (x, y) := pos
  let (dx, dy) := diff
  let new := ((x+dx).toNat, (y+dy).toNat)
  if canMove? goods pos diff
  then (move! goods pos diff, new)
  else (goods, pos)


def calcScore(goods: Array (Array Char)): Nat :=
  List.range goods.size |>.map (fun y =>
    let line := goods.get! y
    List.range line.size |>.map (fun x =>
      if line.get! x == '['
      then 100 * y + x
      else 0
    ) |>.sum
  ) |>.sum

def solve (input: (Array (Array Char) × List Char)): Nat :=
  let (goods, moves) := input
  let pos := findRobot goods
  let (goods, _) := List.foldl (fun (goods, pos) m =>
    match m with
      | '^' => move goods pos ( 0, -1)
      | '>' => move goods pos ( 1,  0)
      | 'v' => move goods pos ( 0,  1)
      | '<' => move goods pos (-1,  0)
      | _ => assert! false; (goods, pos)
  ) (goods, pos) moves
  calcScore goods
  -- "\n".intercalate (goods.map (fun line => line.toList.asString)).toList

def main: IO Unit := do
  let stdin ← IO.getStdin
  let stdout ← IO.getStdout
  let input ← readAll stdin
  let answer := solve input
  stdout.putStrLn s!"{answer}"
