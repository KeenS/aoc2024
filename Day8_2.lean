import Std.Data.HashMap

partial def readAll (input: IO.FS.Stream) : IO ByteArray :=
  let rec loop (acc: ByteArray): IO ByteArray := do
    let buf ← input.read 4096
    let acc := acc.append buf
    if buf.size < 4096
    then return acc
    else loop acc
  loop ByteArray.empty

structure Cord: Type where
  x: Int
  y: Int
deriving Repr, Hashable, BEq

def Cord.org: Cord := ⟨0, 0⟩
def Cord.antinode (a: Cord) (b: Cord): Cord :=
  ⟨2 * a.x - b.x, 2 * a.y- b.y⟩
def Cord.isIn (start: Cord) (bottom: Cord) (v: Cord): Bool :=
  start.x <= v.x && v.x < bottom.x && start.y <= v.y && v.y < bottom.y

def Cord.antinodes (dim: Cord) (a: Cord) (b: Cord): List Cord :=
  let diff_x := a.x - b.x
  let diff_y := a.y - b.y
  let gcd := diff_x.gcd diff_y
  let (dx, dy) := (diff_x / gcd, diff_y / gcd)
  let maxRepeat :=  max dim.x dim.y
  let cands := List.iota maxRepeat.toNat |> (0::·) |>.map (fun n => [⟨a.x + n*dx, a.y + n*dy⟩,⟨a.x - n*dx, a.y - n*dy⟩]) |>.flatten
  cands

def parse(input: String): (Cord × Std.HashMap Char (List Cord)) :=
  let lines := input.dropRightWhile Char.isWhitespace |>.splitOn "\n"
  let chars := lines.map String.toList
  let dim   := ⟨chars.get! 0 |>.length, chars.length⟩
  let tbl   := List.foldl (fun acc (x, line) =>
    List.foldl (fun acc (y, char) =>
      let cord: Cord := ⟨x, y⟩
      if  char.isAlphanum
      then acc.alter char (fun l => Option.some $ cord :: l.getD [])
      else acc
    ) acc line.enum
  ) Std.HashMap.empty chars.enum
  (dim, tbl)


def solve (input: (Cord × Std.HashMap Char (List Cord))): Nat :=
  let (dim, tbl) := input
  let data: Std.HashMap Cord Unit := Std.HashMap.empty
  let data := Std.HashMap.fold (fun acc _ v =>
    let antinodes := List.foldl (fun (acc, prevs) cord =>
      let cords := prevs.map (Cord.antinodes dim cord) |>.flatten
      (cords ++ acc, cord::prevs)
      ) ([], []) v
    List.foldl (fun acc e => acc.insert e ()) acc antinodes.fst
  ) data tbl
  data.keys.filter (Cord.isIn Cord.org dim) |>.length

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let data := parse $ String.fromUTF8! input
  let answer := solve data
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
