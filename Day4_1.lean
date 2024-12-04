partial def readAll (input: IO.FS.Stream) : IO ByteArray :=
  let rec loop (acc: ByteArray): IO ByteArray := do
    let buf ← input.read 4096
    let acc := acc.append buf
    if buf.size < 4096
    then return acc
    else loop acc
  loop ByteArray.empty

def solve (input: Array (Array Char)): Nat :=
  let xmax := input.size
  let ymax := input.get! 0 |> Array.size
  let rec getOneDiag (x y: Nat) (diag: List (Int × Int)): List Char :=
    let x := Int.ofNat x
    let y := Int.ofNat y
    let indices := diag.map (fun ((xd, yd)) => (x + xd, y+yd))
    if indices.all (fun ((x, y)) => 0 <= x && x < xmax && 0 <= y && y < ymax)
    then indices.map (fun ((x, y)) => (input.get! (Int.toNat x)).get! (Int.toNat y))
    else []
  let rec getEightDiags (x y: Nat): List (List Char) :=
    [
      getOneDiag x y [(0, 0), (0, -1), (0, -2), (0, -3)],
      getOneDiag x y [(0, 0), (1, -1), (2, -2), (3, -3)],
      getOneDiag x y [(0, 0), (1, 0), (2, 0), (3, 0)],
      getOneDiag x y [(0, 0), (1, 1), (2, 2), (3, 3)],
      getOneDiag x y [(0, 0), (0, 1), (0, 2), (0, 3)],
      getOneDiag x y [(0, 0), (-1, 1), (-2, 2), (-3, 3)],
      getOneDiag x y [(0, 0), (-1, 0), (-2, 0), (-3, 0)],
      getOneDiag x y [(0, 0), (-1, -1), (-2, -2), (-3, -3)]
    ]
  let xs := List.iota xmax |> List.map (. - 1)
  let ys := List.iota ymax |> List.map (. - 1)
  let counts := xs.map (fun (x) => ys.map (fun (y) => getEightDiags x y |> List.count ['X', 'M', 'A', 'S']))
  List.map (List.foldl (. + .) 0) counts |> List.foldl (. + .) 0


def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let input := (String.fromUTF8! input).dropRight 1
  let input := input.split (. = '\n')
                 |> List.map String.toList
                 |> List.map List.toArray
                 |> List.toArray
  let answer := solve input
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
