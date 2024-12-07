partial def readAll (input: IO.FS.Stream) : IO ByteArray :=
  let rec loop (acc: ByteArray): IO ByteArray := do
    let buf ← input.read 4096
    let acc := acc.append buf
    if buf.size < 4096
    then return acc
    else loop acc
  loop ByteArray.empty

def parse(input: String): List (Nat × List Nat) :=
  let lines := input.dropRightWhile Char.isWhitespace |>.splitOn "\n"
  let numbers := lines.map (·.split Char.isWhitespace)
  numbers.map (fun line =>
    let head := line.head!
    let tail := line.tail!
    (head.dropRight 1 |> String.toNat!, tail.map String.toNat!)
  )


def cat (a b: Nat): Nat :=
  let sig := (10).toDigits b |>.length
  a * 10 ^ sig + b

def canBeCalibratedAux(result: Nat) (data: List Nat) (acc: Nat) (op: Nat -> Nat -> Nat): Bool :=
  match data with
  | [] => result == acc
  | [n] => result == op acc n
  | n::ns =>
    let f := canBeCalibratedAux result ns (op acc n)
    acc <= result && (f (. + .) || f (. * .) || f cat)

def canBeCalibrated (input: (Nat × List Nat)): Bool :=
  let (result, data) := input
  canBeCalibratedAux result data 0 (. + .)

#check canBeCalibrated (190, [10, 19])
#check canBeCalibrated (3267, [81, 40, 27])
#check canBeCalibrated (156, [15, 6])

def solve (input: List (Nat × List Nat)): Nat :=
  input.filter (canBeCalibrated) |>.map Prod.fst |>.sum

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let data := parse $ String.fromUTF8! input
  let answer := solve data
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"

