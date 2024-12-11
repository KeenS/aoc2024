def readAll (input: IO.FS.Stream): IO (List Nat) := do
  let line ← input.getLine
  let line := line.trimRight.splitOn
  return line.map (String.toNat!)


def blink(input: List Nat): List Nat :=
  List.foldl (fun acc e =>
    let digits := Nat.toDigits 10 e
    if e == 0 then 1::acc
    else if digits.length % 2 == 0
    then let (l, r) := digits.splitAt (digits.length / 2)
         r.asString.toNat! :: l.asString.toNat! :: acc
    else e * 2024 :: acc
  ) [] input |>.reverse

def solve(input: List Nat): Nat :=
  Nat.repeat blink 25 input |>.length

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let answer := solve input
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
