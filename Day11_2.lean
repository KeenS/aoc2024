import Std.Data.HashMap

def readAll (input: IO.FS.Stream): IO (List Nat) := do
  let line ← input.getLine
  let line := line.trimRight.splitOn
  return line.map (String.toNat!)


def memoize (k: (Nat × Nat)) (input: (Std.HashMap (Nat × Nat) Nat × Nat)): (Std.HashMap (Nat × Nat) Nat × Nat) :=
  let (memo, v) := input
  let memo := memo.insert k v
  (memo, v)

def countNumbers (memo: Std.HashMap (Nat × Nat) Nat) (n: Nat) (e: Nat): (Std.HashMap (Nat × Nat) Nat × Nat) :=
  if let .some v := memo.get? (n, e) then (memo, v)
  else match n.decEq 0 with
  | isTrue _ => (memo.insert (n, e) 1, 1)
  | isFalse f =>
    have: (n - 1) < n := Nat.sub_one_lt f
    let digits := Nat.toDigits 10 e
    if e == 0 then
      memoize (n, e) <| countNumbers memo (n - 1) 1
    else if digits.length % 2 == 0
    then
      let (l, r) := digits.splitAt (digits.length / 2)
      let (memo, ln) := countNumbers memo (n - 1) l.asString.toNat!
      let (memo, rn) := countNumbers memo (n - 1) r.asString.toNat!
      memoize (n, e) (memo, ln + rn)
    else
     memoize (n, e) <| countNumbers memo (n - 1) (e * 2024)

def solve(input: List Nat): Nat :=
  let n := 75
  let (_, sum) := List.foldl (fun (memo, sum) e =>
     let (memo, ret) := memoize (n, e) <| countNumbers memo n e
     (memo, ret + sum)) (Std.HashMap.empty, 0) input
  sum

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let answer := solve input
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
