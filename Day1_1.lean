partial def loop (input: IO.FS.Stream) (acc: List (Int × Int)): IO (List (Int × Int)) := do
  let l ← input.getLine
  match l.split (' ' = .) with
  | [a,_, _, b] =>
    let a := a.dropRightWhile Char.isWhitespace
    let b := b.dropRightWhile Char.isWhitespace
    let b := b.dropWhile Char.isWhitespace
    loop input ((a.toInt!, b.toInt!) :: acc)
  | _ => return acc

def solve (input: List (Int × Int)): Nat :=
  let (l, r) := input.unzip
  let l := l.mergeSort
  let r := r.mergeSort
  let list := List.zipWith (. - .) l r
  List.foldl (fun (acc) (sub) => acc + sub.natAbs) 0 list


def main: IO Unit := do
  let stdin ← IO.getStdin
  let list ← loop stdin []
  let answer := solve list
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
