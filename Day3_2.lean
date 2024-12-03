partial def readAll (input: IO.FS.Stream) : IO ByteArray :=
  let rec loop (acc: ByteArray): IO ByteArray := do
    let buf ← input.read 4096
    let acc := acc.append buf
    if buf.size < 4096
    then return acc
    else loop acc
  loop (ByteArray.mk (Array.mkArray4 100 111 40 41))

def solve (input: String): Nat :=
  let cands := (input.splitOn "don't()").map (String.splitOn . "do()")
  let cands := List.foldl List.append [] (cands.map List.tail!)
  let cands := List.foldl List.append [] (cands.map (fun (s) => s.split ('m' = .)))
  let rec f (s: String): Option Nat := do
    let s ← if s.startsWith "ul("
            then Option.some $ s.drop 3
            else Option.none
    let d1 := s.takeWhile Char.isDigit
    let s  ← if 0 < d1.length
             then Option.some $ s.drop d1.length
             else Option.none
    let s ← if s.startsWith ","
            then Option.some $ s.drop 1
            else Option.none
    let d2 := s.takeWhile Char.isDigit
    let s  ← if 0 < d2.length
             then Option.some $ s.drop d2.length
             else Option.none
    let _ ← if s.startsWith ")"
            then Option.some $ s.drop 1
            else Option.none
    pure $ d1.toNat! * d2.toNat!
  List.foldl (fun (acc) (elm) => match f elm with | .some a => a + acc | .none => acc) 0 cands

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let stdout ← IO.getStdout
  let answer := solve $ String.fromUTF8! input
  stdout.putStrLn s!"{answer}"
