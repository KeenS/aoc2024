partial def readAll (input: IO.FS.Stream) : IO (List (Nat × Nat) × List (List Nat)) :=
  let rec readRule (acc: List (Nat × Nat)): IO (List (Nat × Nat)) := do
    let line ← input.getLine
    let line := line.dropRight 1
    if line = ""
    then return acc.reverse
    else
      let digits := line.splitOn "|"
      let rule := digits.map String.toNat!
      let rule := (rule.get! 0, rule.get! 1)
      readRule $ rule :: acc
  let rec readPages (acc: List (List Nat)): IO (List (List Nat)) := do
    let line ← input.getLine
    let line := line.dropRight 1
    if line = ""
    then return acc.reverse
    else
      let pages := line.splitOn "," |> List.map String.toNat!
      readPages $ pages::acc
  do
    let rules ← readRule []
    let pages ←  readPages []
    return (rules, pages)

def isOk (rules: List (Nat × Nat)): List Nat -> Bool
  | []    => true
  | x::xs =>
    let mustNot := List.filter (fun (_, b) => x == b) rules |> List.map (fun (a, _) => a)
    let this := (mustNot.map (fun (a) => xs.all (. != a))).and
    this && isOk rules xs

def solve (input: List (Nat × Nat) × List (List Nat)): Nat :=
  let (rules, pages) := input
  let rec getMidPage (pages: List Nat): Nat := pages.get! (pages.length / 2)
  let okPagesList := List.filter (isOk rules) pages
  okPagesList |> List.map getMidPage |> List.sum

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let stdout ← IO.getStdout
  let answer := solve  input
  stdout.putStrLn s!"{answer}"
