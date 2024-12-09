def readAll (input: IO.FS.Stream) : IO String :=
  input.getLine.map (·.dropRight 1)

def expandAux(input: List Char) (isData: Bool) (id: Nat) (acc: List (Array String)): List (Array String) :=
  match input with
  | [] => acc.reverse
  | c::cs =>
    let n := c.toString.toNat!
    let c := if isData then id.repr else "."
    let elem := Array.mkArray n c
    let id := if isData then id + 1 else id
    expandAux cs isData.not id (elem::acc)

def expand(input: String): Array String :=
  expandAux input.toList true 0 [] |> List.foldl Array.append Array.empty

-- TODO: use Finn input.size
def rearrangeAux(input: Array String) (front: Nat) (back: Nat): Array String :=
  if back <= front then input
  else if input.get! front != "." then rearrangeAux input (front + 1) back
  else if input.get! back == "." then rearrangeAux input front (back - 1)
  else rearrangeAux (input.swap! front back) (front + 1) (back - 1)
termination_by (back - front)

def rearrange(input: Array String): Array String :=
  rearrangeAux input 0 (input.size - 1)

def checksum(input: Array String): Nat :=
  input.toList.map String.toNat?
   |>.map (·.getD 0)
   |>.enum
   |>.map (fun (n, e) => n * e)
   |>.sum

def solve (input: String): Nat :=
  let arr := expand input
  let arr := rearrange arr
  checksum arr

def main: IO Unit := do
  let stdin ← IO.getStdin
  let input ← readAll stdin
  let answer := solve input
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
