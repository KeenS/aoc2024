def readAll (input: IO.FS.Stream) : IO String :=
  input.getLine.map (·.dropRight 1)

def expandAux(input: List Char) (isData: Bool) (id: Nat) (acc: List (Nat × String)): List (Nat × String) :=
  match input with
  | [] => acc.reverse
  | c::cs =>
    let n := c.toString.toNat!
    let c := if isData then id.repr else "."
    let elem := (n, c)
    let id := if isData then id + 1 else id
    expandAux cs isData.not id (elem::acc)

def expand(input: String): Array (Nat × String) :=
  expandAux input.toList true 0 [] |> List.toArray

partial def defragSpaceAux(i: Nat) (input: Array (Nat × String)): Array (Nat × String) :=
  if       input.size        <= i+1                                  then input
  else if (input.get! i).snd != "." || (input.get! (i+1)).snd != "." then defragSpaceAux (i+1) input
  else
    let newSize := (input.get! i).fst + (input.get! (i+1)).fst
    let input := input.eraseIdx i
    let input := input.set! i (newSize, ".")
    defragSpaceAux i input

partial def defragSpace (input: Array (Nat × String)): Array (Nat × String) := defragSpaceAux 0 input

-- TODO: use Fin input.size
partial def rearrangeAux(input: Array (Nat × String)) (front: Nat) (back: Nat): Array (Nat × String) :=
  let fe := input.get! front
  let be := input.get! back
  if back == 0            then input
  else if back <= front   then rearrangeAux input 0 (back - 1)
  else if fe.snd != "."   then rearrangeAux input (front + 1) back
  else if be.snd == "."   then rearrangeAux input front (back - 1)
  else if fe.fst < be.fst then rearrangeAux input (front + 1) back
  else if fe.fst = be.fst then
     let input := input.swap! front back |> defragSpace
     rearrangeAux input 0 (back - 1)
  -- be.fst < fe.fst
  else
    let input := input.insertAt! (front + 1) (fe.fst - be.fst, ".")
    let input := input.set! front (be.fst, ".")
    let input := input.swap! front (back + 1) |> defragSpace
    rearrangeAux input 0 back

def rearrange(input: Array (Nat × String)): Array (Nat × String) :=
  rearrangeAux input 0 (input.size - 1)

def checksum(input: Array (Nat × String)): Nat :=
  input.toList.map (fun (n, s) => Array.mkArray n s)
   |> List.foldl Array.append Array.empty
   |>.toList
   |>.map String.toNat?
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
