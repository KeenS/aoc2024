partial def loop (input: IO.FS.Stream) (acc: List (List Int)): IO (List (List Int)) := do
  let l ← input.getLine
  if l = ""
  then return acc
  else
    let numbers := l.split  (' ' = .)
    let numbers := numbers.map (fun (n) => n.dropRightWhile Char.isWhitespace)
    let numbers := numbers.map (fun (n) => n.dropWhile Char.isWhitespace)
    let numbers := numbers.filterMap String.toInt?
    loop input (numbers :: acc)

inductive Tolerence: Type
  | clean
  | removing
  | removed
  | failed

def Tolerence.nextTolerence: Tolerence -> Tolerence
  | .clean => .removing
  | .removing => .removed
  | .removed => .failed
  | .failed => .failed

def Tolerence.isFailed: Tolerence -> Bool
  | .failed => true
  | _ => false

def oneDropped: List Int -> List (List Int)
  | x :: xs => xs :: ((oneDropped xs).map (x :: .))
  | [] => []

def isSafe (input: List Int): Bool :=
  let pair := input.zip (input.tail)
  let isInc := pair.all (fun (a, b) => a < b)
  let isDec := pair.all (fun (a, b) => a > b)
  let isSmallDiff := pair.all (fun (a, b) => 0 < (a - b).natAbs && (a - b).natAbs < 4)
  (isInc || isDec) && isSmallDiff

def isSafeAll (input: List Int): Bool := 
  (oneDropped input).any isSafe

def solve (input: List (List Int)): Nat :=
  List.countP isSafeAll input
  -- List.countP isSafe input


def main: IO Unit := do
  let stdin ← IO.getStdin
  let list ← loop stdin []
  let answer := solve list
  let stdout ← IO.getStdout
  stdout.putStrLn s!"{answer}"
