# ir-curios

IRCurios: The new intermediate representation for Curios.

## Example

The following program ...

```
Unit : Type;
Unit = {unit};

unit : Unit;
unit = :unit;

Nat : Type;
Nat =
  (l: {zero succ}) *> match l {
    |zero| Unit;
    |succ| Nat;
  };

zero : Nat;
zero = :zero, unit;

succ : Nat -> Nat;
succ = a => :succ, a;

add : Nat -> Nat -> Nat;
add = a => b =>
  split a {|l, n| match l {
    |zero| b;
    |succ| succ (add n b);
  }};

start : Nat;
start = add (succ zero) (succ (succ zero));
```

... will generate the following intermediate representation.

```
closure succ_0 {} [0] do
  1 <- int32.alloc 2;
  2 <- pure [Local "0"];
  struct.alloc [Local "1", Local "2"]
end

closure add_1 {0} [1] do
  2 <- pure [Environmental "0"];
  3 <- struct.select [Local "2"] 0;
  4 <- struct.select [Local "2"] 1;
  5 <- pure [Local "3"];
  int32.match [Local "5"]: |1| block.call add_0 [Local "1"] |2| block.call add_1 [Local "4", Local "1"]
end

closure add_0 {} [0] do
  closure.alloc add_1 {Local "0"}
end

block Unit [] do
  pure [Null]
end

block unit [] do
  int32.alloc 0
end

block Nat [] do
  pure [Null]
end

block zero [] do
  0 <- int32.alloc 1;
  1 <- block.call unit [];
  struct.alloc [Local "0", Local "1"]
end

block succ [] do
  closure.alloc succ_0 {}
end

block add_0 [1] do
  pure [Local "1"]
end

block add_1 [4, 1] do
  6 <- block.call succ [];
  7 <- block.call add [];
  8 <- pure [Local "4"];
  9 <- closure.enter [Local "7"] [Local "8"];
  10 <- pure [Local "1"];
  11 <- closure.enter [Local "9"] [Local "10"];
  closure.enter [Local "6"] [Local "11"]
end

block add [] do
  closure.alloc add_0 {}
end

block start [] do
  0 <- block.call add [];
  1 <- block.call succ [];
  2 <- block.call zero [];
  3 <- closure.enter [Local "1"] [Local "2"];
  4 <- closure.enter [Local "0"] [Local "3"];
  5 <- block.call succ [];
  6 <- block.call succ [];
  7 <- block.call zero [];
  8 <- closure.enter [Local "6"] [Local "7"];
  9 <- closure.enter [Local "5"] [Local "8"];
  closure.enter [Local "4"] [Local "9"]
end
```