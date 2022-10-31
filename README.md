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
  10 <- block.call add [];
  11 <- pure [Local "4"];
  8 <- closure.enter [Local "10"] [Local "11"];
  9 <- pure [Local "1"];
  7 <- closure.enter [Local "8"] [Local "9"];
  closure.enter [Local "6"] [Local "7"]
end

block add [] do
  closure.alloc add_0 {}
end

block start [] do
  2 <- block.call add [];
  4 <- block.call succ [];
  5 <- block.call zero [];
  3 <- closure.enter [Local "4"] [Local "5"];
  0 <- closure.enter [Local "2"] [Local "3"];
  6 <- block.call succ [];
  8 <- block.call succ [];
  9 <- block.call zero [];
  7 <- closure.enter [Local "8"] [Local "9"];
  1 <- closure.enter [Local "6"] [Local "7"];
  closure.enter [Local "0"] [Local "1"]
end
```