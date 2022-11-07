# ir-curios

IRCurios: The new intermediate representation for Curios.

## Example

The following program ...

```
Unit : Type;
Unit = {unit};

unit : Unit;
unit = :unit;

List : Type -> Type;
List = A =>
  (label: {null cons}) *> match label {
    |null| Unit;
    |cons| A *> List A;
  };

null : (A: Type) -> List A;
null = A => :null, unit;

cons : (A: Type) -> A -> List A -> List A;
cons = A => a => rest => :cons, a, rest;

fold : (A: Type) -> (B: Type) -> (B -> A -> B) -> B -> List A -> B;
fold = A => B => action => initial => list =>
  split list {|label, list| match label {
    |null| initial;
    |cons| split list {|a, rest| fold A B action (action initial a) rest};
  }};

Nat : Type;
Nat =
  (label: {zero succ}) *> match label {
    |zero| Unit;
    |succ| Nat;
  };

zero : Nat;
zero = :zero, unit;

succ : Nat -> Nat;
succ = value => :succ, value;

add : Nat -> Nat -> Nat;
add = one => other =>
  split one {|label, one| match label {
    |zero| other;
    |succ| succ (add one other);
  }};

one : Nat;
one = succ zero;

two : Nat;
two = succ one;

three : Nat;
three = succ two;

nat_list : List Nat;
nat_list = cons Nat one (cons Nat two (cons Nat three (null Nat)));

folded_nat_list : Nat;
folded_nat_list = fold Nat Nat add zero nat_list;

to_int32 : Nat -> Int32;
to_int32 = value =>
  split value {|label, value| match label {
    |zero| 0;
    |succ| [int32.add 1 (to_int32 value)];
  }};

start : Int32;
start = to_int32 folded_nat_list;
```

... will generate the following intermediate representation.

```
closure List_0 {} [0] do
  pure [Null]
end

closure null_0 {} [0] do
  1 <- int32.alloc 1;
  2 <- block.call unit [];
  struct.alloc [Local "1", Local "2"]
end

closure cons_2 {1} [2] do
  3 <- int32.alloc 2;
  4 <- pure [Environmental "1"];
  5 <- pure [Local "2"];
  6 <- struct.alloc [Local "4", Local "5"];
  struct.alloc [Local "3", Local "6"]
end

closure cons_1 {} [1] do
  closure.alloc cons_2 {Local "1"}
end

closure cons_0 {} [0] do
  closure.alloc cons_1 {}
end

closure fold_4 {3, 0, 1, 2} [4] do
  5 <- pure [Local "4"];
  6 <- struct.select [Local "5"] 0;
  7 <- struct.select [Local "5"] 1;
  8 <- pure [Local "6"];
  int32.match [Local "8"]: |1| block.call fold_0 [Environmental "3"] |2| block.call fold_1 [Local "7", Environmental "0", Environmental "1", Environmental "2", Environmental "3"]
end

closure fold_3 {0, 1, 2} [3] do
  closure.alloc fold_4 {Local "3", Environmental "0", Environmental "1", Environmental "2"}
end

closure fold_2 {0, 1} [2] do
  closure.alloc fold_3 {Environmental "0", Environmental "1", Local "2"}
end

closure fold_1 {0} [1] do
  closure.alloc fold_2 {Environmental "0", Local "1"}
end

closure fold_0 {} [0] do
  closure.alloc fold_1 {Local "0"}
end

closure succ_0 {} [0] do
  1 <- int32.alloc 4;
  2 <- pure [Local "0"];
  struct.alloc [Local "1", Local "2"]
end

closure add_1 {0} [1] do
  2 <- pure [Environmental "0"];
  3 <- struct.select [Local "2"] 0;
  4 <- struct.select [Local "2"] 1;
  5 <- pure [Local "3"];
  int32.match [Local "5"]: |3| block.call add_0 [Local "1"] |4| block.call add_1 [Local "4", Local "1"]
end

closure add_0 {} [0] do
  closure.alloc add_1 {Local "0"}
end

closure to_int32_0 {} [0] do
  1 <- pure [Local "0"];
  2 <- struct.select [Local "1"] 0;
  3 <- struct.select [Local "1"] 1;
  4 <- pure [Local "2"];
  int32.match [Local "4"]: |3| block.call to_int32_0 [] |4| block.call to_int32_1 [Local "3"]
end

block Unit [] do
  pure [Null]
end

block unit [] do
  int32.alloc 0
end

block List [] do
  closure.alloc List_0 {}
end

block null [] do
  closure.alloc null_0 {}
end

block cons [] do
  closure.alloc cons_0 {}
end

block fold_0 [3] do
  pure [Local "3"]
end

block fold_1 [7, 0, 1, 2, 3] do
  9 <- pure [Local "7"];
  10 <- struct.select [Local "9"] 0;
  11 <- struct.select [Local "9"] 1;
  12 <- block.call fold [];
  13 <- pure [Local "0"];
  14 <- closure.enter [Local "12"] [Local "13"];
  15 <- pure [Local "1"];
  16 <- closure.enter [Local "14"] [Local "15"];
  17 <- pure [Local "2"];
  18 <- closure.enter [Local "16"] [Local "17"];
  19 <- pure [Local "2"];
  20 <- pure [Local "3"];
  21 <- closure.enter [Local "19"] [Local "20"];
  22 <- pure [Local "10"];
  23 <- closure.enter [Local "21"] [Local "22"];
  24 <- closure.enter [Local "18"] [Local "23"];
  25 <- pure [Local "11"];
  closure.enter [Local "24"] [Local "25"]
end

block fold [] do
  closure.alloc fold_0 {}
end

block Nat [] do
  pure [Null]
end

block zero [] do
  0 <- int32.alloc 3;
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

block one [] do
  0 <- block.call succ [];
  1 <- block.call zero [];
  closure.enter [Local "0"] [Local "1"]
end

block two [] do
  0 <- block.call succ [];
  1 <- block.call one [];
  closure.enter [Local "0"] [Local "1"]
end

block three [] do
  0 <- block.call succ [];
  1 <- block.call two [];
  closure.enter [Local "0"] [Local "1"]
end

block nat_list [] do
  0 <- block.call cons [];
  1 <- block.call Nat [];
  2 <- closure.enter [Local "0"] [Local "1"];
  3 <- block.call one [];
  4 <- closure.enter [Local "2"] [Local "3"];
  5 <- block.call cons [];
  6 <- block.call Nat [];
  7 <- closure.enter [Local "5"] [Local "6"];
  8 <- block.call two [];
  9 <- closure.enter [Local "7"] [Local "8"];
  10 <- block.call cons [];
  11 <- block.call Nat [];
  12 <- closure.enter [Local "10"] [Local "11"];
  13 <- block.call three [];
  14 <- closure.enter [Local "12"] [Local "13"];
  15 <- block.call null [];
  16 <- block.call Nat [];
  17 <- closure.enter [Local "15"] [Local "16"];
  18 <- closure.enter [Local "14"] [Local "17"];
  19 <- closure.enter [Local "9"] [Local "18"];
  closure.enter [Local "4"] [Local "19"]
end

block folded_nat_list [] do
  0 <- block.call fold [];
  1 <- block.call Nat [];
  2 <- closure.enter [Local "0"] [Local "1"];
  3 <- block.call Nat [];
  4 <- closure.enter [Local "2"] [Local "3"];
  5 <- block.call add [];
  6 <- closure.enter [Local "4"] [Local "5"];
  7 <- block.call zero [];
  8 <- closure.enter [Local "6"] [Local "7"];
  9 <- block.call nat_list [];
  closure.enter [Local "8"] [Local "9"]
end

block to_int32_0 [] do
  int32.alloc 0
end

block to_int32_1 [3] do
  5 <- int32.alloc 1;
  6 <- block.call to_int32 [];
  7 <- pure [Local "3"];
  8 <- closure.enter [Local "6"] [Local "7"];
  int32.add [Local "5", Local "8"]
end

block to_int32 [] do
  closure.alloc to_int32_0 {}
end

block start [] do
  0 <- block.call to_int32 [];
  1 <- block.call folded_nat_list [];
  closure.enter [Local "0"] [Local "1"]
end
```