let part01 = input =>
  input
  ->Js.String2.split("\n")
  ->Js.Array2.map(x =>
    x
    ->Js.String2.split(",")
    ->Js.Array2.map(y =>
      y->Js.String2.split("-")->Js.Array2.map(str => str->Belt.Int.fromString->Belt.Option.getExn)
    )
  )
  ->Js.Array2.reduce((acc, i) => {
    switch i {
    | [[a1, a2], [b1, b2]] if a1 >= b1 && a2 <= b2 => acc + 1
    | [[a1, a2], [b1, b2]] if b1 >= a1 && b2 <= a2 => acc + 1
    | _ => acc
    }
  }, 0)

let part02 = input =>
  input
  ->Js.String2.split("\n")
  ->Js.Array2.map(x =>
    x
    ->Js.String2.split(",")
    ->Js.Array2.map(y =>
      y->Js.String2.split("-")->Js.Array2.map(str => str->Belt.Int.fromString->Belt.Option.getExn)
    )
  )
  ->Js.Array2.reduce((acc, i) => {
    switch i {
    | [[a1, _], [b1, b2]] if a1 >= b1 && a1 <= b2 => acc + 1
    | [[a1, a2], [b1, _]] if b1 >= a1 && b1 <= a2 => acc + 1
    | [[_, a2], [_, b2]] if a2 <= b2 && a2 >= b2 => acc + 1
    | [[_, a2], [_, b2]] if b2 <= a2 && b2 >= a2 => acc + 1
    | _ => acc
    }
  }, 0)

Solution.make(part01, "day04/input")
Solution.make(part02, "day04/input")
