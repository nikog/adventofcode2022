let part01 = input => {
  let max = input->Js.String2.split("\n\n")->Js.Array2.reduce((acc, i) => {
      let rows =
        i
        ->Js.String2.split("\n")
        ->Js.Array2.reduce(
          (acc, i) => acc + Belt.Int.fromString(i)->Belt.Option.getWithDefault(0),
          0,
        )

      Js.Math.max_int(acc, rows)
    }, 0)

  max
}

let sum = x => x->Js.Array2.reduce((acc, i) => acc + i, 0)

let part02 = input => {
  let top3 =
    input
    ->Js.String2.split("\n\n")
    ->Js.Array2.map(i =>
      i
      ->Js.String2.split("\n")
      ->Js.Array2.reduce((acc, i) => acc + Belt.Int.fromString(i)->Belt.Option.getWithDefault(0), 0)
    )
    ->Belt.SortArray.Int.stableSort

  sum(top3->Belt.Array.slice(~len=3, ~offset=-3))
}

Solution.make(part02, "day01/input")
