let traverse = (grid, fn) =>
  grid->Js.Array2.forEachi((line, y) =>
    line->Js.Array2.forEachi((tree, x) => fn(tree, (x, y), grid))
  )

let jooh = arr => arr->Js.Array2.reduce(((max, count), i) => {
    if i > max {
      (i, count + 1)
    } else {
      (max, count)
    }
  }, (0, 0))

let part01 = input => {
  let count = ref(0)

  let rec isHighest = (tree, (x, y), (dX, dY), grid) => {
    switch grid->Belt.Array.get(y + dY)->Belt.Option.getWithDefault([])->Belt.Array.get(x + dX) {
    | None => true
    | Some(otherTree) if tree > otherTree => isHighest(tree, (x + dX, y + dY), (dX, dY), grid)
    | Some(otherTree) if tree <= otherTree => false
    | _ => {
        Js.log(grid->Belt.Array.get(y + dY)->Belt.Option.getWithDefault([])->Belt.Array.get(x + dX))
        assert false
      }
    }
  }

  input
  ->Js.String2.split("\n")
  ->Js.Array2.map(line => line->Js.String2.split(""))
  ->traverse((tree, (x, y), grid) => {
    let highest =
      isHighest(tree, (x, y), (1, 0), grid) ||
      isHighest(tree, (x, y), (-1, 0), grid) ||
      isHighest(tree, (x, y), (0, 1), grid) ||
      isHighest(tree, (x, y), (0, -1), grid)

    if highest {
      count := count.contents + 1
    }
  })

  count
}

let part02 = input => {
  let count = ref(0)

  let rec isHighest = (tree, (x, y), (dX, dY), grid, score) => {
    switch grid->Belt.Array.get(y + dY)->Belt.Option.getWithDefault([])->Belt.Array.get(x + dX) {
    | None => score
    | Some(otherTree) if tree > otherTree =>
      isHighest(tree, (x + dX, y + dY), (dX, dY), grid, score + 1)
    | Some(otherTree) if tree <= otherTree => score + 1
    | _ => assert false
    }
  }

  input
  ->Js.String2.split("\n")
  ->Js.Array2.map(line => line->Js.String2.split(""))
  ->traverse((tree, (x, y), grid) => {
    let scenicScore =
      isHighest(tree, (x, y), (1, 0), grid, 0) *
      isHighest(tree, (x, y), (-1, 0), grid, 0) *
      isHighest(tree, (x, y), (0, 1), grid, 0) *
      isHighest(tree, (x, y), (0, -1), grid, 0)

    if scenicScore > count.contents {
      count := scenicScore
    }
  })

  count
}

Solution.make(part01, "day08/input")
Solution.make(part02, "day08/input")
