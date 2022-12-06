let cmplen = (arr1, arr2) => arr1->Belt.Array.length === arr2->Belt.Array.length
let uniq = arr => arr->Belt.Set.String.fromArray->Belt.Set.String.toArray->cmplen(arr)

let rec process = (~count=0, characters, len) => {
  let isUnique = characters->Js.Array2.slice(~start=0, ~end_=len)->uniq

  switch isUnique {
  | true => len + count
  | false => process(characters->Belt.Array.sliceToEnd(1), len, ~count=count + 1)
  }
}

let part01 = input => input->Js.String2.split("")->process(4)
let part02 = input => input->Js.String2.split("")->process(14)

Solution.make(part01, "day06/input")
Solution.make(part02, "day06/input")
