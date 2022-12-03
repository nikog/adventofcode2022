let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"

let splitInHalf = str => {
  let len = str->Js.String2.length
  let asArray = str->Js.String2.split("")

  (
    asArray->Belt.Array.slice(~offset=0, ~len=len / 2)->Belt.Set.String.fromArray,
    asArray->Belt.Array.slice(~offset=len / 2, ~len=len / 2)->Belt.Set.String.fromArray,
  )
}

let toScore = char => chars->Js.String2.indexOf(char) + 1

let toStrSet = str => str->Js.String2.split("")->Belt.Set.String.fromArray

let rec scoreByGroup = list => {
  switch list {
  | list{fst, snd, thd, ...tail} => {
      let score =
        fst
        ->toStrSet
        ->Belt.Set.String.intersect(snd->toStrSet)
        ->Belt.Set.String.intersect(thd->toStrSet)
        ->Belt.Set.String.reduce(0, (acc, i) => acc + toScore(i))

      score + scoreByGroup(tail)
    }

  | _ => 0
  }
}

let part01 = input => input->Js.String2.split("\n")->Js.Array2.reduce((acc, i) => {
    let (firstHalf, secondHalf) = splitInHalf(i)
    let duplicates = Belt.Set.String.intersect(firstHalf, secondHalf)
    let score = duplicates->Belt.Set.String.reduce(0, (acc, i) => acc + toScore(i))

    acc + score
  }, 0)

let part02 = input => input->Js.String2.split("\n")->Belt.List.fromArray->scoreByGroup

Solution.make(part01, "day03/input")
Solution.make(part02, "day03/input")
