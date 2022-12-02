type hand = Rock | Paper | Scissors
type outcome = Win | Draw | Loss

let toHand = x =>
  switch x {
  | "A" | "X" => Rock
  | "B" | "Y" => Paper
  | "C" | "Z" => Scissors
  | _ => assert false
  }

let toResult = x =>
  switch x {
  | "X" => Loss
  | "Y" => Draw
  | "Z" => Win
  | _ => assert false
  }

let play = (x, y) =>
  switch (x, y) {
  | (Rock, Rock) => Draw
  | (Rock, Paper) => Win
  | (Rock, Scissors) => Loss
  | (Paper, Rock) => Loss
  | (Paper, Paper) => Draw
  | (Paper, Scissors) => Win
  | (Scissors, Rock) => Win
  | (Scissors, Paper) => Loss
  | (Scissors, Scissors) => Draw
  }

let toScore = (hand, play) => {
  let handScore = switch hand {
  | Rock => 1
  | Paper => 2
  | Scissors => 3
  }

  let playScore = switch play {
  | Win => 6
  | Draw => 3
  | Loss => 0
  }

  handScore + playScore
}

let toExpectedPlay = (hand, result) => {
  switch (hand, result) {
  | (Rock, Win) => Paper
  | (Rock, Loss) => Scissors
  | (Rock, Draw) => Rock
  | (Paper, Win) => Scissors
  | (Paper, Loss) => Rock
  | (Paper, Draw) => Paper
  | (Scissors, Win) => Rock
  | (Scissors, Loss) => Paper
  | (Scissors, Draw) => Scissors
  }
}

let part01 = input => {
  input->Js.String2.split("\n")->Js.Array2.reduce((acc, i) => {
    let plays = i->Js.String2.split(" ")->Js.Array2.map(toHand)

    switch plays {
    | [opponent, player] => {
        let outcome = play(opponent, player)
        acc + toScore(player, outcome)
      }

    | _ => acc
    }
  }, 0)
}

let part02 = input => {
  input->Js.String2.split("\n")->Js.Array2.reduce((acc, i) => {
    let plays = i->Js.String2.split(" ")

    switch plays {
    | [opponent, result] => {
        let opponentHand = toHand(opponent)
        let expectedResult = toResult(result)

        let expectedPlay = toExpectedPlay(opponentHand, expectedResult)

        acc + toScore(expectedPlay, expectedResult)
      }

    | _ => acc
    }
  }, 0)
}

Solution.make(part01, "day02/input")
Solution.make(part02, "day02/input")
