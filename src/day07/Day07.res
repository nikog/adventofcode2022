let part01 = input => {
  let path = ref([])
  let dirs = ref(Belt.Map.String.empty)

  let pathToStr = path => path->Js.Array2.joinWith("/")
  let int = str => str->Belt.Int.fromString->Belt.Option.getExn
  let min = arr => Js.Math.minMany_int(arr)

  let _ =
    input
    ->Js.String2.split("\n")
    ->Js.Array2.map(line => {
      let parts = line->Js.String2.split(" ")

      switch parts {
      | [_, cmd, args] if cmd == "cd" && args == "/" => path := [""]
      | [_, cmd, args] if cmd == "cd" && args == ".." =>
        let _ = path.contents->Js.Array2.pop
      | [_, cmd, args] if cmd == "cd" =>
        let _ = path.contents->Js.Array2.push(args)
      | [_, cmd] if cmd == "ls" => ()
      | [dir, _] if dir == "dir" => ()
      | [size, _] => {
          let stack = path.contents->Belt.Array.copy
          path.contents->Js.Array2.forEach(_ => {
            let key = stack->pathToStr
            let dirVal = dirs.contents->Belt.Map.String.get(key)->Belt.Option.getWithDefault(0)
            dirs := dirs.contents->Belt.Map.String.set(key, size->int + dirVal)
            let _ = stack->Js.Array2.pop
          })
        }

      | _ => assert false
      }
    })

  dirs.contents->Belt.Map.String.toArray->Js.Array2.reduce((acc, i) => {
    let (_, val) = i
    acc + (val <= 100000 ? val : 0)
  }, 0)
}

let part02 = input => {
  let path = ref([])
  let dirs = ref(Belt.Map.String.empty)

  let pathToStr = path => path->Js.Array2.joinWith("/")
  let int = str => str->Belt.Int.fromString->Belt.Option.getExn
  let min = arr => Js.Math.minMany_int(arr)

  let _ =
    input
    ->Js.String2.split("\n")
    ->Js.Array2.map(line => {
      let parts = line->Js.String2.split(" ")

      switch parts {
      | [_, cmd, args] if cmd == "cd" && args == "/" => path := [""]
      | [_, cmd, args] if cmd == "cd" && args == ".." =>
        let _ = path.contents->Js.Array2.pop
      | [_, cmd, args] if cmd == "cd" =>
        let _ = path.contents->Js.Array2.push(args)
      | [_, cmd] if cmd == "ls" => ()
      | [dir, _] if dir == "dir" => ()
      | [size, _] => {
          let stack = path.contents->Belt.Array.copy
          path.contents->Js.Array2.forEach(_ => {
            let key = stack->pathToStr
            let dirVal = dirs.contents->Belt.Map.String.get(key)->Belt.Option.getWithDefault(0)
            dirs := dirs.contents->Belt.Map.String.set(key, size->int + dirVal)
            let _ = stack->Js.Array2.pop
          })
        }

      | _ => assert false
      }
    })

  let root = dirs.contents->Belt.Map.String.get("")->Belt.Option.getExn
  let freeSpace = 70000000 - root
  let neededSpace = 30000000 - freeSpace

  dirs.contents
  ->Belt.Map.String.toArray
  ->Js.Array2.map(i => {
    let (_, val) = i
    val
  })
  ->Js.Array2.filter(i => {
    i >= neededSpace
  })
  ->min
}

Solution.make(part01, "day07/input")
Solution.make(part02, "day07/input")
