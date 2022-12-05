@val external __dirname: string = "__dirname"

let input = Node.Path.resolve(__dirname, "stacks")->Node.Fs.readFileAsUtf8Sync
let moves = Node.Path.resolve(__dirname, "input")->Node.Fs.readFileAsUtf8Sync

let toStr = list => list->Belt.List.reduce("", (acc, i) => acc ++ i)

let stacks = input->Js.String2.split("\n")->Js.Array2.reduce((acc, line) => {
    line
    ->Js.String2.match_(%re("/(.{3})(?:\s|\n|$)/g"))
    ->Belt.Option.getExn
    ->Js.Array2.map(x => x->Belt.Option.getExn->Js.String2.replaceByRe(%re("/[\s|\[|\]]/g"), ""))
    ->Js.Array2.reducei((acc2, value, i) => {
      switch value {
      | "" => acc2
      | value => acc2->Js.Array2.mapi((val, j) => j == i ? val->Belt.List.add(value) : val)
      }
    }, acc)
  }, Belt.Array.make(9, list{}))->Js.Array2.map(list => list->Belt.List.reverse)

let int = str => str->Belt.Int.fromString->Belt.Option.getExn

let orders = moves->Js.String2.split("\n")->Js.Array2.reduce((acc, x) => {
    let order = x->Js.String2.match_(%re("/\d+/g"))

    switch order {
    | Some([Some(amount), Some(from), Some(to)]) => {
        let splits = acc[from->int - 1]->Belt.List.splitAt(amount->int)

        switch splits {
        | Some(head, tail) => {
            acc[from->int - 1] = tail
            acc[to->int - 1] = head->/* Belt.List.reverse-> */ Belt.List.concat(acc[to->int - 1])

            acc
          }

        | None => assert false
        }
      }

    | _ => assert false
    }
  }, stacks)

let result = orders->Js.Array2.reduce((acc, i) => {
  acc ++ i->Belt.List.head->Belt.Option.getWithDefault(" ")
}, "")

Js.log(result)
