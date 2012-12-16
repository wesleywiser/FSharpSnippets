module TarjanAlgorithm

type Vertex<'a> = { Item: 'a; mutable Index: int option; mutable Lowlink : int option }

let tarjanAlgorithm (V : 'a seq) (E : 'b seq) (getConnectedVerticies : ('a -> 'a seq)) : ('a seq seq) =
    let V = Map(seq { for v in V -> (v, { Item = v; Index = None; Lowlink = None }) })

    let index = ref 0
    let s = ref [] //one way linked lists are the same as stacks

    let rec strongconnect (v : 'a Vertex) : ('a seq) =
        v.Index <- Some !index
        v.Lowlink <- Some !index
        index := !index + 1
        s := v.Item :: !s //push v.Item onto the stack (list)

        for w in (getConnectedVerticies v.Item) |> Seq.map (fun w -> V.[w]) do
            if w.Index.IsNone then 
                //Successor w has not yet been visited; recurse on it
                strongconnect w |> ignore
                v.Lowlink <- Some (min v.Lowlink.Value w.Lowlink.Value)
            else if List.exists ((=) w.Item) !s then
                //Successor w is in stack S and hence in the current SCC
                v.Lowlink <- Some (min v.Lowlink.Value w.Index.Value)
        
        //if v is a root node, pop the stack and generate an SCC
        if v.Lowlink.Value = v.Index.Value then
            let rec generateScc rest work =
                match rest with
                | head :: tail when head = v.Item -> (tail, head :: work)
                | head :: tail -> generateScc tail (head :: work)
                | [] -> failwith "Invalid state."
            
            let (stack, scc) = generateScc !s []
            s := stack

            seq { for vertex in scc -> vertex }
        else Seq.empty

    seq {
        for (_,v) in (V |> Map.toSeq) do
            yield strongconnect v
    }