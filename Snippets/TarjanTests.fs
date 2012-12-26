(*
Copyright 2012 Wesley Wiser

See the file License.txt for copying permission.

Based on tests located here: https://github.com/danielrbradley/CycleDetection/blob/master/StronglyConnectedComponentsTests/StronglyConnectedComponentTests.cs
*)
module TarjanTests

open Xunit
open TarjanAlgorithm

let areEqual<'a> (actual : 'a) (expected : 'a) = Assert.Equal(expected, actual)

type Node<'a> = { Item : 'a; mutable ConnectedTo : 'a list }

let getConnections nodes n = n.ConnectedTo |> Seq.map (fun connectedTo -> Seq.find (fun node -> node.Item = connectedTo) nodes)

let force s = s |> Seq.map Seq.toList |> Seq.toList

[<Fact>]
let EmptyGraph () = 
    let nodes : Node<int> seq = Seq.empty
    let expected : Node<int> list list = []

    let actual = force (tarjanAlgorithm nodes (getConnections nodes))

    areEqual expected actual

// A
[<Fact>]
let SingleVertex () =
    let nodes = { Item = 1; ConnectedTo = [] } |> Seq.singleton
    let expected : int Node list list = [nodes |> Seq.toList]

    let actual = force (tarjanAlgorithm nodes (getConnections nodes))

    areEqual expected actual

// A→B
[<Fact>]
let Linear2 () =
    let node2 = { Item = 2; ConnectedTo = [] }
    let node1 = { Item = 1; ConnectedTo = [2] }

    let nodes = seq { yield node1; yield node2 }
    let expected = [ [node2]; [node1] ]

    let actual = force (tarjanAlgorithm nodes (getConnections nodes))

    areEqual expected actual

// A→B→C
[<Fact>]
let Linear3 () =
    let node3 = { Item = 3; ConnectedTo = [] }
    let node2 = { Item = 2; ConnectedTo = [3] }
    let node1 = { Item = 1; ConnectedTo = [2] }

    let nodes = seq { yield node1; yield node2; yield node3 }
    let expected= [ [node3]; [node2]; [node1] ]

    let actual = force (tarjanAlgorithm nodes (getConnections nodes))

    areEqual expected actual

// A↔B
[<Fact>]
let Cycle2 () =
    let node1 = { Item = 1; ConnectedTo = [2] }
    let node2 = { Item = 2; ConnectedTo = [1] }

    let nodes = seq { yield node1; yield node2; }
    let expected = [ [node1; node2] ]

    let actual = force (tarjanAlgorithm nodes (getConnections nodes))

    areEqual expected actual

// A→B
// ↑ ↓
// └─C
[<Fact>]
let Cycle3 () =
    let node1 = { Item = 1; ConnectedTo = [2] }
    let node2 = { Item = 2; ConnectedTo = [3] }
    let node3 = { Item = 3; ConnectedTo = [1] }

    let nodes = seq { yield node1; yield node2; yield node3 }
    let expected = [ [node1; node2; node3] ]

    let actual = force (tarjanAlgorithm nodes (getConnections nodes))

    areEqual expected actual

// A→B   D→E
// ↑ ↓   ↑ ↓
// └─C   └─F
[<Fact>]
let TwoIsolated3Cycles () =
    let node1 = { Item = 1; ConnectedTo = [2] }
    let node2 = { Item = 2; ConnectedTo = [3] }
    let node3 = { Item = 3; ConnectedTo = [1] }

    let node4 = { Item = 4; ConnectedTo = [5] }
    let node5 = { Item = 5; ConnectedTo = [6] }
    let node6 = { Item = 6; ConnectedTo = [4] }

    let nodes = seq { yield node1; yield node2; yield node3; yield node4; yield node5; yield node6 }
    let expected = [ [node1; node2; node3]; [node4; node5; node6; ] ]

    let actual = force (tarjanAlgorithm nodes (getConnections nodes))

    areEqual expected actual

// A→B
// ↑ ↓
// └─C-→D
[<Fact>]
let Cycle3WithStub () =
    let node1 = { Item = 1; ConnectedTo = [2] }
    let node2 = { Item = 2; ConnectedTo = [3] }
    let node3 = { Item = 3; ConnectedTo = [1; 4] }
    let node4 = { Item = 4; ConnectedTo = [] }

    let nodes = seq { yield node1; yield node2; yield node3; yield node4; }
    let expected = [ [node4]; [node1; node2; node3;] ]

    let actual = force (tarjanAlgorithm nodes (getConnections nodes))

    areEqual expected actual