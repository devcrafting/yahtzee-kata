module Yam

open Xunit
open FsUnit.Xunit

type Roll = int list

type ScoreBox =
    | Ones of Roll
    | Twos of Roll
    | Threes of Roll
    | Fours of Roll
    | Fives of Roll
    | Six of Roll
    | ThreeOfAKind of Roll
    | FourOfAKind of Roll
    | Full of Roll
    | LargeStraight of Roll
    | SmallStraight of Roll

let sum value inRoll =
    inRoll
        |> List.filter (fun x -> x = value)
        |> List.sum

let sumIfAtLeast nbOfAKind inRoll =
    if inRoll
        |> Seq.groupBy id
        |> Seq.exists (fun (x, y) -> Seq.length y > nbOfAKind - 1)
    then inRoll  |> List.sum
    else 0

let straight length wonPoints inRoll =
    let orderedDistinctValue = inRoll |> Seq.sortBy id |> Seq.distinct
    let shift =
        [(inRoll |> Seq.max) + 1]
        |> Seq.append (orderedDistinctValue |> Seq.skip 1)
    if Seq.length orderedDistinctValue = length
        && Seq.zip orderedDistinctValue shift
            |> Seq.forall (fun (x, y) -> y = x + 1)
    then wonPoints
    else 0

let result scoreBox =
    match scoreBox with
    | Ones roll -> sum 1 roll
    | Twos roll -> sum 2 roll
    | Threes roll -> sum 3 roll
    | Fours roll -> sum 4 roll
    | Fives roll -> sum 5 roll
    | Six roll -> sum 6 roll
    | ThreeOfAKind roll -> sumIfAtLeast 3 roll
    | FourOfAKind roll -> sumIfAtLeast 4 roll
    | Full roll ->
        let groupByValue = roll |> Seq.groupBy id
        if groupByValue
            |> Seq.forall (fun (x, y) -> Seq.length y = 3 || Seq.length y = 2)
        then 25
        else
            if groupByValue |> Seq.length = 1
            then 50
            else 0
    | LargeStraight roll ->
        straight 5 40 roll
    | SmallStraight roll ->
        straight 4 30 roll

[<Fact>]
let ``Given a roll [5,5,5,5,5] and a score box, return result`` () =
    result (Fives [5;5;5;5;5]) |> should equal 25

[<Fact>]
let ``Given a roll [4,5,5,5,5] and a score box, return result`` () =
    result (Fives [4;5;5;5;5]) |> should equal 20

[<Fact>]
let ``Given a roll [1,2,3,4,6] and a score box, return result`` () =
    result (Ones [1;2;3;4;6]) |> should equal 1

[<Fact>]
let ``Given a roll [1,2,3,4,6] and a score box Twos, return result`` () =
    result (Twos [1;2;3;4;6]) |> should equal 2

[<Fact>]
let ``Given a roll [1,2,3,4,6] and a score box Threes, return result`` () =
    result (Threes [1;2;3;4;6]) |> should equal 3

[<Fact>]
let ``Given a roll [1,2,3,4,6] and a score box Fours, return result`` () =
    result (Fours [1;2;3;4;6]) |> should equal 4

[<Fact>]
let ``Given a roll [1,2,3,4,6] and a score box Six, return result`` () =
    result (Six [1;2;3;4;6]) |> should equal 6

[<Fact>]
let ``Given a roll [4,5,5,5,5] and a score box ThreeOfAKind, return result`` () =
    result (ThreeOfAKind [4;5;5;5;5]) |> should equal 24

[<Fact>]
let ``Given a roll [4,2,3,5,5] and a score box ThreeOfAKind, return result`` () =
    result (ThreeOfAKind [4;2;3;5;5]) |> should equal 0

[<Fact>]
let ``Given a roll [3,5,5,5,5] and a score box FourOfAKind, return result`` () =
    result (FourOfAKind [3;5;5;5;5]) |> should equal 23

[<Fact>]
let ``Given a roll [3,3,5,5,5] and a score box FourOfAKind, return result`` () =
    result (Full [3;3;5;5;5]) |> should equal 25

[<Fact>]
let ``Given a roll [3,4,5,5,5] and a score box FourOfAKind, return result`` () =
    result (Full [3;4;5;5;5]) |> should equal 0

[<Fact>]
let ``Given a roll [5,5,5,5,5] and a score box FourOfAKind, return result`` () =
    result (Full [5;5;5;5;5]) |> should equal 50

[<Fact>]
let ``Given a roll [1,2,3,5,6] and a score box FourOfAKind, return result`` () =
    result (LargeStraight [1;2;3;5;6]) |> should equal 0

[<Fact>]
let ``Given a roll [1,5,3,4,2] and a score box FourOfAKind, return result`` () =
    result (LargeStraight [1;5;3;4;2]) |> should equal 40

[<Fact>]
let ``Given a roll [1,2,3,4,2] and a score box FourOfAKind, return result`` () =
    result (SmallStraight [1;2;3;4;2]) |> should equal 30

[<Fact>]
let ``Given a roll [1,2,3,3,2] and a score box FourOfAKind, return result`` () =
    result (SmallStraight [1;2;3;3;2]) |> should equal 0
