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
    | Luck of Roll

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
    | Luck roll -> List.sum roll

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

[<Fact>]
let ``Given a roll [1,2,3,3,2] and a score box Luck, return result`` () =
    result (Luck [1;2;3;3;2]) |> should equal 11

open System.Collections.Generic

type ScoreBoard = Map<ScoreBox, int>

type Player = { Name: string; ScoreBoard: ScoreBoard }

let player = { Name = "Joe"; ScoreBoard = Map.empty }

let score result scoreBox player =
    if player.ScoreBoard |> Map.exists (fun x y -> x = scoreBox)
    then
        failwith "cannot score twice"
    else
        let newScoreBoard = player.ScoreBoard.Add(scoreBox, result scoreBox)
        { player with ScoreBoard = newScoreBoard }

[<Fact>]
let ``Given a Player, a Roll and a ScoreBox, when score, return player with ScoreBoard updated`` () =
    let scoreBox = LargeStraight [1;2;3;4;5]
    let result scoreBox = 40
    let expectedScoreBoard = Map.empty.Add(scoreBox, 40)
    player |> score result scoreBox
    |> should equal { player with ScoreBoard = expectedScoreBoard }

[<Fact>]
let ``Given a Player, a Roll and a LargeStraight ScoreBox, when score, raise error 'cannot score twice on a score box''`` () =
    let scoreBox = LargeStraight [1;2;3;4;5]
    let result scoreBox = 40
    let alreadyPlayedScoreBox = Map.empty.Add(scoreBox, 40)
    ( fun () -> { player with ScoreBoard = alreadyPlayedScoreBox }
                |> score result scoreBox |> ignore )
    |> should throw typeof<System.Exception>

let rec play player =
    // TODO roll (Random) => Roll
    let roll = [1;2;3;4;5]
    // TODO ask player which ScoreBox for this roll OR reroll
    let scoreBox = LargeStraight roll

    let playerNextTurn = player |> score result scoreBox
    if playerNextTurn.ScoreBoard |> Map.toList |> List.length = 13
    then
        playerNextTurn.ScoreBoard |> Map.toList |> List.map snd |> List.sum
    else
        playerNextTurn |> play
