module StateTest exposing (..)

import Expect exposing (Expectation)
import Test exposing (..)

import State exposing (..)

pickHead : State (List Int) Int
pickHead =
  get |> andThen (\l -> case l of
    [] -> Debug.todo "No such element"
    x::xs ->
      put xs |> andThen (\_ ->
      return x))

sumHeads : State (List Int) Int
sumHeads =
  pickHead |> andThen (\x ->
  pickHead |> andThen (\y ->
  return (x + y)))

suite : Test
suite =
    describe "State"
    [ test "return" <| \_ ->
        State.return 1
          |> State.run [1, 2, 3]
          |> Expect.equal ( 1, [1, 2, 3] )

    , test "get" <| \_ ->
        State.get
          |> State.run [1, 2, 3]
          |> Expect.equal ( [1, 2, 3], [1, 2, 3] )

    , test "put" <| \_ ->
        State.put [4, 5]
          |> State.run [1, 2, 3]
          |> Expect.equal ( (), [4, 5] )

    , test "combined" <| \_ ->
        sumHeads
          |> State.run [1, 1, 1, 1, 1]
          |> Expect.equal ( 2, [1, 1, 1] )
    ]