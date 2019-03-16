module Getto.SortTest exposing (..)
import Getto.Sort as Sort

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)

suite : Test
suite =
  describe "Sort"
    [ describe "by"
      [ test "should return Sort.Value of initial column, down order" <|
        \_ ->
          "id" |> Sort.by |> Sort.toString
          |> Expect.equal
            { column    = "id"
            , direction = "down"
            }
      ]
    , describe "fromString"
      [ test "should decode from ( column, 'down' )" <|
        \_ ->
          { column = Just "id", direction = Just "down" } |> Sort.fromString |> Maybe.map Sort.toString
          |> Expect.equal
            ( Just { column = "id", direction = "down" } )

      , test "should decode from ( column, 'up' )" <|
        \_ ->
          { column = Just "id", direction = Just "up" } |> Sort.fromString |> Maybe.map Sort.toString
          |> Expect.equal
            ( Just { column = "id", direction = "up" } )

      , test "should return Nothing from ( column, 'unknown-order' )" <|
        \_ ->
          { column = Just "id", direction = Just "unknown-order" } |> Sort.fromString |> Maybe.map Sort.toString
          |> Expect.equal
            Nothing

      , test "should return Nothing from ( column, Nothing )" <|
        \_ ->
          { column = Just "id", direction = Nothing } |> Sort.fromString
          |> Expect.equal
            Nothing

      , test "should return Nothing from ( Nothing, 'down' )" <|
        \_ ->
          { column = Nothing, direction = Just "down" } |> Sort.fromString
          |> Expect.equal
            Nothing
      ]
    , describe "stateOf"
      [ test "should return current order and next sort" <|
        \_ ->
          let
            {current,next} = "id" |> Sort.by |> Sort.stateOf "id"
          in
            ( current
            , next |> Sort.toString
            )
            |> Expect.equal
              ( Just Sort.Down
              , { column    = "id"
                , direction = "up"
                }
              )
      , test "should return Nothing and default sort with different target column" <|
        \_ ->
          let
            {current,next} = "id" |> Sort.by |> Sort.stateOf "other"
          in
            ( current
            , next |> Sort.toString
            )
            |> Expect.equal
              ( Nothing
              , { column    = "other"
                , direction = "down"
                }
              )
      ]
    ]
