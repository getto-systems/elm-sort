module Getto.Sort exposing
  ( Value
  , Direction(..)
  , State
  , by
  , stateOf
  , toString
  , fromString
  )

{-| sort utilities for html table

    import Html as H exposing ( Html )
    import Html.Attributes as A
    import Html.Events as E

    type Msg
      = SortBy Sort.Value

    sort = "id" |> Sort.by

    sort |> Sort.stateOf "id"   |> link "id"
    sort |> Sort.stateOf "name" |> link "name"

    link : String -> Sort.State -> Html Msg
    link text {current,next} =
      H.a
        ( List.append
          [ "#" |> A.href
          , next |> SortBy |> E.onClick
          ]
          ( case current of
            Nothing -> []
            Just _  -> [ "is-active" |> A.class ]
          )
        )
        [ text |> H.text
        , " "  |> H.text
        , case current of
            Nothing        -> ""     |> H.text
            Just Sort.Up   -> "up"   |> H.text
            Just Sort.Down -> "down" |> H.text
        ]

# Definition
@docs Value, Direction, State

# Construction
@docs by

# State
@docs stateOf

# Encode/Decode
@docs toString, fromString
 -}


{-| sort definition
 -}
type Value = Value
  { column    : String
  , direction : Direction
  }


{-| sort direction
 -}
type Direction
  = Up
  | Down


{-| current sort direction and next sort definition
 -}
type alias State =
  { current : Maybe Direction
  , next    : Value
  }


{-| create Value that sort by "column"
 -}
by : String -> Value
by column = Down |> init column

init : String -> Direction -> Value
init column direction = Value
  { column    = column
  , direction = direction
  }


{-| get current sort state from value and "column"
 -}
stateOf : String -> Value -> State
stateOf column (Value value) =
  if column /= value.column
    then { current = Nothing, next = column |> by }
    else
      { current = Just value.direction
      , next    = Value { value | direction = value.direction |> invert }
      }

invert : Direction -> Direction
invert direction =
  case direction of
    Up -> Down
    Down -> Up


{-| encode value to string

    "id" |> Sort.by |> Sort.toString
    -- { column = "id", direction = "down" }
 -}
toString : Value -> { column : String, direction : String }
toString (Value model) =
  { column    = model.column
  , direction =
    case model.direction of
      Up   -> "up"
      Down -> "down"
  }


{-| decode value from string

    { column    = Just "id"
    , direction = Just "up"
    }
    |> Sort.fromString
 -}
fromString : { column : Maybe String, direction : Maybe String } -> Maybe Value
fromString {column,direction} =
  case (column,direction) of
    ( Just col, Just dir ) -> dir |> decodeDirection |> Maybe.map (init col)
    _ -> Nothing

decodeDirection : String -> Maybe Direction
decodeDirection direction =
  case direction |> String.toLower of
    "up"   -> Just Up
    "down" -> Just Down
    _ -> Nothing
