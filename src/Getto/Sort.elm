module Getto.Sort exposing
  ( Value
  , Direction(..)
  , by
  , stateOf
  , toString
  , fromString
  )

type Value = Value
  { column    : String
  , direction : Direction
  }
type Direction
  = Up
  | Down

by : String -> Value
by column = Down |> init column

init : String -> Direction -> Value
init column direction = Value
  { column    = column
  , direction = direction
  }

stateOf : String -> Value -> { current : Maybe Direction, next : Value }
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


toString : Value -> { column : String, direction : String }
toString (Value model) =
  { column    = model.column
  , direction =
    case model.direction of
      Up   -> "up"
      Down -> "down"
  }

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
