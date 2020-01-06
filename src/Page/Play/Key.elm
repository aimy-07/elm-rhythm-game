module Page.Play.Key exposing (Key(..), KeyType(..), new, unwrap)

import Keyboard exposing (Key(..))


type Key
    = S
    | D
    | F
    | J
    | K
    | L


type KeyType
    = Character Key
    | Space
    | Invalid


new : Keyboard.RawKey -> KeyType
new rawKey =
    case Keyboard.anyKeyUpper rawKey of
        Just (Keyboard.Character keyStr) ->
            case keyStr of
                "S" ->
                    Character S

                "D" ->
                    Character D

                "F" ->
                    Character F

                "J" ->
                    Character J

                "K" ->
                    Character K

                "L" ->
                    Character L

                _ ->
                    Invalid

        Just Keyboard.Spacebar ->
            Space

        _ ->
            Invalid


unwrap : Key -> String
unwrap key =
    case key of
        S ->
            "S"

        D ->
            "D"

        F ->
            "F"

        J ->
            "J"

        K ->
            "K"

        L ->
            "L"
