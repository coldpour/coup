module Main exposing (..)

import Html exposing (Html, button, div, text, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List as List


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = model
        , view = view
        , update = update
        }


type Msg
    = Increment
    | Decrement


type alias Model =
    { num : Int
    , text : String
    , deck : Deck
    }


type alias Deck =
    List Card


deck : Deck
deck =
    [ Card Ambassador Exchange BlockSteal
    , Card Assassin Assassinate NoCounter
    , Card Captain Steal BlockSteal
    , Card Contessa NoAction BlockAssassination
    , Card Duke Take3 BlockForeignAid
    ]
        |> List.concatMap (List.repeat 3)


type alias Card =
    { name : Name
    , action : Action
    , counter : Counter
    }


type Name
    = Ambassador
    | Assassin
    | Captain
    | Contessa
    | Duke


type Action
    = Assassinate
    | Exchange
    | ForeignAid
    | Income
    | NoAction
    | Steal
    | Take3


type Counter
    = BlockAssassination
    | BlockForeignAid
    | BlockSteal
    | NoCounter


model : Model
model =
    Model 0 "" deck


update : Msg -> Model -> Model
update msg model =
    case msg of
        Increment ->
            { model | num = model.num + 1 }

        Decrement ->
            { model | num = model.num - 1 }


deckView : Deck -> Html Msg
deckView deck =
    ul []
        (List.map
            cardView
            deck
        )


cardView : Card -> Html Msg
cardView card =
    li [ class "card" ]
        [ text (toString card.name)
        ]


view : Model -> Html Msg
view model =
    div []
        [ div [] [ deckView model.deck ]
        , div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [] [ text (toString model.num) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        ]
