module Main exposing (..)

import Html exposing (Html, button, div, text, ul, li)
import Html.Attributes exposing (class)
import Html.Events exposing (onClick)
import List as List
import Platform.Cmd as Cmd
import Random as Random
import Random.List as RandomList


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { num : Int
    , text : String
    , deck : Deck
    }


type alias Deck =
    List Card


init : ( Model, Cmd Msg )
init =
    ( Model 0 "" deck, Cmd.none )


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



-- UPDATE


type Msg
    = Increment
    | Decrement
    | ShuffleDeck
    | NewDeck Deck


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Increment ->
            ( { model | num = model.num + 1 }, Cmd.none )

        Decrement ->
            ( { model | num = model.num - 1 }, Cmd.none )

        ShuffleDeck ->
            ( model, Random.generate NewDeck (RandomList.shuffle model.deck) )

        NewDeck newDeck ->
            ( { model | deck = newDeck }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


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
        [ div []
            [ deckView model.deck
            , button [ onClick ShuffleDeck ] [ text "Shuffle" ]
            ]
        , div []
            [ button [ onClick Decrement ] [ text "-" ]
            , div [] [ text (toString model.num) ]
            , button [ onClick Increment ] [ text "+" ]
            ]
        ]
