module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JsonDecode
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
    , players : Players
    , started : Bool
    , newPlayerName : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model 0 "" deck [] False "", Cmd.none )


type State
    = Open
    | Closed


type alias Players =
    List Player


type alias Player =
    { name : String
    , coins : Int
    , cards : List Card
    }


deck : Deck
deck =
    [ Card Ambassador Exchange BlockSteal
    , Card Assassin Assassinate NoCounter
    , Card Captain Steal BlockSteal
    , Card Contessa NoAction BlockAssassination
    , Card Duke Take3 BlockForeignAid
    ]
        |> List.concatMap (List.repeat 3)


type alias Deck =
    List Card


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
    = StartGame Deck
    | ShuffleDeckAndStartGame
    | AddPlayer
    | RemovePlayer Player
    | UpdateNewPlayerName String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        StartGame newDeck ->
            ( { model
                | started = True
                , deck = newDeck
              }
            , Cmd.none
            )

        AddPlayer ->
            ( { model
                | players = ((createNewPlayer model.newPlayerName) :: model.players)
                , newPlayerName = ""
              }
            , Cmd.none
            )

        RemovePlayer player ->
            ( { model
                | players =
                    List.filter (\p -> not (p == player)) model.players
              }
            , Cmd.none
            )

        UpdateNewPlayerName str ->
            ( { model | newPlayerName = str }, Cmd.none )

        ShuffleDeckAndStartGame ->
            ( model, Random.generate StartGame (RandomList.shuffle model.deck) )


createNewPlayer : String -> Player
createNewPlayer name =
    Player name 2 []



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    if model.started then
        gameView model
    else
        lobbyView model


lobbyView : Model -> Html Msg
lobbyView model =
    div [ class "lobby" ]
        [ h2 [] [ text "Welcome to Coup!" ]
        , lobbyInputView model.newPlayerName
        , playersView model.players
        , startView (List.length model.players)
        ]


lobbyInputView : String -> Html Msg
lobbyInputView name =
    input
        [ class "new-player"
        , placeholder "Who's playing?"
        , autofocus True
        , value name
        , onInput UpdateNewPlayerName
        , onEnter AddPlayer
        ]
        []


startView : Int -> Html Msg
startView numPlayers =
    if 1 < numPlayers && numPlayers <= 5 then
        button [ onClick ShuffleDeckAndStartGame ] [ text "Start" ]
    else
        text "Need 2 to 5 players to start a game."


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                JsonDecode.succeed msg
            else
                JsonDecode.fail "not ENTER"
    in
        on "keydown" (JsonDecode.andThen isEnter keyCode)


gameView : Model -> Html Msg
gameView model =
    div []
        [ playersView model.players
        , deckView model.deck
        ]


playersView : Players -> Html Msg
playersView players =
    div []
        [ h2 [] [ text "The Players" ]
        , ul [] (List.map playerView players)
        ]


playerView : Player -> Html Msg
playerView player =
    li [ class "player" ]
        [ text (toString player.name)
        , button [ onClick (RemovePlayer player) ] [ text "x" ]
        ]


deckView : Deck -> Html Msg
deckView deck =
    div []
        [ h2 [] [ text "The Deck" ]
        , ul [] (List.map cardView deck)
        , button [ onClick ShuffleDeckAndStartGame ] [ text "Shuffle" ]
        ]


cardView : Card -> Html Msg
cardView card =
    li [ class "card" ]
        [ text (toString card.name)
        ]
