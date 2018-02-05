module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Json.Decode as JsonDecode
import List as List
import Maybe as Maybe
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
    { deck : Deck
    , players : Players
    , started : Bool
    , newPlayerName : String
    }


init : ( Model, Cmd Msg )
init =
    ( Model unshuffledDeck noPlayers unStarted blankName, Cmd.none )


unshuffledDeck : Deck
unshuffledDeck =
    [ Card Ambassador Exchange BlockSteal
    , Card Assassin Assassinate NoCounter
    , Card Captain Steal BlockSteal
    , Card Contessa NoAction BlockAssassination
    , Card Duke Take3 BlockForeignAid
    ]
        |> List.concatMap (List.repeat 3)


noPlayers : Players
noPlayers =
    []


unStarted : Bool
unStarted =
    False


blankName : String
blankName =
    ""


type alias Players =
    List Player


type alias Player =
    { name : String
    , coins : Int
    , cards : List Card -- TODO: make video about switching List to Map
    }


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
        StartGame shuffledDeck ->
            let
                { playersWithCards, remainingDeck } =
                    deal shuffledDeck model.players
            in
                ( { model
                    | started = True
                    , players = playersWithCards
                    , deck = remainingDeck
                  }
                , Cmd.none
                )

        AddPlayer ->
            ( { model
                | players =
                    ((createNewPlayer model.newPlayerName) :: model.players)
                , newPlayerName = ""
              }
            , Cmd.none
            )

        RemovePlayer player ->
            ( { model
                | players =
                    List.filter (\p -> not (p == player)) model.players

                -- TODO: all players with the same name will get removed
              }
            , Cmd.none
            )

        UpdateNewPlayerName str ->
            ( { model | newPlayerName = str }, Cmd.none )

        ShuffleDeckAndStartGame ->
            ( model, Random.generate StartGame (RandomList.shuffle model.deck) )


deal : Deck -> Players -> { playersWithCards : Players, remainingDeck : Deck }
deal cards players =
    dealCards cards players []


dealCards : Deck -> Players -> Players -> { playersWithCards : Players, remainingDeck : Deck }
dealCards deck playersWithoutCards playersWithCards =
    let
        cardsPerPlayer =
            2

        dealtPlayer =
            Maybe.withDefault (createNewPlayer "unknown") (List.head playersWithoutCards)

        remainingPlayers =
            List.drop 1 playersWithoutCards

        dealtCards =
            List.take cardsPerPlayer deck

        remainingDeck =
            List.drop cardsPerPlayer deck
    in
        if [] == playersWithoutCards then
            { playersWithCards = playersWithCards
            , remainingDeck = remainingDeck
            }
        else
            dealCards
                remainingDeck
                remainingPlayers
                ({ dealtPlayer | cards = dealtCards } :: playersWithCards)


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
        , startView (List.length model.players)
        , lobbyPlayersView model.players
        ]


lobbyInputView : String -> Html Msg
lobbyInputView name =
    div []
        [ input
            [ class "new-player"
            , placeholder "Who's playing?"
            , autofocus True
            , value name
            , onInput UpdateNewPlayerName
            , onEnter AddPlayer
            ]
            []
        ]


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
    div [ class "game" ]
        [ gamePlayersView model.players
        , deckView model.deck
        ]


gamePlayersView : Players -> Html Msg
gamePlayersView players =
    div [ class "game-players" ]
        [ h2 [] [ text "The Players" ]
        , ol [] (List.map gamePlayerView players)
        ]


gamePlayerView : Player -> Html Msg
gamePlayerView player =
    li [ class "game-player" ]
        [ text ((toString player.name) ++ "- $" ++ (toString player.coins))
        , cardsView player.cards
        ]


lobbyPlayersView : Players -> Html Msg
lobbyPlayersView players =
    div [ class "lobby-players" ]
        [ h2 [] [ text "The Players" ]
        , ol [] (List.map lobbyPlayerView players)
        ]


lobbyPlayerView : Player -> Html Msg
lobbyPlayerView player =
    li [ class "lobby-player" ]
        [ text (toString player.name)
        , button [ onClick (RemovePlayer player) ] [ text "x" ]
        ]


deckView : Deck -> Html Msg
deckView deck =
    div []
        [ h2 [] [ text "The Deck" ]
        , cardsView deck
        ]


cardsView : Deck -> Html Msg
cardsView cards =
    ol [] (List.map cardView cards)


cardView : Card -> Html Msg
cardView card =
    li [ class "card" ]
        [ text (toString card.name)
        ]
