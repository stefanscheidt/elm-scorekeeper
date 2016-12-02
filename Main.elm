module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Player =
    { id : Int
    , name : String
    , points : Int
    }


type alias Play =
    { id : Int
    , playerId : Int
    , name : String
    , score : Int
    }


type alias Model =
    { players : List Player
    , playerId : Maybe Int
    , name : String
    , plays : List Play
    }


initialModel : Model
initialModel =
    { players = []
    , playerId = Nothing
    , name = ""
    , plays = []
    }


type Msg
    = Edit Player
    | Score Player Int
    | Input String
    | Save
    | Cancel
    | DeletePlay Play


resetInput : Model -> Model
resetInput model =
    { model | name = "", playerId = Nothing }


save : Model -> Model
save model =
    case model.playerId of
        Just id ->
            edit model id

        Nothing ->
            add model


add : Model -> Model
add model =
    let
        player =
            { id = (List.length model.players) + 1
            , name = model.name
            , points = 0
            }
    in
        { model | players = player :: model.players }
            |> resetInput


edit : Model -> Int -> Model
edit model id =
    let
        newPlayers =
            List.map
                (\player ->
                    if player.id == id then
                        { player | name = model.name }
                    else
                        player
                )
                model.players
    in
        { model | players = newPlayers }
            |> resetInput


update : Msg -> Model -> Model
update msg model =
    case msg of
        Edit player ->
            { model | playerId = Just player.id, name = player.name }

        Input name ->
            { model | name = name }

        Save ->
            if model.name /= "" then
                save model
            else
                model

        Cancel ->
            model
                |> resetInput

        _ ->
            model


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playersList model
        , playerForm model
        , p [] [ toString model |> text ]
        ]


playerRow : Player -> Html Msg
playerRow player =
    div []
        [ button [ type_ "button", onClick (Edit player) ] [ text "Edit" ]
        , span [] [ text player.name ]
        ]


playersList : Model -> Html Msg
playersList model =
    div []
        [ li [] (List.map playerRow model.players)
        ]


playerForm : Model -> Html Msg
playerForm model =
    Html.form [ onSubmit Save ]
        [ input
            [ type_ "text"
            , placeholder "Add/Edit Player"
            , onInput Input
            , value model.name
            ]
            []
        , button [ type_ "submit", disabled (model.name == "") ] [ text "Save" ]
        , button [ type_ "button", disabled (model.name == ""), onClick Cancel ] [ text "Cancel" ]
        ]


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }
