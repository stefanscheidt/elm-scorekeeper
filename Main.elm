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
    updatePlayer model id (\p -> { p | name = model.name })
        |> resetInput


score : Model -> Player -> Int -> Model
score model player points =
    updatePlayer model player.id (\p -> { p | points = p.points + points })


updatePlayer : Model -> Int -> (Player -> Player) -> Model
updatePlayer model id updateFn =
    let
        newPlayers =
            model.players
                |> List.map
                    (\p ->
                        if p.id == id then
                            updateFn p
                        else
                            p
                    )
    in
        { model | players = newPlayers }


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

        Score player points ->
            score model player points

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
    li []
        [ i [ class "edit", onClick (Edit player) ] []
        , div [] [ text player.name ]
        , button [ type_ "button", onClick (Score player 2) ] [ text "2pt" ]
        , button [ type_ "button", onClick (Score player 3) ] [ text "3pt" ]
        , div [] [ text (toString player.points) ]
        ]


playersList : Model -> Html Msg
playersList model =
    let
        total =
            List.map .points model.players
                |> List.sum
    in
        div []
            [ header []
                [ div [] [ text "Name" ]
                , div [] [ text "Ponts" ]
                ]
            , ul [] (List.map playerRow model.players)
            , footer []
                [ div [] [ text "Total: " ]
                , div [] [ text (toString total) ]
                ]
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
