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
    model
        |> updatePlayer id (\player -> { player | name = model.name })
        |> updatePlays id (\play -> { play | name = model.name })
        |> resetInput


score : Model -> Player -> Int -> Model
score model player points =
    model
        |> updatePlayer player.id (\p -> { p | points = p.points + points })
        |> addPlay player points


addPlay : Player -> Int -> Model -> Model
addPlay player points model =
    let
        newPlay =
            { id = (List.length model.plays) + 1
            , playerId = player.id
            , name = player.name
            , score = points
            }
    in
        { model | plays = newPlay :: model.plays }


deletePlay : Model -> Play -> Model
deletePlay model play =
    let
        updatePlayerFn =
            (\player -> { player | points = player.points - play.score })

        newPlays =
            List.filter (\p -> p.id /= play.id) model.plays
    in
        model
            |> updatePlayer play.playerId updatePlayerFn
            |> (\m -> { m | plays = newPlays })


updatePlayer : Int -> (Player -> Player) -> Model -> Model
updatePlayer id updateFn model =
    let
        newPlayers =
            model.players
                |> List.map
                    (\player ->
                        if player.id == id then
                            updateFn player
                        else
                            player
                    )
    in
        { model | players = newPlayers }


updatePlays : Int -> (Play -> Play) -> Model -> Model
updatePlays playerId updateFn model =
    let
        newPlays =
            model.plays
                |> List.map
                    (\play ->
                        if play.playerId == playerId then
                            updateFn play
                        else
                            play
                    )
    in
        { model | plays = newPlays }


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

        DeletePlay play ->
            deletePlay model play


view : Model -> Html Msg
view model =
    div [ class "scoreboard" ]
        [ h1 [] [ text "Score Keeper" ]
        , playersList model
        , playerForm model
        , playsList model
        , p [] [ toString model |> text ]
        ]


playerRow : Model -> Player -> Html Msg
playerRow model player =
    li []
        [ i [ class "edit", onClick (Edit player) ] []
        , div [ class (playerNameStyle model player) ] [ text player.name ]
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
            , ul [] (model.players |> List.sortBy .name |> List.map (playerRow model))
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
            , class (playerInputStyle model)
            , placeholder "Add/Edit Player"
            , onInput Input
            , value model.name
            , autofocus True
            ]
            []
        , button [ type_ "submit", disabled (model.name == "") ] [ text "Save" ]
        , button [ type_ "button", disabled (model.name == ""), onClick Cancel ] [ text "Cancel" ]
        ]


playRow : Play -> Html Msg
playRow play =
    li []
        [ i [ class "remove", onClick (DeletePlay play) ] []
        , div [] [ text play.name ]
        , div [] [ text (toString play.score) ]
        ]


playsList : Model -> Html Msg
playsList model =
    div []
        [ header []
            [ div [] [ text "Name" ]
            , div [] [ text "Score" ]
            ]
        , ul [] (List.map playRow model.plays)
        ]


playerNameStyle : Model -> Player -> String
playerNameStyle model player =
    case model.playerId of
        Just id ->
            if id == player.id then
                "edit"
            else
                ""

        Nothing ->
            ""


playerInputStyle : Model -> String
playerInputStyle model =
    case model.playerId of
        Just id ->
            "edit"

        Nothing ->
            ""


main : Program Never Model Msg
main =
    Html.beginnerProgram { model = initialModel, view = view, update = update }
