module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array exposing (..)
import Json.Decode exposing (..)
import Html.Keyed exposing (..)
import Debug exposing (..)


main =
    Html.beginnerProgram { model = model, view = view, update = update }



-- MODEL


type alias Todo =
    { text : String
    , isComplete : Bool
    , id : Int
    }


type alias Model =
    { todos : Array Todo
    , textInput : String
    , todosCount : Int
    }


todos =
    Array.empty


model : Model
model =
    Model todos "" 0



-- UPDATE


type Msg
    = AddTodo
    | Update String
    | ToggleTodo Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        AddTodo ->
            { model | todosCount = model.todosCount + 1, todos = push { text = model.textInput, isComplete = False, id = model.todosCount } model.todos, textInput = "" }

        Update text ->
            { model | textInput = text }

        ToggleTodo id ->
            let
                toggleTodo todo =
                    if todo.id == id then
                        { todo | isComplete = not todo.isComplete }
                    else
                        todo
            in
                { model | todos = Array.map toggleTodo model.todos }



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ input [ type_ "text", placeholder "Todo", onInput Update, onEnter AddTodo ] []
        , Html.Keyed.ul [] (List.map viewKeyedTodo (toList model.todos))
        ]


viewKeyedTodo : Todo -> ( String, Html Msg )
viewKeyedTodo todo =
    ( toString todo.id, viewTodo todo )


viewTodo : Todo -> Html Msg
viewTodo todo =
    li [ todoStyle todo.isComplete, onClick (ToggleTodo todo.id) ] [ text todo.text ]


todoStyle completed =
    if completed then
        style [ ( "text-decoration", "line-through" ) ]
    else
        style [ ( "text-decoration", "none" ) ]


onEnter : Msg -> Attribute Msg
onEnter msg =
    let
        isEnter code =
            if code == 13 then
                Json.Decode.succeed msg
            else
                Json.Decode.fail "not ENTER"
    in
        on "keydown" (Json.Decode.andThen isEnter keyCode)
