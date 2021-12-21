port module Main exposing (main)

import Array
import Browser exposing (Document)
import Debug exposing (toString)
import Html exposing (Attribute, Html, a, button, canvas, code, div, h1, input, option, p, section, select, span, text, textarea)
import Html.Attributes exposing (class, classList, href, id, spellcheck, type_, value)
import Html.Events exposing (on, onClick, onInput)
import Json.Decode as Json
import List
import List.Extra exposing (greedyGroupsOf)
import Programs exposing (..)
import String


port cmd : ( String, Maybe Runtime ) -> Cmd msg


port unload : (String -> msg) -> Sub msg


port tick : (ProgramState -> msg) -> Sub msg


port output : (String -> msg) -> Sub msg


port breakpoint : (Int -> msg) -> Sub msg


type Msg
    = Run
    | Step
    | Continue
    | Pause
    | Tick ProgramState
    | Output String
    | SetDelay String
    | SetProgram String
    | EditorInput String
    | UpdateProgramInput String
    | Breakpoint Int


type alias Model =
    { program : String
    , input : String
    , output_ : Maybe String
    , memory : List Int
    , breakpoints : List Int
    , idx : Int
    , pointer : Int
    , steps : Int
    , delay : Int
    }


type alias ProgramState =
    { memory : List Int
    , idx : Int
    , pointer : Int
    , steps : Int
    }


type alias Runtime =
    { program : String
    , memory : List Int
    , breakpoints : List Int
    , input : String
    , idx : Int
    , pointer : Int
    , steps : Int
    , speed : Int
    }


main =
    let
        model =
            initialModel
        init : () -> (Model, Cmd Msg)
        init _ = ( model, cmd ( "init", Just (toRuntime model) ))
    in
        Browser.document
            { init = init
            , view = view
            , update = update
            , subscriptions = subscriptions
            }


initialModel : Model
initialModel =
    cleanState programHelloWorld


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ tick Tick
        , output Output
        , unload EditorInput
        , breakpoint Breakpoint
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update message model =
    case message of
        Run ->
            let
                breakpoints =
                    model.breakpoints

                input =
                    model.input

                clean =
                    cleanState model.program

                reset =
                    { clean
                        | delay = model.delay
                        , breakpoints = breakpoints
                        , input = input
                    }
            in
                ( reset, cmd ( "start", Just (toRuntime reset) ) )

        Step ->
            ( model, cmd ( "step", Just (toRuntime model) ) )

        Continue ->
            ( model, cmd ( "continue", Just (toRuntime model) ) )

        Pause ->
            ( model, cmd ( "pause", Just (toRuntime model) ) )

        Output addition ->
            let
                output_ =
                    Maybe.withDefault "" model.output_ ++ addition
            in
                ( { model | output_ = Just output_ }, Cmd.none )

        Tick state ->
            ( mergeState state model, Cmd.none )

        SetDelay raw ->
            let
                delay =
                    case String.toInt raw of
                        Nothing ->
                            50

                        Just val ->
                            val

                update_ =
                    { model | delay = delay }
            in
                ( update_, cmd ( "speed", Just (toRuntime update_) ) )

        SetProgram name ->
            let
                program =
                    programLoad name

                clean =
                    cleanState program

                update_ =
                    { clean | input = model.input }

                runtime =
                    toRuntime <| update_
            in
                ( update_, cmd ( "load", Just runtime ) )

        EditorInput program ->
            let
                clean =
                    cleanState program

                update_ =
                    { clean | input = model.input }
            in
                ( update_, Cmd.none )

        UpdateProgramInput input ->
            ( { model | input = input }, Cmd.none )

        Breakpoint pos ->
            let
                breakpoints =
                    if List.member pos model.breakpoints then
                        List.filter ((/=) pos) model.breakpoints
                    else
                        pos :: model.breakpoints
            in
                ( { model | breakpoints = breakpoints }, Cmd.none )


view : Model -> Document Msg
view model =
    { title = "Brainf🌲ck"
    , body= [div [ class "cf pa3 pa4-ns container helvetica" ]
        [ canvas [ id "canvas" ] []
        , editorTitle
        , div
            [ class "fl w-100 w-50-ns editor-section" ]
            [ section [] <| editorProgram model ]
        , div
            [ class "fl w-100 w-50-ns editor-section" ]
            [ section [] <| editorControls model
            , section [] <| editorMemory model
            , section [] <| editorOutput model
            , section [] <| editorInformation model
            ]
        ]
    ]
    }

mono : String -> Html Msg
mono str =
    code
        [ class "f6 ph1 tc bg-light-gray word-wrap" ]
        [ text str ]


link : String -> String -> Html Msg
link title shref =
    a
        [ href shref
        , class "link blue"
        ]
        [ text title ]


btn : List (Attribute msg) -> String -> Html msg
btn attrs val =
    button
        ([ class "mr2 mb2 pointer" ] ++ attrs)
        [ text val ]


lbl : String -> Html Msg
lbl txt =
    div
        [ class "f6 mb2 gray i" ]
        [ text txt ]


cleanState : String -> Model
cleanState program =
    { program = program
    , input = ""
    , output_ = Nothing
    , memory = []
    , breakpoints = []
    , idx = 0
    , pointer = 0
    , steps = 0
    , delay = 10
    }


toRuntime : Model -> Runtime
toRuntime { program, memory, breakpoints, input, idx, pointer, steps, delay } =
    { program = program
    , memory = memory
    , breakpoints = breakpoints
    , input = input
    , idx = idx
    , pointer = pointer
    , steps = steps
    , speed = 1000 * delay // 100
    }


mergeState : ProgramState -> Model -> Model
mergeState { memory, idx, pointer, steps } model =
    { model
        | memory = memory
        , idx = idx
        , pointer = pointer
        , steps = steps
    }


editorControls : Model -> List (Html Msg)
editorControls { delay } =
    let
        setProgram =
            Json.map (\s -> SetProgram s) <|
                Json.at [ "target", "value" ] Json.string
    in
        [ lbl "Wunschliste laden"
        , select
            [ class "w-50 mb3"
            , on "change" setProgram
            ]
            [ option
                []
                [ text "helloworld.bf" ]
            , option
                []
                [ text "cat-buffer.bf" ]
            , option
                []
                [ text "cat-stream.bf" ]
            , option
                []
                [ text "squares.bf" ]
            , option
                []
                [ text "fib.bf" ]
            , option
                []
                [ text "random.bf" ]
            ]
        , lbl ("Wartedauer nach jeder Aktion: " ++ toString (delay*10) ++ " ms")
        , input
            [ class "w-50 mb3"
            , type_ "range"
            , onInput SetDelay
            , value (toString delay)
            ]
            []
        , lbl "Kommandozentrale"
        , div
            [ class "mb2" ]
            [ btn [ onClick Run ] "Start"
            , btn [ onClick Pause ] "Anhalten"
            , btn [ onClick Step ] "Schritt"
            , btn [ onClick Continue ] "Fortsetzen"
            ]
        ]


editorOutput : Model -> List (Html Msg)
editorOutput model =
    let
        output_ =
            Maybe.withDefault "none" model.output_
    in
        [ div
            [ class "mt3" ]
            []
        , lbl "gespendete Geschenke"
        , div
            [ class "pb2 mb2" ]
            [ input
                [ class "w-50 f6 monospace"
                , id "readBuffer"
                , onInput UpdateProgramInput
                ]
                []
            ]
        , lbl "geklaute Geschenke"
        , div
            [ class "pb2 mb2 output" ]
            [ mono output_ ]
        ]


editorProgram : Model -> List (Html Msg)
editorProgram { program } =
    let
        getProgram =
            Json.map (\s -> EditorInput s) <|
                Json.at [ "target", "innerText" ] Json.string
    in
        [ textarea
            [ spellcheck False
            , class "editor"
            , on "keyup" getProgram
            ]
            [ text program ]
        ]


editorTitle : Html Msg
editorTitle =
    h1
        [ class "mt0 f3 f2-m f1-l title fw1 baskerville" ]
        [ text "Brainf🌲ck" ]


editorMemory : Model -> List (Html Msg)
editorMemory { memory, pointer } =
    let
        pageSize =
            10

        memSize =
            List.length memory

        padded =
            if memSize < pageSize then
                memory ++ List.repeat (pageSize - memSize) 0
            else
                memory

        isActive =
            \offset index ->
                pageSize * offset + index == pointer

        asCell =
            \offset index val ->
                div
                    [ class "memcell"
                    , classList [ ( "selected", isActive offset index ) ]
                    ]
                    [ span
                        []
                        [ text (toString val) ]
                    ]

        asRow =
            \index vals ->
                div [ class "cellrow" ]
                    (List.indexedMap (asCell index) vals)
    in
        [ lbl "Geschenkelager"
        , div [ class "mb3" ] <|
            List.indexedMap asRow <|
                greedyGroupsOf pageSize padded
        ]


editorInformation : Model -> List (Html Msg)
editorInformation { program, output_, idx, steps } =
    let
        isOptcode =
            \opt_ ->
                String.contains opt_ "+-[]<>,."

        opts =
            Array.filter isOptcode <|
                Array.fromList <|
                    String.split "" program

        opt =
            Maybe.withDefault " " <|
                Array.get idx opts

        programSize =
            String.length program

        codeSize =
            Array.length <| opts

        outputMessage =
            case output_ of
                Nothing ->
                    text "Du bist scheinbar ehrlich und hast noch keine Kekse geklaut."

                Just str ->
                    span
                        []
                        [ text "Du hast "
                        , mono <| toString <| String.length str
                        , text " Geschenke geklaut."
                        ]
    in
        [ p
            [ class "lh-copy" ]
            [ text "Deine Wunschliste hat "
            , mono <| toString programSize
            , text " Zeichen, darin enthalten sind "
            , mono <| toString codeSize
            , text " gültige Anweisungen. Die nächste Anweisung steht an Position "
            , mono <| toString idx
            , text " und lautet "
            , mono opt
            , text ". Bisher hast du "
            , mono <| toString steps
            , text " Aktionen durchgeführt. "
            , outputMessage
            ]
        ]