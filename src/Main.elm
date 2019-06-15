module Main exposing (Model, Msg, init, main)

import Browser exposing (Document)
import Browser.Dom as Dom exposing (focus)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (onClick, onInput)
import Html.Events.Extra.Touch as Touch
import Html.Parser as Parser exposing (Node(..))
import Http
import Json.Decode as Decode
import List.Extra
import ParsingUtil exposing (toVirtualDomWrapWords)
import Ports exposing (..)
import Regex exposing (Regex)
import Regexes exposing (..)
import Task



-- MAIN


main =
    Browser.document
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


urlBase : String
urlBase =
    "http://rueo.ru/sercxo"


getWords : String -> Cmd Msg
getWords query =
    Cmd.batch
        [ Http.request
            { method = "get"
            , url = urlBase ++ "/?ajax&term=" ++ query
            , expect = Http.expectJson (GotWords query) decodeItems
            , body = Http.emptyBody
            , tracker = Just "getWords"
            , headers = []
            , timeout = Nothing
            }

        -- команды начиинают исполнение с конца списка
        , Http.cancel "getWords"
        ]


decodeWordLabel : Decode.Decoder String
decodeWordLabel =
    Decode.field "value" Decode.string


decodeItems : Decode.Decoder (List String)
decodeItems =
    Decode.list decodeWordLabel


getArticle : String -> Model -> Cmd Msg
getArticle word model =
    Cmd.batch
        [ Http.request
            { method = "get"
            , url = urlBase ++ "/" ++ word
            , expect = Http.expectString <| GotArticle word
            , body = Http.emptyBody
            , tracker = Just "getArticle"
            , headers = []
            , timeout = Nothing
            }
        , saveHistory <| List.Extra.unique (word :: model.history)

        -- команды начиинают исполнение с конца списка
        , Http.cancel "getArticle"
        ]


cancelRequests : Cmd Msg
cancelRequests =
    Cmd.batch
        [ Http.cancel "getArticle"
        , Http.cancel "getWords"
        ]


type Stage
    = Failure
    | Loading
    | Initial


type alias State =
    { suggest : Stage
    , article : Stage
    }


type alias ClientRect =
    { width : Float
    , height : Float
    }


type alias Model =
    { state : State
    , input : String
    , lastSuccessfulSearch : String
    , article : Maybe Article
    , suggest : Maybe (List String)
    , history : List String
    , localHistory : List String
    , currentIndex : Int
    , startCoords : ( Float, Float )
    , clientRect : ClientRect
    }


type alias Flags =
    { history : List String
    , clientRect : ClientRect
    }


initialState : State
initialState =
    { article = Initial, suggest = Initial }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { state = initialState
      , input = ""
      , lastSuccessfulSearch = ""
      , article = Nothing
      , suggest = Nothing
      , history = flags.history
      , localHistory = []
      , currentIndex = 0
      , startCoords = ( 0, 0 )
      , clientRect = flags.clientRect
      }
    , Cmd.none
    )


type alias Article =
    String


type alias GotWordsResult =
    List String


type Direction
    = Prev
    | Next


type Msg
    = Idle
    | Input String
    | Clear
    | SelectWord String
    | GotWords String (Result Http.Error GotWordsResult)
    | GotArticle String (Result Http.Error Article)
    | SetHistory (List String)
    | NavigateHistory Direction
    | TouchStartAt ( Float, Float )
    | TouchMoveAt ( Float, Float )
    | TouchEndAt ( Float, Float )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWords word result ->
            processGotWordsResponse word model result

        Input str ->
            processInput model str

        Clear ->
            ( { model
                | input = ""
                , article = Nothing
                , suggest = Nothing
                , state = initialState
                , lastSuccessfulSearch = ""
              }
            , cancelRequests
            )

        SelectWord word ->
            processWordSelection model word

        GotArticle word result ->
            processGotArticleResponse word model result

        SetHistory history ->
            ( { model | history = history }, Cmd.none )

        Idle ->
            ( model, Cmd.none )

        NavigateHistory direction ->
            navigate direction model

        TouchStartAt coords ->
            ( { model | startCoords = coords }, Cmd.none )

        TouchMoveAt _ ->
            ( model, Cmd.none )

        TouchEndAt coords ->
            ( { model | startCoords = coords }, Cmd.none )


view : Model -> Document Msg
view model =
    Document "Dictionary"
        [ rootElement model
            [ leftColumn model
            , middleColumn model
            , rightColumn model
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ updateHistory SetHistory
        , searchKeyPressed Input
        , backspaceKeyPressed (\_ -> Input (trimLast model.input))
        ]


trimLast : String -> String
trimLast str =
    String.dropRight 1 str


navigate : Direction -> Model -> ( Model, Cmd Msg )
navigate direction model =
    case direction of
        Prev ->
            ( { model | currentIndex = nextIndex model }
            , (takeHistoryAt (nextIndex model) model |> getArticle) <| model
            )

        Next ->
            ( { model | currentIndex = prevIndex model }
            , (takeHistoryAt (prevIndex model) model |> getArticle) <| model
            )


nextIndex : Model -> Int
nextIndex model =
    Basics.min (model.currentIndex + 1) (List.length model.localHistory)


prevIndex : Model -> Int
prevIndex model =
    Basics.max (model.currentIndex - 1) 0


takeHistoryAt : Int -> Model -> String
takeHistoryAt at model =
    Maybe.withDefault "" <| List.head <| List.drop at model.localHistory


processGotWordsResponse : String -> Model -> Result Http.Error GotWordsResult -> ( Model, Cmd Msg )
processGotWordsResponse word model result =
    case result of
        Ok words ->
            if List.length words == 0 && word /= "" then
                ( { model | state = updateSuggestState Failure model }
                , getWords (String.dropRight 1 word)
                )

            else
                ( { model
                    | suggest = Just (List.take 10 words)
                    , state = updateSuggestState Initial model
                    , lastSuccessfulSearch = word
                  }
                , Cmd.none
                )

        _ ->
            ( { model | state = updateSuggestState Failure model }
            , Cmd.none
            )


updateSuggestState : Stage -> Model -> State
updateSuggestState stage { state } =
    { state | suggest = stage }


processInput : Model -> String -> ( Model, Cmd Msg )
processInput model str =
    if str == "" then
        ( { model
            | input = str
            , state = initialState
            , suggest = Nothing
            , lastSuccessfulSearch = str
          }
        , cancelRequests
        )

    else
        ( { model | input = str, state = updateSuggestState Loading model }
        , Cmd.batch
            [ Task.attempt (always Idle) <| Dom.focus "search"
            , getWords str
            ]
        )


processWordSelection : Model -> String -> ( Model, Cmd Msg )
processWordSelection model word =
    ( { model
        | input = word
        , localHistory = word :: List.drop model.currentIndex model.localHistory
        , state = updateArticleState Loading model
        , suggest = Nothing
        , currentIndex = 0
      }
    , getArticle word model
    )


updateArticleState : Stage -> Model -> State
updateArticleState stage { state } =
    { state | article = stage }


processGotArticleResponse : String -> Model -> Result Http.Error Article -> ( Model, Cmd Msg )
processGotArticleResponse word model result =
    case result of
        Ok article ->
            if Regex.contains articleNotFoundRegex article then
                ( { model
                    | article = Nothing
                    , state = updateArticleState Failure model
                  }
                , getWords word
                )

            else
                ( { model
                    | article = Just article
                    , state = updateArticleState Initial model
                    , lastSuccessfulSearch = word
                  }
                , Cmd.none
                )

        _ ->
            ( { model
                | article = Nothing
                , state = updateArticleState Failure model
              }
            , getWords word
            )



-- Views


leftColumn : Model -> Html Msg
leftColumn model =
    div [ class "left" ] []


rightColumn : Model -> Html Msg
rightColumn model =
    div [ class "right" ]
        [ historyView model
        ]


middleColumn : Model -> Html Msg
middleColumn model =
    div [ class "middle" ]
        [ searchField model
        , wordsList model
        , articleView model
        ]


rootElement : Model -> List (Html Msg) -> Html Msg
rootElement model children =
    div
        [ onTouch "start" <| TouchStartAt << touchCoordinates
        , onTouch "end" <| processTouchEnd model << touchCoordinates
        , class "root"
        ]
        children


searchField : Model -> Html Msg
searchField model =
    let
        isValid =
            model.lastSuccessfulSearch == model.input || model.state.suggest == Loading
    in
    div [ class "search" ]
        [ input
            [ type_ "text"
            , onInput Input
            , value model.input
            , placeholder "Поиск..."
            , id "search"
            , propagationlessKeyPress
            , autocomplete False
            , spellcheck False
            , classList [ ( "search__field", True ), ( "search__field_error", not isValid ) ]
            ]
            []
        , clearButton model
        , localHistoryView model
        ]


clearButton : Model -> Html Msg
clearButton model =
    button [ onClick Clear ] [ text "X" ]


wordsList : Model -> Html Msg
wordsList model =
    case model.suggest of
        Just suggest ->
            div [] (List.map makeWordOption suggest)

        Nothing ->
            div [] []


historyView : Model -> Html Msg
historyView model =
    div
        [ onWordClick SelectWord ]
        (List.map clickableWordView model.history)


localHistoryView : Model -> Html Msg
localHistoryView model =
    div []
        [ button
            [ onClick <| NavigateHistory Prev
            , disabled <| model.currentIndex >= List.length model.localHistory - 1
            ]
            [ text "<" ]
        , button
            [ onClick <| NavigateHistory Next
            , disabled <| model.currentIndex <= 0
            ]
            [ text ">" ]
        ]


clickableWordView : String -> Html Msg
clickableWordView word =
    div [ class "clickable" ] [ text word ]


articleView : Model -> Html Msg
articleView model =
    case model.article of
        Just article ->
            prepareArticle article

        Nothing ->
            div [] []


makeWordOption : String -> Html Msg
makeWordOption name =
    div [ onWordOptionSelect ] [ text name ]



-- Результаты поиска в HTML


findSearchResultInList : List Node -> Node
findSearchResultInList nodes =
    Maybe.withDefault (Text "") (List.foldl lazyNodeFind Nothing nodes)


lazyNodeFind : Node -> Maybe Node -> Maybe Node
lazyNodeFind node m =
    case m of
        Nothing ->
            findSearchResult node

        Just n ->
            Just n


findSearchResult : Node -> Maybe Node
findSearchResult node =
    case node of
        Element _ attrs children ->
            if containsSearchResultClassName attrs then
                Just node

            else
                List.foldl lazyNodeFind Nothing children

        Text _ ->
            Nothing

        Comment _ ->
            Nothing


stripKom : Node -> Node
stripKom node =
    case node of
        Element name attrs children ->
            if containsKomClassName attrs then
                Comment ""
                -- Comment используем вместо Text, так как Text делает лишнюю обертку в DOM дереве

            else
                Element name attrs <| List.map stripKom children

        _ ->
            node


containsSearchResultClassName : List ( String, String ) -> Bool
containsSearchResultClassName attributes =
    List.any (\( name, val ) -> name == "class" && val == "search_result") attributes


containsKomClassName : List ( String, String ) -> Bool
containsKomClassName attributes =
    List.any (\( name, val ) -> name == "class" && val == "kom") attributes



-- Форматирование статьи


reducers : List (String -> String)
reducers =
    [ removeDoctype, removeTrailingDiv, removeScripts ]


prepareArticle : Article -> Html Msg
prepareArticle article =
    List.foldl (<|) article reducers |> parseHtml


removeScripts : String -> String
removeScripts html =
    Regex.replace scriptTagRegex (always "") html


removeTrailingDiv : String -> String
removeTrailingDiv html =
    Regex.replace trailingDivRegex (always "") html


removeDoctype : String -> String
removeDoctype html =
    if Regex.contains doctypeRegex html then
        List.foldr (++) "" (String.split "\n" html |> List.drop 1)

    else
        html


parseHtml : String -> Html Msg
parseHtml html =
    Parser.run html |> processResult


processResult : Result a (List Node) -> Html Msg
processResult nodes =
    case nodes of
        Ok tree ->
            div [ onWordClick SelectWord ] (toVirtualDomWrapWords [ stripKom <| findSearchResultInList tree ])

        _ ->
            text "Error"



-- Events


onWordClick : (String -> Msg) -> Html.Attribute Msg
onWordClick strToMsg =
    Events.on "click" <|
        Decode.map (pairToMsg strToMsg) <|
            decodeClassTextPair


decodeClassName : Decode.Decoder String
decodeClassName =
    Decode.at [ "target", "className" ] Decode.string


decodeInnerText : Decode.Decoder String
decodeInnerText =
    Decode.at [ "target", "innerText" ] Decode.string


decodeClassTextPair : Decode.Decoder ( String, String )
decodeClassTextPair =
    Decode.map2 Tuple.pair decodeClassName decodeInnerText


pairToMsg : (String -> Msg) -> ( String, String ) -> Msg
pairToMsg strToMsg ( className, innerText ) =
    case className of
        "clickable" ->
            processClickableWord innerText |> strToMsg

        _ ->
            Idle


processClickableWord : String -> String
processClickableWord word =
    Regex.replace stressRegex (always "") word


onWordOptionSelect : Html.Attribute Msg
onWordOptionSelect =
    Events.on "click" <|
        Decode.map SelectWord <|
            Decode.at [ "target", "innerText" ] Decode.string


propagationlessKeyPress : Html.Attribute Msg
propagationlessKeyPress =
    Events.stopPropagationOn "keypress" <| Decode.succeed ( Idle, True )


onTouch : String -> (Touch.Event -> Msg) -> Html.Attribute Msg
onTouch name =
    { stopPropagation = False, preventDefault = False }
        |> Touch.onWithOptions ("touch" ++ name)



-- Utils


processTouchEnd : Model -> ( Float, Float ) -> Msg
processTouchEnd model ( x, y ) =
    let
        ( xs, _ ) =
            model.startCoords
    in
    if xs >= model.clientRect.width * 0.4 && xs <= model.clientRect.width * 0.6 then
        processTouchNavigation model ( x, y )

    else
        Idle


processTouchNavigation : Model -> ( Float, Float ) -> Msg
processTouchNavigation model ( x, y ) =
    let
        ( xs, ys ) =
            model.startCoords
    in
    if vectorLength ( x - xs, y - ys ) >= 50 && ((x - xs) / abs (y - ys)) >= 3 then
        NavigateHistory Prev

    else if vectorLength ( x - xs, y - ys ) >= 50 && ((x - xs) / abs (y - ys)) <= -3 then
        NavigateHistory Next

    else
        Idle


vectorLength : ( Float, Float ) -> Float
vectorLength ( x, y ) =
    sqrt <| x ^ 2 + y ^ 2


touchCoordinates : Touch.Event -> ( Float, Float )
touchCoordinates touchEvent =
    List.head touchEvent.changedTouches
        |> Maybe.map .clientPos
        |> Maybe.withDefault ( 0, 0 )
