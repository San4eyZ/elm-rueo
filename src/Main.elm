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
            , expect = Http.expectJson (GotSuggest query) decodeItems
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


type alias MobileView =
    { historyVisible : Bool
    , menuVisible : Bool
    }


type alias Model =
    { state : State
    , input : String
    , lastSuccessfulSearch : String
    , article : Maybe (Html Msg)
    , suggest : Maybe (List String)
    , history : List String
    , localHistory : List String
    , currentHistoryIndex : Int
    , currentSuggestIndex : Int
    , startCoords : ( Float, Float )
    , clientRect : ClientRect
    , mobileView : MobileView
    }


type alias Flags =
    { history : List String
    , clientRect : ClientRect
    }


initialState : State
initialState =
    { article = Initial, suggest = Initial }


initialMobileView : MobileView
initialMobileView =
    { historyVisible = False, menuVisible = False }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( { state = initialState
      , input = ""
      , lastSuccessfulSearch = ""
      , article = Nothing
      , suggest = Nothing
      , history = flags.history
      , localHistory = []
      , currentHistoryIndex = 0
      , currentSuggestIndex = -1
      , startCoords = ( 0, 0 )
      , clientRect = flags.clientRect
      , mobileView = initialMobileView
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
    | ClearAll
    | ClearSuggest
    | SelectWord String
    | GotSuggest String (Result Http.Error GotWordsResult)
    | GotArticle String (Result Http.Error Article)
    | SetHistory (List String)
    | NavigateHistory Direction
    | NavigateSuggest Direction
    | TouchStartAt ( Float, Float )
    | TouchMoveAt ( Float, Float )
    | TouchEndAt ( Float, Float )
    | ToggleMenu
    | ToggleHistory
    | InitView


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotSuggest word result ->
            processGotSuggestResponse word model result

        Input str ->
            processInput model str

        ClearAll ->
            ( { model
                | input = ""
                , article = Nothing
                , suggest = Nothing
                , state = initialState
                , lastSuccessfulSearch = ""
              }
            , cancelRequests
            )

        ClearSuggest ->
            ( { model | suggest = Nothing }, Cmd.none )

        SelectWord word ->
            processWordSelection model word

        GotArticle word result ->
            processGotArticleResponse word model result

        SetHistory history ->
            ( { model | history = history }, Cmd.none )

        Idle ->
            ( model, Cmd.none )

        NavigateHistory direction ->
            navigateHistory direction model

        NavigateSuggest direction ->
            navigateSuggest direction model

        TouchStartAt coords ->
            ( { model | startCoords = coords }, Cmd.none )

        TouchMoveAt _ ->
            ( model, Cmd.none )

        TouchEndAt coords ->
            ( { model | startCoords = coords }, Cmd.none )

        ToggleMenu ->
            toggleMenu model

        ToggleHistory ->
            toggleHistory model

        InitView ->
            ( { model | mobileView = initialMobileView }, Cmd.none )


toggleMenu : Model -> ( Model, Cmd Msg )
toggleMenu model =
    let
        newMobileView =
            { historyVisible = model.mobileView.historyVisible
            , menuVisible = not model.mobileView.menuVisible
            }
    in
    ( { model | mobileView = newMobileView }, Cmd.none )


toggleHistory : Model -> ( Model, Cmd Msg )
toggleHistory model =
    let
        newMobileView =
            { historyVisible = not model.mobileView.historyVisible
            , menuVisible = model.mobileView.menuVisible
            }
    in
    ( { model | mobileView = newMobileView }, Cmd.none )


view : Model -> Document Msg
view model =
    Document "Dictionary"
        [ rootElement model
            [ leftColumn model
            , middleColumn model
            , rightColumn model
            , overlay model
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


navigateHistory : Direction -> Model -> ( Model, Cmd Msg )
navigateHistory direction model =
    case direction of
        Prev ->
            let
                word =
                    takeHistoryAt (nextIndex History model) model
            in
            ( { model | currentHistoryIndex = nextIndex History model, input = word, suggest = Nothing }
            , getArticle word model
            )

        Next ->
            let
                word =
                    takeHistoryAt (prevIndex History model) model
            in
            ( { model | currentHistoryIndex = prevIndex History model, input = word, suggest = Nothing }
            , getArticle word model
            )


navigateSuggest : Direction -> Model -> ( Model, Cmd Msg )
navigateSuggest direction model =
    if model.suggest == Nothing then
        ( model, Cmd.none )

    else
        case direction of
            Prev ->
                let
                    index =
                        prevIndex Suggest model
                in
                ( { model | currentSuggestIndex = index, input = takeSuggestAt index model }, Cmd.none )

            Next ->
                let
                    index =
                        nextIndex Suggest model
                in
                ( { model | currentSuggestIndex = index, input = takeSuggestAt index model }, Cmd.none )


type NavigationType
    = Suggest
    | History


nextIndex : NavigationType -> Model -> Int
nextIndex navType model =
    case navType of
        Suggest ->
            Basics.min (model.currentSuggestIndex + 1) (List.length (Maybe.withDefault [] model.suggest) - 1)

        History ->
            Basics.min (model.currentHistoryIndex + 1) (List.length model.localHistory - 1)


prevIndex : NavigationType -> Model -> Int
prevIndex navType model =
    case navType of
        Suggest ->
            Basics.max (model.currentSuggestIndex - 1) 0

        option2 ->
            Basics.max (model.currentHistoryIndex - 1) 0


takeHistoryAt : Int -> Model -> String
takeHistoryAt at model =
    Maybe.withDefault "" <| List.head <| List.drop at model.localHistory


takeSuggestAt : Int -> Model -> String
takeSuggestAt at model =
    Maybe.withDefault "" <| List.head <| List.drop at (Maybe.withDefault [] model.suggest)


processGotSuggestResponse : String -> Model -> Result Http.Error GotWordsResult -> ( Model, Cmd Msg )
processGotSuggestResponse word model result =
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
            , currentSuggestIndex = -1
          }
        , cancelRequests
        )

    else
        ( { model
            | input = str
            , state = updateSuggestState Loading model
            , currentSuggestIndex = -1
          }
        , Cmd.batch
            [ Task.attempt (always Idle) <| Dom.focus "search"
            , getWords str
            ]
        )


processWordSelection : Model -> String -> ( Model, Cmd Msg )
processWordSelection model word =
    ( { model
        | input = word
        , localHistory = word :: List.drop model.currentHistoryIndex model.localHistory
        , state = updateArticleState Loading model
        , suggest = Nothing
        , currentHistoryIndex = 0
        , currentSuggestIndex = -1
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
                    | article = Just <| prepareArticle article
                    , state = updateArticleState Initial model
                    , lastSuccessfulSearch = word
                  }
                , saveHistory <| List.Extra.unique (word :: model.history)
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
    div [ classList  [("left", True), ("left_active", model.mobileView.menuVisible)] ] []


rightColumn : Model -> Html Msg
rightColumn model =
    div [ classList [("right", True), ("right_active", model.mobileView.historyVisible)] ]
        [ historyView model
        ]


middleColumn : Model -> Html Msg
middleColumn model =
    div [ class "middle" ]
        [ searchField model
        , articleView model
        ]


overlay : Model -> Html Msg
overlay model =
    if model.mobileView.historyVisible || model.mobileView.menuVisible then
        div [ class "overlay", onClick InitView ] []

    else
        text ""


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
            model.lastSuccessfulSearch == model.input || model.state.suggest == Loading || model.currentSuggestIndex /= -1
    in
    div [ class "search" ]
        [ menuButton model
        , input
            [ type_ "text"
            , onInput Input
            , onArrowDown
            , onEnterPress model.input
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
        , suggestView model
        , historyButton model
        ]


clearButton : Model -> Html Msg
clearButton model =
    button [ onClick ClearAll, class "button", class "button-clear" ] [ text "X" ]


suggestView : Model -> Html Msg
suggestView model =
    case model.suggest of
        Just suggest ->
            div [ class "suggest" ] (List.indexedMap (makeWordOption model) suggest)

        Nothing ->
            text ""


historyButton : Model -> Html Msg
historyButton model =
    button [ class "button", class "button-history", onClick ToggleHistory ] [ text "H" ]


menuButton : Model -> Html Msg
menuButton model =
    button [ class "button", class "button-menu", onClick ToggleMenu ] [ text "M" ]


makeWordOption : Model -> Int -> String -> Html Msg
makeWordOption model index name =
    let
        isActive =
            model.currentSuggestIndex == index
    in
    div
        [ onWordOptionSelect
        , class "clickable"
        , classList [ ( "suggest__item", True ), ( "suggest__item_active", isActive ) ]
        ]
        [ text name ]


historyView : Model -> Html Msg
historyView model =
    div
        [ onWordClick SelectWord, class "history" ]
        (historyHeadingView :: List.map historyItemView model.history)


historyHeadingView : Html Msg
historyHeadingView =
    h2 [ class "history__heading" ] [ text "История" ]


localHistoryView : Model -> Html Msg
localHistoryView model =
    div [ class "navigation-buttons" ]
        [ button
            [ onClick <| NavigateHistory Prev
            , disabled <| model.currentHistoryIndex >= List.length model.localHistory - 1
            , class "button"
            , class "button-prev"
            ]
            [ text "<" ]
        , button
            [ onClick <| NavigateHistory Next
            , disabled <| model.currentHistoryIndex <= 0
            , class "button"
            , class "button-next"
            ]
            [ text ">" ]
        ]


historyItemView : String -> Html Msg
historyItemView word =
    div [ class "clickable", class "history__item" ] [ text word ]


articleView : Model -> Html Msg
articleView model =
    case model.article of
        Just article ->
            article

        Nothing ->
            div [] []



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
    [ removeDoctype, removeScripts, removeTrailingDiv ]


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


onArrowDown : Html.Attribute Msg
onArrowDown =
    Events.on "keydown" (keyDownDecoder arrowKeyCodeToMsg)


onEnterPress : String -> Html.Attribute Msg
onEnterPress input =
    Events.on "keyup" (keyDownDecoder <| enterKeyCodeToMsg input)


keyDownDecoder : (Int -> Msg) -> Decode.Decoder Msg
keyDownDecoder codeToMsg =
    Decode.map codeToMsg Events.keyCode


arrowKeyCodeToMsg : Int -> Msg
arrowKeyCodeToMsg code =
    case code of
        38 ->
            NavigateSuggest Prev

        40 ->
            NavigateSuggest Next

        _ ->
            Idle


enterKeyCodeToMsg : String -> Int -> Msg
enterKeyCodeToMsg input code =
    case code of
        13 ->
            SelectWord input

        _ ->
            Idle


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
    if String.contains "clickable" className then
        processClickableWord innerText |> strToMsg

    else
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
