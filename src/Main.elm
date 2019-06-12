module Main exposing (Model, Msg, init, main)

import Browser exposing (Document)
import Browser.Dom as Dom exposing (focus)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (onClick, onInput)
import Html.Parser as Parser exposing (Node(..))
import Http
import Json.Decode as Decode
import Json.Encode as Encode
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
    Http.get
        { url = urlBase ++ "/?ajax&term=" ++ query
        , expect = Http.expectJson GotWords decodeItems
        }


decodeWordLabel : Decode.Decoder String
decodeWordLabel =
    Decode.field "value" Decode.string


decodeItems : Decode.Decoder (List String)
decodeItems =
    Decode.list decodeWordLabel


getArticle : String -> Model -> Cmd Msg
getArticle word model =
    Cmd.batch
        [ Http.get
            { url = urlBase ++ "/" ++ word
            , expect = Http.expectString <| GotArticle word
            }
        , saveHistory <| List.Extra.unique (word :: model.history)
        ]


type State
    = Failure
    | Loading
    | Initial
    | Success (List String)


type alias Model =
    { state : State
    , input : String
    , article : Maybe Article
    , history : List String
    , localHistory : List String
    , currentIndex : Int
    }


init : List String -> ( Model, Cmd Msg )
init history =
    ( Model Initial "" Nothing history [] 0
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
    | SelectWord String
    | GotWords (Result Http.Error GotWordsResult)
    | GotArticle String (Result Http.Error Article)
    | SetHistory (List String)
    | NavigateHistory Direction


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWords result ->
            processGotWordsResponse model result

        Input str ->
            processInput model str

        SelectWord str ->
            ( { model | input = str, localHistory = str :: model.localHistory }, getArticle str model )

        GotArticle word result ->
            processGotArticleResponse word model result

        SetHistory history ->
            ( { model | history = history }, Cmd.none )

        Idle ->
            ( model, Cmd.none )

        NavigateHistory direction ->
            navigate direction model


view : Model -> Document Msg
view model =
    Document "Dictionary"
        [ div []
            [ searchField model
            , loader model
            , errorBlock model
            , wordsList model
            , articleView model
            , historyView model
            , localHistoryView model
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
            ( { model | currentIndex = model.currentIndex + 1 }
            , (takeHistoryAt (model.currentIndex + 1) model |> getArticle) <| model
            )

        Next ->
            ( { model | currentIndex = model.currentIndex - 1 }
            , (takeHistoryAt (model.currentIndex - 1) model |> getArticle) <| model
            )


takeHistoryAt : Int -> Model -> String
takeHistoryAt at model =
    Maybe.withDefault "" <| List.head <| List.drop at model.localHistory


processGotWordsResponse : Model -> Result Http.Error GotWordsResult -> ( Model, Cmd Msg )
processGotWordsResponse model result =
    case result of
        Ok words ->
            ( { model | state = Success (List.take 10 words) }, Cmd.none )

        _ ->
            ( { model | state = Failure }, Cmd.none )


processInput : Model -> String -> ( Model, Cmd Msg )
processInput model str =
    if str == "" then
        ( { model | input = str, state = Initial }, Cmd.none )

    else
        ( { model | input = str, state = Loading }
        , Cmd.batch
            [ Task.attempt (\_ -> Idle) <| Dom.focus "search"
            , getWords str
            ]
        )


processGotArticleResponse : String -> Model -> Result Http.Error Article -> ( Model, Cmd Msg )
processGotArticleResponse word model result =
    case result of
        Ok article ->
            ( { model | article = Just article }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )



-- Views


searchField : Model -> Html Msg
searchField model =
    div []
        [ input
            [ type_ "text"
            , onInput Input
            , value model.input
            , placeholder "Поиск..."
            , id "search"
            , propagationlessKeyPress
            ]
            []
        ]


loader : Model -> Html Msg
loader model =
    case model.state of
        Loading ->
            div [] [ text "Загрузка" ]

        _ ->
            div [] []


errorBlock : Model -> Html Msg
errorBlock model =
    case model.state of
        Failure ->
            div [] [ text "Ошибка" ]

        _ ->
            div [] []


wordsList : Model -> Html Msg
wordsList model =
    case model.state of
        Success response ->
            div [] (List.map makeWordOption response)

        _ ->
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
        , div [] <| List.map (\word -> div [] [ text word ]) model.localHistory
        , div [] [ text <| String.fromInt model.currentIndex ]
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
        Element name attrs children ->
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
