module Main exposing (Model, Msg, init, main)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (onInput)
import Html.Parser as Parser exposing (Node(..))
import Http
import Json.Decode as Decode
import Json.Encode as Encode
import ParsingUtil exposing (toVirtualDomWrapWords)
import Ports exposing (..)
import Regex exposing (Regex)
import Regexes exposing (..)



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


getArticle : String -> Model -> Cmd Msg
getArticle word model =
    Cmd.batch
        [ Http.get
            { url = urlBase ++ "/" ++ word
            , expect = Http.expectString GotArticle
            }
        , saveHistory <| [ word ] ++ model.history
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
    }


init : List String -> ( Model, Cmd Msg )
init history =
    ( Model Initial "" Nothing history
    , Cmd.none
    )


type alias Article =
    String


type alias GotWordsResult =
    List String


type Msg
    = Idle
    | Input String
    | SelectWord String
    | GotWords (Result Http.Error GotWordsResult)
    | GotArticle (Result Http.Error Article)
    | SetHistory (List String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWords result ->
            processGotWordsResponse model result

        Input str ->
            processInput model str

        SelectWord str ->
            ( model, getArticle str model )

        GotArticle result ->
            processGotArticleResponse model result

        SetHistory history ->
            ( { model | history = history }, Cmd.none )

        Idle ->
            ( model, Cmd.none )


view : Model -> Document Msg
view model =
    Document "Title"
        [ div []
            [ searchField model
            , loader model
            , errorBlock model
            , wordsList model
            , articleView model
            , historyView model
            ]
        ]


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ updateHistory SetHistory
        , searchKeyPressed Input
        ]


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
        ( { model | input = str, state = Loading }, getWords str )


processGotArticleResponse : Model -> Result Http.Error Article -> ( Model, Cmd Msg )
processGotArticleResponse model result =
    case result of
        Ok article ->
            ( { model | article = Just article }, Cmd.none )

        _ ->
            ( model, Cmd.none )


searchField : Model -> Html Msg
searchField model =
    div []
        [ input [ type_ "text", onInput Input, value model.input, placeholder "Поиск..." ] []
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
    div [] <| List.map (\word -> div [] [ text word ]) model.history


articleView : Model -> Html Msg
articleView model =
    case model.article of
        Just article ->
            prepareArticle article

        Nothing ->
            div [] []


makeWordOption : String -> Html Msg
makeWordOption name =
    div [ onWordSelect ] [ text name ]


decodeWordLabel : Decode.Decoder String
decodeWordLabel =
    Decode.field "value" Decode.string


decodeItems : Decode.Decoder (List String)
decodeItems =
    Decode.list decodeWordLabel



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
            if containsSearchResult attrs then
                Just node

            else
                List.foldl lazyNodeFind Nothing children

        Text _ ->
            Nothing

        Comment _ ->
            Nothing


containsSearchResult : List ( String, String ) -> Bool
containsSearchResult attributes =
    List.any (\( name, val ) -> name == "class" && val == "search_result") attributes



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
            div [ onWordClick ] (toVirtualDomWrapWords [ findSearchResultInList tree ])

        _ ->
            text "Error"



-- Events


onWordClick : Html.Attribute Msg
onWordClick =
    Events.on "click" <|
        Decode.map pairToMsg <|
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


pairToMsg : ( String, String ) -> Msg
pairToMsg ( className, innerText ) =
    case className of
        "clickable" ->
            SelectWord innerText

        _ ->
            Idle


onWordSelect : Html.Attribute Msg
onWordSelect =
    Events.on "click" <|
        Decode.map SelectWord <|
            Decode.at [ "target", "innerText" ] Decode.string
