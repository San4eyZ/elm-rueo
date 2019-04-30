module Main exposing (Model, getWords, init, main, urlBase)

import Browser exposing (Document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events as Events exposing (onInput)
import Http
import Json.Decode as Decode
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


getArticle : String -> Cmd Msg
getArticle word =
    Http.get
        { url = urlBase ++ "/" ++ word
        , expect = Http.expectString GotArticle
        }


type State
    = Failure
    | Loading
    | Initial
    | Success (List String)


type alias Model =
    { state : State, input : String, article : Maybe Article }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Initial "" Nothing
    , Cmd.none
    )


type alias Article =
    String


type alias GotWordsResult =
    List String


type Msg
    = Input String
    | SelectWord String
    | GotWords (Result Http.Error GotWordsResult)
    | GotArticle (Result Http.Error Article)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotWords result ->
            processGotWordsResponse model result

        Input str ->
            processInput model str

        SelectWord str ->
            ( model, getArticle str )

        GotArticle result ->
            processGotArticleResponse model result


view : Model -> Document Msg
view model =
    Document "Title"
        [ div []
            [ searchField model
            , loader model
            , errorBlock model
            , wordsList model
            , articleView model
            ]
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


articleView : Model -> Html Msg
articleView model =
    case model.article of
        Just article ->
            div [] [ text article ]

        Nothing ->
            div [] []


onWordSelect : Html.Attribute Msg
onWordSelect =
    Events.on "click" (Decode.field "target" (Decode.map SelectWord (Decode.field "innerText" Decode.string)))


makeWordOption : String -> Html Msg
makeWordOption name =
    div [ onWordSelect ] [ text name ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


decodeWordLabel : Decode.Decoder String
decodeWordLabel =
    Decode.field "value" Decode.string


decodeItems : Decode.Decoder (List String)
decodeItems =
    Decode.list decodeWordLabel
