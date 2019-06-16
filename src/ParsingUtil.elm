module ParsingUtil exposing (toVirtualDomWrapWords)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Parser exposing (Node(..))
import Regex
import Regexes exposing (..)


toVirtualDomWrapWords : List Node -> List (Html msg)
toVirtualDomWrapWords nodes =
    List.map toVirtualDomEachWrapWords nodes


toVirtualDomEachWrapWords : Node -> Html msg
toVirtualDomEachWrapWords node =
    case node of
        Element name attrs children ->
            Html.node name (List.map toAttribute attrs) (toVirtualDomWrapWords children)

        Text s ->
            processText s

        Comment _ ->
            text ""


makeSplitable : String -> String
makeSplitable s =
    Regex.replace wordRegex (\result -> "|" ++ result.match ++ "|") s


processText : String -> Html msg
processText s =
    span [] <|
        List.indexedMap wrapOdds <|
            String.split "|" <|
                makeSplitable s


wrapOdds : Int -> String -> Html msg
wrapOdds i word =
    if remainderBy 2 i == 1 then
        span [ class "clickable" ] [ text word ]

    else
        text word


toAttribute : ( String, String ) -> Attribute msg
toAttribute ( name, value ) =
    attribute name value
