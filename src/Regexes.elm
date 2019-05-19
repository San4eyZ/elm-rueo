module Regexes exposing (assuredRegex, doctypeRegex, scriptTagRegex, trailingDivRegex, wordRegex)

import Regex exposing (Regex)


assuredRegex : String -> Regex
assuredRegex str =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith { caseInsensitive = False, multiline = True } str


scriptTagRegex : Regex
scriptTagRegex =
    assuredRegex "<script.*?>((.|\n)*?)<\\/script>"


trailingDivRegex : Regex
trailingDivRegex =
    assuredRegex "<\\/div>(?!(.|\n)*?(div)(.|\n)*?$)"


doctypeRegex : Regex
doctypeRegex =
    assuredRegex "^<!DOCTYPE"


wordRegex : Regex
wordRegex =
    assuredRegex "[А-Яа-яёо́]+|[A-Za-zĉŭĝ]+"
