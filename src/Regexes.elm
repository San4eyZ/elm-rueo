module Regexes exposing (articleNotFoundRegex, doctypeRegex, scriptTagRegex, stressRegex, trailingDivRegex, wordRegex)

import Regex exposing (Regex)


assuredRegex : String -> Regex
assuredRegex str =
    Maybe.withDefault Regex.never <|
        Regex.fromStringWith { caseInsensitive = False, multiline = False } str


scriptTagRegex : Regex
scriptTagRegex =
    assuredRegex "<script.*?>((.|\n)*?)<\\/script>"


trailingDivRegex : Regex
trailingDivRegex =
    assuredRegex "<\\/div>(?!(.|\n|\r)*?(div)(.|\n|\r)*$)"


doctypeRegex : Regex
doctypeRegex =
    assuredRegex "^<!DOCTYPE"


wordRegex : Regex
wordRegex =
    assuredRegex "[А-Яа-яёо́]+|[A-Za-zĉŭĝŝ]+"


stressRegex : Regex
stressRegex =
    assuredRegex "́"


articleNotFoundRegex : Regex
articleNotFoundRegex =
    assuredRegex "Подходящей словарной статьи не найдено"
