{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedStrings #-}

{-

Write a function (dollars) that accepts a `String` and returns a `String`.
It will accept a numeric value as input, representing an amount of money, and convert to its transcribed English.

For example, the input "1.11" will result in a return value of "one dollar and eleven cents"

Invalid characters should be ignored, meaning that every input string has an output string.
The empty string produces "zero dollars and zero cents"

There is a `test` function below that lists more examples of input and output. There are also functions and
data structures that may assist you in deriving the result. It is not compulsory that they are used.

-}

module Course.Cheque where

import Course.Core
import Course.Optional
import Course.List
import Course.Functor
import Course.Applicative
import Course.Monad

-- $setup
-- >>> :set -XOverloadedStrings

-- The representation of the grouping of each exponent of one thousand. ["thousand", "million", ...]
illion ::
  List Chars
illion =
  let preillion ::
        List (Chars -> Chars)
      preillion =
        listh [
          const ""
        , const "un"
        , const "do"
        , const "tre"
        , const "quattuor"
        , const "quin"
        , const "sex"
        , const "septen"
        , const "octo"
        , \q -> if "n" `isPrefixOf` q then "novem" else "noven"
        ]
      postillion ::
        List Chars
      postillion =
        listh [
          "vigintillion"
        , "trigintillion"
        , "quadragintillion"
        , "quinquagintillion"
        , "sexagintillion"
        , "septuagintillion"
        , "octogintillion"
        , "nonagintillion"
        , "centillion"
        , "decicentillion"
        , "viginticentillion"
        , "trigintacentillion"
        , "quadragintacentillion"
        , "quinquagintacentillion"
        , "sexagintacentillion"
        , "septuagintacentillion"
        , "octogintacentillion"
        , "nonagintacentillion"
        , "ducentillion"
        , "deciducentillion"
        , "vigintiducentillion"
        , "trigintaducentillion"
        , "quadragintaducentillion"
        , "quinquagintaducentillion"
        , "sexagintaducentillion"
        , "septuagintaducentillion"
        , "octogintaducentillion"
        , "nonagintaducentillion"
        , "trecentillion"
        , "decitrecentillion"
        , "vigintitrecentillion"
        , "trigintatrecentillion"
        , "quadragintatrecentillion"
        , "quinquagintatrecentillion"
        , "sexagintatrecentillion"
        , "septuagintatrecentillion"
        , "octogintatrecentillion"
        , "nonagintatrecentillion"
        , "quadringentillion"
        , "deciquadringentillion"
        , "vigintiquadringentillion"
        , "trigintaquadringentillion"
        , "quadragintaquadringentillion"
        , "quinquagintaquadringentillion"
        , "sexagintaquadringentillion"
        , "septuagintaquadringentillion"
        , "octogintaquadringentillion"
        , "nonagintaquadringentillion"
        , "quingentillion"
        , "deciquingentillion"
        , "vigintiquingentillion"
        , "trigintaquingentillion"
        , "quadragintaquingentillion"
        , "quinquagintaquingentillion"
        , "sexagintaquingentillion"
        , "septuagintaquingentillion"
        , "octogintaquingentillion"
        , "nonagintaquingentillion"
        , "sescentillion"
        , "decisescentillion"
        , "vigintisescentillion"
        , "trigintasescentillion"
        , "quadragintasescentillion"
        , "quinquagintasescentillion"
        , "sexagintasescentillion"
        , "septuagintasescentillion"
        , "octogintasescentillion"
        , "nonagintasescentillion"
        , "septingentillion"
        , "deciseptingentillion"
        , "vigintiseptingentillion"
        , "trigintaseptingentillion"
        , "quadragintaseptingentillion"
        , "quinquagintaseptingentillion"
        , "sexagintaseptingentillion"
        , "septuagintaseptingentillion"
        , "octogintaseptingentillion"
        , "nonagintaseptingentillion"
        , "octingentillion"
        , "decioctingentillion"
        , "vigintioctingentillion"
        , "trigintaoctingentillion"
        , "quadragintaoctingentillion"
        , "quinquagintaoctingentillion"
        , "sexagintaoctingentillion"
        , "septuagintaoctingentillion"
        , "octogintaoctingentillion"
        , "nonagintaoctingentillion"
        , "nongentillion"
        , "decinongentillion"
        , "vigintinongentillion"
        , "trigintanongentillion"
        , "quadragintanongentillion"
        , "quinquagintanongentillion"
        , "sexagintanongentillion"
        , "septuagintanongentillion"
        , "octogintanongentillion"
        , "nonagintanongentillion"
        ]
  in listh [
       ""
     , "thousand"
     , "million"
     , "billion"
     , "trillion"
     , "quadrillion"
     , "quintillion"
     , "sextillion"
     , "septillion"
     , "octillion"
     , "nonillion"
     , "decillion"
     , "undecillion"
     , "duodecillion"
     , "tredecillion"
     , "quattuordecillion"
     , "quindecillion"
     , "sexdecillion"
     , "septendecillion"
     , "octodecillion"
     , "novemdecillion"
     ] ++ lift2 ((++) =<<) preillion postillion

-- A data type representing the digits zero to nine.
data Digit =
  Zero
  | One
  | Two
  | Three
  | Four
  | Five
  | Six
  | Seven
  | Eight
  | Nine
  deriving (Eq, Ord)

showDigit ::
  Digit
  -> Chars
showDigit Zero =
  "zero"
showDigit One =
  "one"
showDigit Two =
  "two"
showDigit Three =
  "three"
showDigit Four =
  "four"
showDigit Five =
  "five"
showDigit Six =
  "six"
showDigit Seven =
  "seven"
showDigit Eight =
  "eight"
showDigit Nine =
  "nine"

showDigitTimes10 ::
    Digit
    -> Chars
showDigitTimes10 One =
  "Ten"
showDigitTimes10 Two =
  "twenty"
showDigitTimes10 Three =
  "thirty"
showDigitTimes10 Four =
  "forty"
showDigitTimes10 Five =
  "fifty"
showDigitTimes10 Six =
  "sixty"
showDigitTimes10 Seven =
  "seventy"
showDigitTimes10 Eight =
  "eighty"
showDigitTimes10 Nine =
  "ninety"
showDigitTimes10 _ =
  ""

showDigitsPlus10 ::
  Digit
  -> Chars
showDigitsPlus10 Zero =
  "ten"
showDigitsPlus10 One =
  "eleven"
showDigitsPlus10 Two =
  "twelve"
showDigitsPlus10 Three =
  "thirteen"
showDigitsPlus10 Four =
  "fourteen"
showDigitsPlus10 Five =
  "fifteen"
showDigitsPlus10 Six =
  "sixteen"
showDigitsPlus10 Seven =
  "seventeen"
showDigitsPlus10 Eight =
  "eighteenn"
showDigitsPlus10 Nine =
  "nineteen"
showDigitsPlus10 _ =
  ""

show2DigitNums ::
  Digit
  -> Digit
  -> Chars
show2DigitNums Zero u = showDigit u
show2DigitNums One u  = showDigitsPlus10 u
show2DigitNums t Zero = showDigitTimes10 t
show2DigitNums t u    = showDigitTimes10 t ++ "-" ++ showDigit u

show3DigitNums ::
  Digit
  -> Digit
  -> Digit
  -> Chars
show3DigitNums Zero Zero Zero = ""
show3DigitNums h Zero Zero    = showDigit h ++ " hundred"
show3DigitNums Zero t u       = show2DigitNums t u
show3DigitNums h t u          = showDigit h ++ " hundred and " ++ show2DigitNums t u

-- A data type representing one, two or three digits, which may be useful for grouping.
data Digit3 =
  D1 Digit
  | D2 Digit Digit
  | D3 Digit Digit Digit
  deriving Eq

isAllZeros ::
  Digit3
  -> Bool
isAllZeros (D1 Zero)            = True
isAllZeros (D2 Zero Zero)       = True
isAllZeros (D3 Zero Zero Zero)  = True
isAllZeros _                    = False

toListDigit3 ::
  List Digit
  -> List Digit3
toListDigit3 =
  foldRight
    (\d d3s ->
      case d3s of
        (D2 t u :. d3s') ->
          D3 d t u :. d3s'
        (D1 u :. d3s') ->
          D2 d u :. d3s'
        _ ->
          D1 d :. d3s)
    Nil

showDigit3 ::
  Digit3
  -> Chars
showDigit3 (D1 d) =
  showDigit d
showDigit3 (D2 t u) =
  show2DigitNums t u
showDigit3 (D3 h t u) =
  show3DigitNums h t u

showListDigit3 ::
  List Digit3
  -> Chars
showListDigit3 =
  foldRight showDigit3' Nil
  . reverse
  . zip illion
  . reverse
  where
    showDigit3' (_, d3) Nil =
      showDigit3 d3
    showDigit3' (ion, d3) d3s
      | isAllZeros d3 =
        showDigit3 d3 ++ d3s
      | otherwise =
        showDigit3 d3 ++ " " ++ ion ++ " " ++ d3s

showListDigit3WithSuffix ::
  Chars
  -> Chars
  -> List Digit3
  -> Chars
showListDigit3WithSuffix single _ (D1 One :. Nil) =
  showDigit One ++ single
showListDigit3WithSuffix _ plural l =
  showListDigit3 l ++ plural

-- Possibly convert a character to a digit.
fromChar ::
  Char
  -> Optional Digit
fromChar '0' =
  Full Zero
fromChar '1' =
  Full One
fromChar '2' =
  Full Two
fromChar '3' =
  Full Three
fromChar '4' =
  Full Four
fromChar '5' =
  Full Five
fromChar '6' =
  Full Six
fromChar '7' =
  Full Seven
fromChar '8' =
  Full Eight
fromChar '9' =
  Full Nine
fromChar _ =
  Empty

-- | Take a numeric value and produce its English output.
--
-- >>> dollars "0"
-- "zero dollars and zero cents"
--
-- >>> dollars "1"
-- "one dollar and zero cents"
--
-- >>> dollars "0.1"
-- "zero dollars and ten cents"
--
-- >>> dollars "1."
-- "one dollar and zero cents"
--
-- >>> dollars "0."
-- "zero dollars and zero cents"
--
-- >>> dollars "0.0"
-- "zero dollars and zero cents"
--
-- >>> dollars ".34"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "0.3456789"
-- "zero dollars and thirty-four cents"
--
-- >>> dollars "1.0"
-- "one dollar and zero cents"
--
-- >>> dollars "1.01"
-- "one dollar and one cent"
--
-- >>> dollars "a1a"
-- "one dollar and zero cents"
--
-- >>> dollars "a1a.a0.7b"
-- "one dollar and seven cents"
--
-- >>> dollars "100"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.0"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "100.00000"
-- "one hundred dollars and zero cents"
--
-- >>> dollars "1000456.13"
-- "one million four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "1001456.13"
-- "one million one thousand four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "16000000456.13"
-- "sixteen billion four hundred and fifty-six dollars and thirteen cents"
--
-- >>> dollars "100.45"
-- "one hundred dollars and forty-five cents"
--
-- >>> dollars "100.07"
-- "one hundred dollars and seven cents"
--
-- >>> dollars "9abc9def9ghi.jkl9mno"
-- "nine hundred and ninety-nine dollars and ninety cents"
--
-- >>> dollars "12345.67"
-- "twelve thousand three hundred and forty-five dollars and sixty-seven cents"
--
-- >>> dollars "456789123456789012345678901234567890123456789012345678901234567890.12"
-- "four hundred and fifty-six vigintillion seven hundred and eighty-nine novemdecillion one hundred and twenty-three octodecillion four hundred and fifty-six septendecillion seven hundred and eighty-nine sexdecillion twelve quindecillion three hundred and forty-five quattuordecillion six hundred and seventy-eight tredecillion nine hundred and one duodecillion two hundred and thirty-four undecillion five hundred and sixty-seven decillion eight hundred and ninety nonillion one hundred and twenty-three octillion four hundred and fifty-six septillion seven hundred and eighty-nine sextillion twelve quintillion three hundred and forty-five quadrillion six hundred and seventy-eight trillion nine hundred and one billion two hundred and thirty-four million five hundred and sixty-seven thousand eight hundred and ninety dollars and twelve cents"
dollars ::
  Chars
  -> Chars
dollars cs =
  let
    num = integer cs
    dec =
      if '.' `elem` cs
        then decimal cs
        else decimal ".0"
  in
    showListDigit3WithSuffix " dollar" " dollars" num
    ++ " and "
    ++ showListDigit3WithSuffix " cent" " cents" dec
  -- error "todo: Course.Cheque#dollars"

integer ::
  Chars
  -> List Digit3
integer =
  toListDigit3
  . (\ds -> ifThenElse (ds == Nil) (Zero :. Nil) ds)
  . map (\(Full d) -> d)
  . filter (/= Empty)
  . map fromChar
  . takeWhile (/= '.')

decimal ::
  Chars
  -> List Digit3
decimal =
  toListDigit3
  . (\ds -> ifThenElse (ds == Nil) (Zero :. Nil) ds)
  . dropWhile (== Zero)
  . take 2
  . reverse
  . (Zero :.)
  . dropWhile (== Zero)
  . reverse
  . map (\(Full d) -> d)
  . filter (/= Empty)
  . map fromChar
  . drop 1
  . dropWhile (/= '.')
