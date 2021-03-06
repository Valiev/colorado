module Main
  where

import Control.Monad (forM_)

data Color = Black
           | Red
           | Green
           | Yellow
           | Blue
           | Magenta
           | Cyan
           | White
           | DefaultColor
           | LightBlack
           | LightRed
           | LightGreen
           | LightYellow
           | LightBlue
           | LightMagenta
           | LightCyan
           | LightWhite
           deriving (Show, Enum)

allColors = [Black .. LightWhite]

data TextStyle = DefaultStyle
               | Bold
               | Underline
               | Blink
               | Swap
               | Hide

styleCode :: TextStyle -> Int
styleCode style = case style of
    DefaultStyle -> 0
    Bold -> 1
    Underline -> 4
    Blink -> 5
    Swap -> 6
    Hide -> 8

colorCode' :: Int -> Color -> Int
colorCode' offset color = offset + case color of
    Black -> 0
    Red -> 1
    Green -> 2
    Yellow -> 3
    Blue -> 4
    Magenta -> 5
    Cyan -> 6
    White -> 7
    DefaultColor -> 9
    LightBlack -> 60
    LightRed -> 61
    LightGreen -> 62
    LightYellow -> 63
    LightBlue -> 64
    LightMagenta -> 65
    LightCyan -> 66
    LightWhite -> 67

colorCode = colorCode' 30
bgColorCode = colorCode' 40

data ColorText = ColorText {
                   text :: String,
                   style :: TextStyle,
                   color :: Color,
                   bgColor :: Color
                 }

renderText :: ColorText -> String
renderText colorText = prefix ++ rendered_strings ++ suffix
  where rendered_strings = style_str ++ ";" ++ color_str ++ ";" ++ bgcolor_str ++ "m" ++ text_str
        prefix      = "\ESC["
        suffix      = "\ESC[0m"
        style_str   = (show . styleCode) $ style colorText
        color_str   = (show . colorCode) $ color colorText
        bgcolor_str = (show . bgColorCode) $ bgColor colorText
        text_str    = text colorText

-- Compose a line of single text color with different background colors
renderBgColors :: Color -> String -> String
renderBgColors clr text = concat coloredWords
  where coloredWords = map bgColorText allColors
        bgColorText bgClr = renderText $ ColorText text DefaultStyle clr bgClr

main :: IO ()
main = do
  -- Color Matrix
  forM_ allColors (\clr -> putStrLn $ renderBgColors clr "colors")
