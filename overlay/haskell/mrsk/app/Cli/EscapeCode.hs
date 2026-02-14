module Cli.EscapeCode where

import Control.Applicative (Alternative ((<|>)), many)
import Data.Attoparsec.Text qualified as Atto
import Data.Functor ((<&>))
import Data.Text (Text)
import Data.Word (Word8)
import Graphics.Vty (Attr (..), MaybeDefault (..), defAttr)
import Graphics.Vty qualified as Vty

data CaretCode
  = Caret'Bell
  | Caret'Backspace
  | Caret'Tab
  | Caret'LineFeed
  | Caret'FormFeed
  | Caret'CarriegeReturn
  | Caret'Escape
  deriving (Show)

-- data FeCode = Fe'SingleShiftTwo

data Color4Bit
  = Color4Bit'Black Bool
  | Color4Bit'Red Bool
  | Color4Bit'Green Bool
  | Color4Bit'Yellow Bool
  | Color4Bit'Blue Bool
  | Color4Bit'Magenta Bool
  | Color4Bit'Cyan Bool
  | Color4Bit'White Bool
  deriving (Show)

data SGRCode
  = SGR'Reset
  | SGR'Bold
  | SGR'Faint
  | SGR'Italic
  | SGR'Underline
  | SGR'CrossedOut
  | -- add reverse video
    SGR'Normal
  | SGR'NotUnderlined
  | SGR'Reveal
  | SGR'NotCrossedOut
  | SGR'SetForegroundColor Color4Bit
  | SGR'SetForegroundColor8Bit Word8
  | SGR'SetForegroundColor24Bit Word8 Word8 Word8
  | SGR'DefaultForegroundColor
  | SGR'SetBackgroundColor Color4Bit
  | SGR'SetBackgroundColor8Bit Word8
  | SGR'SetBackgroundColor24Bit Word8 Word8 Word8
  | SGR'DefaultBackgroundColor
  | SGR'Framed
  | SGR'Encircled
  | SGR'Overlined
  | SGR'NotFramedOrEncircled
  | SGR'NotOverlined
  | SGR'SetUnderlineColor8Bit Word8
  | SGR'SetUnderlineColor24Bit Word8 Word8 Word8
  | SGR'DefaultUnderlineColor
  deriving (Show)

parseBetterColor :: (Word8 -> Word8 -> Word8 -> SGRCode) -> (Word8 -> SGRCode) -> Atto.Parser SGRCode
parseBetterColor c24bit c8bit = do
  colorMode <- Atto.digit
  case colorMode of
    '5' -> Atto.char ';' *> (c8bit <$> Atto.decimal)
    '2' -> do
      r <- Atto.char ';' *> Atto.decimal
      g <- Atto.char ';' *> Atto.decimal
      b <- Atto.char ';' *> Atto.decimal

      pure $ c24bit r g b
    _ -> fail "Neither a foreground or background color"

parse4BitSGR
  :: Word8
  -> (Color4Bit -> Atto.Parser SGRCode)
  -> Atto.Parser SGRCode
parse4BitSGR code c4bit
  | code == 30 || code == 40 = c4bit (Color4Bit'Black False)
  | code == 31 || code == 41 = c4bit (Color4Bit'Red False)
  | code == 32 || code == 42 = c4bit (Color4Bit'Green False)
  | code == 33 || code == 43 = c4bit (Color4Bit'Yellow False)
  | code == 34 || code == 44 = c4bit (Color4Bit'Blue False)
  | code == 35 || code == 45 = c4bit (Color4Bit'Magenta False)
  | code == 36 || code == 46 = c4bit (Color4Bit'Cyan False)
  | code == 37 || code == 47 = c4bit (Color4Bit'White False)
  | code == 90 || code == 100 = c4bit (Color4Bit'Black True)
  | code == 91 || code == 101 = c4bit (Color4Bit'Red True)
  | code == 92 || code == 102 = c4bit (Color4Bit'Green True)
  | code == 93 || code == 103 = c4bit (Color4Bit'Yellow True)
  | code == 94 || code == 104 = c4bit (Color4Bit'Blue True)
  | code == 95 || code == 105 = c4bit (Color4Bit'Magenta True)
  | code == 96 || code == 106 = c4bit (Color4Bit'Cyan True)
  | code == 97 || code == 107 = c4bit (Color4Bit'White True)
  | otherwise = fail ("Not a 4bit color: " <> show code)

parseSGRCode :: Atto.Parser SGRCode
parseSGRCode = do
  code :: Word8 <- Atto.decimal

  case code of
    0 -> pure SGR'Reset
    1 -> pure SGR'Bold
    2 -> pure SGR'Faint
    3 -> pure SGR'Italic
    4 -> pure SGR'Underline
    9 -> pure SGR'CrossedOut
    22 -> pure SGR'Normal
    23 -> pure SGR'Normal
    24 -> pure SGR'NotUnderlined
    28 -> pure SGR'Reveal
    29 -> pure SGR'NotCrossedOut
    x
      | x >= 30 && x <= 37 || x >= 90 && x <= 97 ->
          parse4BitSGR code (pure . SGR'SetForegroundColor)
      | x == 38 -> Atto.char ';' *> parseBetterColor SGR'SetUnderlineColor24Bit SGR'SetUnderlineColor8Bit
      | x >= 40 && x <= 47 || x >= 100 && x <= 107 ->
          parse4BitSGR code (pure . SGR'SetBackgroundColor)
      | x == 48 -> Atto.char ';' *> parseBetterColor SGR'SetUnderlineColor24Bit SGR'SetUnderlineColor8Bit
    39 -> pure SGR'DefaultForegroundColor
    49 -> pure SGR'DefaultBackgroundColor
    51 -> pure SGR'Framed
    52 -> pure SGR'Encircled
    53 -> pure SGR'Overlined
    54 -> pure SGR'NotFramedOrEncircled
    55 -> pure SGR'NotOverlined
    58 -> Atto.char ';' *> parseBetterColor SGR'SetUnderlineColor24Bit SGR'SetUnderlineColor8Bit
    59 -> pure SGR'DefaultUnderlineColor
    _ -> fail "Not SGR code"

nonEmpty :: Atto.Parser Text -> Atto.Parser Text
nonEmpty parser = parser >>= \result -> if result == "" then fail "empty result" else pure result

parseSomething :: Atto.Parser [Either Text [SGRCode]]
parseSomething =
  many $
    (nonEmpty (Atto.takeTill (== '\ESC')) <&> Left)
      <|> (Atto.char '\ESC' >> Atto.char '[' *> ((parseSGRCode `Atto.sepBy'` (Atto.char ';')) <* Atto.char 'm') <&> Right)

data CSICode
  = CSI'CursorUp Word8
  | CSI'CursorDown Word8
  | CSI'CursorForward Word8
  | CSI'CursorBack Word8
  | CSI'CursorNextLine Word8
  | CSI'CursorPreviousLine Word8
  | CSI'CursorHorizontalAbsolute Word8
  | CSI'CursorPosition Word8 Word
  | CSI'EraseInDisplay Word8
  | CSI'EraseInLine Word8
  | CSI'ScrollUp Word8
  | CSI'ScrollDown Word8
  | CSI'HorizontalVerticalPosition Word Word
  | CSI'SelectGraphicRendition SGRCode
  | CSI'AuxPortOn
  | CSI'AuxPortOff
  | CSI'DeviceStatusReport

data EscapeCode
  = EscapeCode'Caret CaretCode
  | -- | EscapeCode'Fe FeCode
    EscapeCode'CSI CSICode

color4bitToBrick :: Color4Bit -> Vty.Color
color4bitToBrick color4bit =
  case color4bit of
    Color4Bit'Black False -> Vty.black
    Color4Bit'Red False -> Vty.red
    Color4Bit'Green False -> Vty.green
    Color4Bit'Yellow False -> Vty.yellow
    Color4Bit'Blue False -> Vty.blue
    Color4Bit'Magenta False -> Vty.magenta
    Color4Bit'Cyan False -> Vty.cyan
    Color4Bit'White False -> Vty.white
    Color4Bit'Black True -> Vty.brightBlack
    Color4Bit'Red True -> Vty.brightRed
    Color4Bit'Green True -> Vty.brightGreen
    Color4Bit'Yellow True -> Vty.brightYellow
    Color4Bit'Blue True -> Vty.brightBlue
    Color4Bit'Magenta True -> Vty.brightMagenta
    Color4Bit'Cyan True -> Vty.brightCyan
    Color4Bit'White True -> Vty.brightWhite

applyCode :: Attr -> SGRCode -> Attr
applyCode _ SGR'Reset = defAttr
applyCode attr SGR'Bold = attr{attrStyle = SetTo Vty.bold}
applyCode attr SGR'Faint = attr{attrStyle = SetTo Vty.dim}
applyCode attr SGR'Italic = attr{attrStyle = SetTo Vty.italic}
applyCode attr SGR'Underline = attr{attrStyle = SetTo Vty.underline}
applyCode attr SGR'CrossedOut = attr{attrStyle = SetTo Vty.strikethrough}
applyCode attr SGR'Normal = attr{attrStyle = Default}
applyCode attr SGR'NotUnderlined = if attrStyle attr == SetTo Vty.underline then attr{attrStyle = Default} else attr
applyCode attr SGR'NotCrossedOut = if attrStyle attr == SetTo Vty.strikethrough then attr{attrStyle = Default} else attr
applyCode attr (SGR'SetForegroundColor color4bit) =
  attr
    { attrForeColor = SetTo (color4bitToBrick color4bit)
    }
applyCode attr (SGR'SetBackgroundColor color4bit) =
  attr
    { attrBackColor = SetTo (color4bitToBrick color4bit)
    }
applyCode attr _ = attr

escapeCodeToBrick :: [Either Text [SGRCode]] -> Attr -> [(Attr, Text)]
escapeCodeToBrick (Left text : rest) attr = (attr, text) : escapeCodeToBrick rest attr
escapeCodeToBrick (Right codes : rest) attr = escapeCodeToBrick rest (foldl applyCode attr codes)
escapeCodeToBrick [] _ = []
