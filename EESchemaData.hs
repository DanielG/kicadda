-- kicadda: KiCad Design Automation
-- Copyright (C) 2016  Daniel Gröber <dxld@darkboxed.org>
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

{-# LANGUAGE
  TypeFamilies,
  OverloadedStrings,
  FlexibleContexts,
  GeneralizedNewtypeDeriving,
  GADTs,
  DataKinds
  #-}
module EESchemaData where

import Control.Applicative
import Control.Monad
import Data.Char
import Data.List
import Data.Maybe
import Text.Printf
import Text.Megaparsec
import Text.Megaparsec.String
import Text.Megaparsec.Lexer
import Text.Show.Pretty

import Linear

class HasRef a where
    href :: a -> String
instance HasRef SchComp where
    href = scRef
instance HasRef LibDef where
    href = ldRef

class HasPos a where
    startPos :: a -> V2 Mil
    endPos   :: a -> V2 Mil
instance HasPos SchComp where
    startPos = scPos
    endPos   = scPos
instance HasPos SchSheet where
    startPos = ssPos
    endPos   = ssPos
instance HasPos SchBitmap where
    startPos = sbPos
    endPos   = sbPos
instance HasPos SchLine where
    startPos = slStart
    endPos   = slEnd
instance HasPos SchEntry where
    startPos = seStart
    endPos   = seEnd
instance HasPos SchMarker where
    startPos = smPos
    endPos   = smPos
instance HasPos SchText where
    startPos = stPos
    endPos   = stPos

coincide :: HasPos a => a -> V2 Mil -> Bool
coincide a b = let
    sa = startPos a
    ea = endPos   a
  in
    or [ sa == b
       , ea == b
       ]

data SchItem
  = SIC SchComp
  | SIS SchSheet
  | SIB SchBitmap
  | SIL SchLine
  | SIE SchEntry
  | SIM SchMarker
  | SIT SchText
  deriving (Eq, Ord, Show)

isSIC, isSIS, isSIB, isSIL, isSIE, isSIM, isSIT :: SchItem -> Bool

isSIC a = case a of SIC {} -> True; _ -> False
isSIS a = case a of SIS {} -> True; _ -> False
isSIB a = case a of SIB {} -> True; _ -> False
isSIL a = case a of SIL {} -> True; _ -> False
isSIE a = case a of SIE {} -> True; _ -> False
isSIM a = case a of SIM {} -> True; _ -> False
isSIT a = case a of SIT {} -> True; _ -> False

data Sch = Sch {
      sMagic  :: String,
      sHeader :: String,
      sDesc   :: SchDescr,
      sItems  :: [SchItem]
    } deriving (Eq, Ord, Show)

data SchDescr = SchDescr {
      sdPaper    :: String,
      sdSize     :: (V2 Mil),
      sdOrient   :: Maybe String,
      sdEncoding :: String,
      sdSheet    :: (Int, Int),
      sdTitle    :: String,
      sdDate     :: String,
      sdRevision :: String,
      sdCompany  :: String,
      sdComment1 :: String,
      sdComment2 :: String,
      sdComment3 :: String,
      sdComment4 :: String
    } deriving (Eq, Ord, Show)

data SchComp = SchComp {
      scPart      :: String,
      scRef       :: String,
      scUnit      :: Int,
      scCoversion :: Int,
      scStamp     :: String,
      scPos       :: V2 Mil,
      scAltRefs   :: [(String, String, String)],
      scFields    :: [SchCompField],
      scMatrix    :: [Int]
    } deriving (Eq, Ord, Show)

data SchCompField = SchCompField {
      scfId      :: Int,
      scfValue   :: String,
      scfOrient  :: String,
      scfPos     :: V2 Mil,
      scfSize    :: Mil,
      scfFlags   :: String,
      scfJustify :: String,
      scfStyle   :: String,
      scfName    :: Maybe String
    } deriving (Eq, Ord, Show)

data SchSheet = SchSheet {
      ssPos    :: V2 Mil,
      ssSize   :: V2 Mil,
      ssStamp  :: String,
      ssName   :: (String, Mil),
      ssFile   :: (String, Mil),
      ssFields :: [(Int, String, Char, Char, (V2 Mil), Mil)]
    } deriving (Eq, Ord, Show)

data SchBitmap = SchBitmap {
      sbPos   :: V2 Mil,
      sbScale :: Double,
      sbData  :: String
    } deriving (Eq, Ord, Show)

data SchLine = SchLine {
      slType :: String,
      slStart :: V2 Mil,
      slEnd   :: V2 Mil
    } deriving (Eq, Ord, Show)

data SchEntry = SchEntry {
      seType :: String,
      seStart :: V2 Mil,
      seEnd   :: V2 Mil
    } deriving (Eq, Ord, Show)

data SchMarker = SchMarker {
      smType :: String,
      smPos  :: V2 Mil
    } deriving (Eq, Ord, Show)

data SchText = SchText {
      stType      :: String,
      stPos       :: V2 Mil,
      stOrient    :: Int,
        -- ^
        -- * 0 is the horizontal and left justified.
        -- * 1 is vertical and top justified.
        -- * 2 is horizontal and right justified.  It is the equivalent of the mirrored 0 orentation.
        -- * 3 is veritcal and bottom justifiend. It is the equivalent of the mirrored 1 orentation.


      stSize      :: Mil,
      stShape     :: Maybe String,
      stShape'    :: String,
      stThickness :: Int,
      stText      :: String
    } deriving (Eq, Ord, Show)

type Lib = [LibDef]

data LibDef = LibDef {
      ldName        :: String,
      ldRef         :: String,
      ldTextOff     :: Mil,
      ldDrawPinNum  :: Bool,
      ldDrawPinName :: Bool,
      ldUnitCount   :: Int,
      ldUnitsLocked :: Bool,
      ldOption      :: Maybe String,
      ldFields      :: [LibField],
      ldAliases     :: [String],
      ldFpList      :: Maybe [String],
      ldDrawCmds    :: [LibDraw]
    } deriving (Eq, Ord, Show)

data LibField = LibField {
      lfId        :: Int,
      lfLabel     :: String,
      lfPos       :: V2 Mil,
      lfDimension :: Mil,
      lfOrient    :: Char,
      lfVisible   :: Bool,
      lfHJustify  :: Maybe Char,
      lfVJustify  :: Maybe Char,
      lfStyle     :: Maybe (Char, Char),
      lfValue     :: Maybe String
    } deriving (Eq, Ord, Show)

data LibDraw =
    LDPolygon Int Int Mil [(V2 Mil)] (Maybe Char)
  | LDRectangle (V2 Mil) (V2 Mil) Int Int Mil (Maybe Char)
  | LDCircle (V2 Mil) Mil Int Int Mil Char
  | LDArc (V2 Mil) Mil Int Int Int Int Mil (Maybe Char) (Maybe ((V2 Mil), (V2 Mil)))
  | LDText Int (V2 Mil) Mil Int Int String (Maybe (String, Bool, Char, Char))
  | LDPin {
      ldpName    :: String,
      ldpId      :: String,
      ldpPos     :: V2 Mil,
      ldpLen     :: Mil,
      ldpOrient  :: Char,
      ldpNumSz   :: Mil,
      ldpNamSz   :: Mil,
      ldpUnit    :: Int,
      ldpConv    :: Int,
      ldpType    :: Char,
      ldpVisible :: Bool,
      ldpShape   :: Maybe String
    }
  deriving (Eq, Ord, Show)

newtype Mil = Mil { unMil :: Integer }
 deriving (Eq, Ord, Show, Enum, Num, Integral, Real)

parseDescr :: Parser SchDescr
parseDescr = do
  _ <- lexBlockMark "Descr"
  paper <- lexToken
  width <- lexMil
  height <- lexMil
  orient <- optional lexToken
  _ <- eol

  SchDescr paper (V2 width height) orient
    <$> parseKeyValue "encoding" lexToken
    <*> parseKeyValue "Sheet"    ((,) <$> lexInt <*> lexInt)
    <*> parseKeyValue "Title"    lexString
    <*> parseKeyValue "Date"     lexString
    <*> parseKeyValue "Rev"      lexString
    <*> parseKeyValue "Comp"     lexString
    <*> parseKeyValue "Comment1" lexString
    <*> parseKeyValue "Comment2" lexString
    <*> parseKeyValue "Comment3" lexString
    <*> parseKeyValue "Comment4" lexString
    <* line (lexBlockMark "EndDescr")

printDescr :: SchDescr -> String
printDescr (SchDescr
              paper
              (V2 size_x size_y)
              orient
              encoding
              (sheet_id, sheet_count)
              title
              date
              revision
              company
              comment1
              comment2
              comment3
              comment4)
 = unlines [ intercalate " " $
               [ "$Descr"
               , paper
               , printMil
                 size_x
               , printMil size_y
               ] ++ maybeToList orient
           , "encoding " ++ encoding
           , intercalate " " [ "Sheet", show sheet_id, show sheet_count ]
           , "Title "    ++ printString title
           , "Date "     ++ printString date
           , "Rev "      ++ printString revision
           , "Comp "     ++ printString company
           , "Comment1 " ++ printString comment1
           , "Comment2 " ++ printString comment2
           , "Comment3 " ++ printString comment3
           , "Comment4 " ++ printString comment4
           , "$EndDescr"
           ]

testDescr :: IO ()
testDescr = do
  let s = "\
\$Descr A4 11693 8268\n\
\encoding utf-8\n\
\Sheet 1 6\n\
\Title \"\"\n\
\Date \"\"\n\
\Rev \"\"\n\
\Comp \"\"\n\
\Comment1 \"\"\n\
\Comment2 \"\"\n\
\Comment3 \"\"\n\
\Comment4 \"\"\n\
\$EndDescr\n"
  a <- testParser (parseDescr <* eof) s
  let s' = printDescr a
  putStr s'
  when (s /= s') $ error "testDescr mismatch"

parseComp :: Parser SchComp
parseComp = do
  _ <- line $ lexBlockMark "Comp"
  _ <- lexMark "L"
  part <- lexToken
  ref  <- lexToken
  _ <- eol
  _ <- lexMark "U"
  unit <- lexInt
  conversion <- lexInt
  stamp <- lexToken
  _ <- eol
  pos <- line $ lexMark "P" *> (V2 <$> lexMil <*> lexMil)
  alt_refs <- many $ do
    _ <- lexMark "AR"
    path <- lexKeyValueEq "Path" lexString'
    ref' <- lexKeyValueEq "Ref" lexString'
    part' <- lexKeyValueEq "Part" lexString'
    _ <- eol
    return (path, ref', part')
  fields <- many $ do
    _ <- lexMark "F"
    i <- lexInt
    value <- lexString
    orient <- lexToken
    pos' <- V2 <$> lexMil <*> lexMil
    size <- lexMil
    flags <- lexToken
    justify <- lexToken
    style <- lexToken
    name <- optional lexString
    _ <- eol
    return $ SchCompField i value orient pos' size flags justify style name
  _ <- char '\t' *> manyTill anyChar eol
  matrix <- char '\t' *> manyTill lexInt eol
  _ <- line $ lexBlockMark "EndComp"
  return $ SchComp part ref unit conversion stamp pos alt_refs fields matrix

printComp :: SchComp -> String
printComp (SchComp
             part
             ref
             unit
             conv
             stamp
             (V2 pos_x pos_y)
             alt_refs
             fields
             matrix)
 = unlines $ concat
     [ [ "$Comp"
       , intercalate " " [ "L", part, ref ]
       , intercalate " " [ "U", show unit, show conv, stamp ]
       , intercalate " " $ [ "P" ] ++ map printMil [ pos_x, pos_y ]
       ]
     , map printAltRef alt_refs
     , map printField fields
     , [ "\t" ++ intercalate " " ([ show unit ] ++ map printMil [ pos_x, pos_y ])
       , "\t" ++ intercalate " " (map show matrix)
       , "$EndComp"
       ]
     ]
 where
   printField (SchCompField
                 i
                 value
                 orient
                 (V2 px py)
                 size
                 flags
                 justify
                 style
                 name)
    = intercalate " " $
        [ "F"
        , show i
        , printString value
        , orient
        , printMil px
        , printMil py
        , printMil size
        , flags
        , justify
        , style
        ] ++ maybeToList (printString <$> name)
   printAltRef (path, ref', part')
    = intercalate " " [ "AR"
                      , "Path="++printString path
                      , "Ref="++printString ref'
                      , "Part="++printString part'
                      ]

testComp :: IO ()
testComp = do
  let s = "\
\$Comp\n\
\L STM32F429VETx U102\n\
\U 1 1 57F6CB4D\n\
\P 10600 4150\n\
\AR Path=\"/54EE0C3D/54F02658\" Ref=\"R1501\" Part=\"1\"\n\
\F 0 \"U102\" H 6200 7100 50 0000 C CNN\n\
\F 1 \"STM32F429VETx\" H 8000 7100 50 0000 C CNN\n\
\F 2 \"LQFP100\" H 8000 7200 50 0000 C CNN\n\
\F 3 \"\" H 10600 4150 50 0000 C CNN\n\
\F 4 \"20%\" H 1650 1400 60 0000 C CNN \"Tolerance\"\n\
\\t1 10600 4150\n\
\\t1 0 0 -1\n\
\$EndComp\n"
  a <- testParser (parseComp <* eof) s
  let s' = printComp a
  putStr s'
  when (s /= s') $ error "testComponent mismatch"

parseSheet :: Parser SchSheet
parseSheet = do
  _ <- line $ lexBlockMark "Sheet"
  _ <- lexMark "S"
  pos  <- V2 <$> lexMil <*> lexMil
  size <- V2 <$> lexMil <*> lexMil
  _ <- eol
  stamp <- line $ lexMark "U" *> lexToken
  name  <- line $ lexMark "F0" *> ((,) <$> lexString <*> lexMil)
  file  <- line $ lexMark "F1" *> ((,) <$> lexString <*> lexMil)
  fields <- many $ do
    _ <- char 'F'
    i    <- lexInt :: Parser Int
    labl <- lexString
    ty   <- choice (map char [ 'I', 'O', 'B', 'T', 'U' ]) <* skipSpaces
    side <- choice (map char [ 'L', 'R', 'T', 'B' ])      <* skipSpaces
    p    <- V2 <$> lexMil <*> lexMil
    sz   <- lexMil
    _ <- eol
    return (i, labl, ty, side, p, sz)
  _ <- line $ lexBlockMark "EndSheet"
  return $ SchSheet pos size stamp name file fields

printSheet :: SchSheet -> String
printSheet (SchSheet
              (V2 pos_x pos_y)
              (V2 size_x size_y)
              stamp
              (name, name_size)
              (file, file_size)
              fields)
 = unlines $ concat
     [ [ "$Sheet"
       , intercalate " " $ [ "S" ] ++ map printMil [ pos_x, pos_y
                                                   , size_x, size_y ]
       , intercalate " " [ "U", stamp ]
       , intercalate " " [ "F0", printString name, printMil name_size ]
       , intercalate " " [ "F1", printString file, printMil file_size ]
       ]
     , map printField fields
     , [ "$EndSheet" ]
     ]
 where
   printField (i, value, ty, orient, (V2 px py), size) =
       intercalate " "
         [ "F"++show i
         , printString value
         , [ty]
         , [orient]
         , printMil px
         , printMil py
         , printMil size ]

testSheet :: IO ()
testSheet = do
  let s = "\
\$Sheet\n\
\S 8500 1050 550 650\n\
\U 56944AA5\n\
\F0 \"Power Supply\" 60\n\
\F1 \"power.sch\" 60\n\
\F2 \"V5\" O R 9050 1150 60\n\
\F3 \"3V3\" O R 9050 1250 60\n\
\F4 \"Vbat\" I L 8500 1150 60\n\
\$EndSheet\n"
  a <- testParser (parseSheet <* eof) s
  let s' = printSheet a
  putStr s'
  when (s /= s') $ error "testSheet mismatch"

parseBitmap :: Parser SchBitmap
parseBitmap = do
  _ <- line $ lexBlockMark "Bitmap"
  pos   <- line $ lexMark "Pos" *> (V2 <$> lexMil <*> lexMil)
  scale <- line $ lexMark "Scale" *> lexDouble
  _ <- line $ lexMark "Data"
  dat   <- line $ hexOrSpaceDigits `manyTill` lexMark "EndData"
  _ <- line $ lexBlockMark "EndBitmap"
  return $ SchBitmap pos scale (concat dat)
 where
    hexOrSpaceDigits = some hexDigitChar <|> some spaceChar

printBitmap :: SchBitmap -> String
printBitmap (SchBitmap (V2 pos_x pos_y) scale dat)
 = unlines [ "$Bitmap"
           , intercalate " " $ [ "Pos" ] ++ map printMil [ pos_x, pos_y ]
           , intercalate " " [ "Scale", printf "%1.6f" scale ]
           , "Data"
           , dropWhileEnd isSpace dat
           , "EndData"
           , "$EndBitmap"
           ]

testBitmap :: IO ()
testBitmap = do
  let s = "\
\$Bitmap\n\
\Pos 10600 10350\n\
\Scale 1.000000\n\
\Data\n\
\89 50 4E 47 0D 0A 1A 0A 00 00 00 0D 49 48 44 52 00 00 01 00 00 00 01 02 08 06 00 00 00 11 BA 09\n\
\6D 00 00 00 04 73 42 49 54 08 08 08 08 7C 08 64 88 00 00 00 09 70 48 59 73 00 00 4A 38 00 00 4A\n\
\58\n\
\EndData\n\
\$EndBitmap\n"
  a <- testParser (parseBitmap <* eof) s
  let s' = printBitmap a
  putStr s'
  when (s /= s') $ error "testBitmap mismatch"

parseLine :: Parser SchLine
parseLine = do
  ty <- line $
    between (lexMark "Wire") (lexMark "Line") $
      lexMark "Wire" <|> lexMark "Bus" <|> lexMark "Notes"
  [sx, sy, ex, ey] <- line $ char '\t' *> count 4 lexMil
  return $ SchLine ty (V2 sx sy) (V2 ex ey)

printLine :: SchLine -> String
printLine (SchLine ty (V2 start_x start_y) (V2 end_x end_y))
 = unlines [ intercalate " " [ "Wire", ty, "Line"]
           , "\t" ++ intercalate " "
               (map printMil [ start_x, start_y
                             , end_x, end_y
                             ])
           ]

testLine :: IO ()
testLine = do
  let s = "\
\Wire Wire Line\n\
\\t4000 7050 4200 7050\n\
\Wire Bus Line\n\
\\t1850 4200 1950 4200\n\
\Wire Notes Line\n\
\\t2400 2200 2400 2600\n"
  a <- testParser (some parseLine <* eof) s
  let s' = concat $ map printLine a
  putStr s'
  when (s /= s') $ error "testLine mismatch"

parseEntry :: Parser SchEntry
parseEntry = do
  ty <- line $
    lexMark "Entry"
      *> choice [ lexMark "Wire" <* lexMark "Line"
                , lexMark "Bus"  <* lexMark "Bus"
                ]
  [sx, sy, ex, ey] <- line $ char '\t' *> count 4 lexMil
  return $ SchEntry ty (V2 sx sy) (V2 ex ey)

printEntry :: SchEntry -> String
printEntry (SchEntry ty (V2 sx sy) (V2 ex ey))
 = unlines [ intercalate " " $
               "Entry" : case ty of
                           "Wire" -> ["Wire", "Line"]
                           "Bus"  -> ["Bus", "Bus"]
           , "\t" ++ intercalate " " (map printMil [sx,sy,ex,ey])
           ]

testEntry :: IO ()
testEntry = do
  let s = "\
\Entry Wire Line\n\
\\t7000 2550 7100 2650\n\
\Entry Bus Bus\n\
\\t7000 2450 7100 2550\n"
  a <- testParser (some parseEntry <* eof) s
  let s' = concat $ map printEntry a
  putStr s'
  when (s /= s') $ error "testEntry mismatch"

parseMarker :: Parser SchMarker
parseMarker = do
  line $ SchMarker <$> (lexMark "Connection" <|> lexMark "NoConn")
                   <*  lexToken
                   <*> (V2 <$> lexMil <*> lexMil)

printMarker :: SchMarker -> String
printMarker (SchMarker ty (V2 sx sy))
 = intercalate " " [ ty, "~", printMil sx, printMil sy ] ++ "\n"

testMarker :: IO ()
testMarker = do
  let s = "\
\Connection ~ 9450 2650\n\
\NoConn ~ 1700 3300\n"
  a <- testParser (some parseMarker <* eof) s
  let s' = concat $ map printMarker a
  putStr s'
  when (s /= s') $ error "testMarker mismatch"

parseText :: Parser SchText
parseText = do
  _ <- lexMark "Text"
  ty <- choice [ lexMark "Label"
               , lexMark "GLabel"
               , lexMark "HLabel"
               , lexMark "Notes"
               ]
  pos <- V2 <$> lexMil <*> lexMil
  orient <- lexInt
  size <- lexMil
  shape <- if ty `elem` [ "GLabel", "HLabel" ]
     then Just <$> choice (map lexMark [ "Input"
                                       , "Output"
                                       , "BiDi"
                                       , "3State"
                                       , "UnSpc"
                                       ])
     else return Nothing
  shape' <- lexToken
  thickness <- lexInt
  _ <- eol
  text <- anyChar `manyTill` eol
  return $ SchText ty pos orient size shape shape' thickness text

printText :: SchText -> String
printText (SchText ty (V2 px py) orient size shape shape' thick text)
 = unlines [ intercalate " " $ concat
               [ [ "Text"
                 , ty
                 , printMil px
                 , printMil py
                 , show orient
                 , printMil size
                 ]
               , maybeToList shape
               , [ shape'
                 , show thick
                 ]
               ]
           , text
           ]

testText :: IO ()
testText = do
  let s = "\
\Text Label 650 650 0 60 ~ 0\n\
\LabelRightNormal\n\
\Text Label 900 1300 1 60 Italic 0\n\
\LableUpItalic\n\
\Text Label 650 950 2 60 ~ 12\n\
\LabelLeftBold\n\
\Text Label 650 1100 3 60 ~ 12\n\
\LabelDownBoldItalic\n\
\Text GLabel 1350 2150 0 60 Input ~ 0\n\
\GLabelInput\n\
\Text GLabel 1350 2300 0 60 Output ~ 0\n\
\GLabelOutput\n\
\Text GLabel 1350 2450 0 60 BiDi ~ 0\n\
\GLableBi\n\
\Text GLabel 1350 2600 0 60 3State ~ 0\n\
\GLabelTri\n\
\Text GLabel 1350 2750 0 60 UnSpc ~ 0\n\
\GLabelPass\n\
\Text HLabel 1350 2950 0 60 Input ~ 0\n\
\HLable\n\
\Text Notes 1800 2050 0 1234 ~ 0\n\
\JustSomeText\n\
\Text GLabel 950 3250 3 60 BiDi Italic 12\n\
\GLabelDownBoldItalicBiDi\n"
  a <- testParser (some parseText <* eof) s
  let s' = concat $ map printText a
  putStr s'
  when (s /= s') $ error "testText mismatch"

parseSchematic :: Parser Sch
parseSchematic = do
  magic <- line $ string "EESchema Schematic File Version 2"
  header <- line $ anyChar `manyTill` string "EELAYER END"
  desc <- parseDescr
  items <- many $ choice
             [ SIC <$> parseComp
             , SIS <$> parseSheet
             , SIB <$> parseBitmap
             , SIL <$> parseLine
             , SIE <$> parseEntry
             , SIM <$> parseMarker
             , SIT <$> parseText
             ]
  void $ line $ string "$EndSCHEMATC"
  eof
  return $ Sch magic (header ++ "EELAYER END") desc items

--------------------------------------------------------------------------------

printSchematic :: Sch -> String
printSchematic (Sch magic header desc items)
 = concat [ magic ++ "\n"
          , header ++ "\n"
          , printDescr desc
          , concat (map printItem items)
          , "$EndSCHEMATIC\n"
          ]

printItem :: SchItem -> String
printItem (SIC i) = printComp   i
printItem (SIS i) = printSheet  i
printItem (SIB i) = printBitmap i
printItem (SIL i) = printLine   i
printItem (SIE i) = printEntry  i
printItem (SIM i) = printMarker i
printItem (SIT i) = printText   i


--------------------------------------------------------------------------------

parseLibField :: Parser LibField
parseLibField = do
  _ <- char 'F'
  i      <- lexInt
  lab    <- lexString
  pos    <- V2 <$> lexMil <*> lexMil
  dim    <- lexMil
  orient <- choice (map char [ 'H', 'V' ]) <* skipSpaces
  vis    <- lexBool "V" "I"

  hjustify <-
    (eol *> return Nothing) <|>
    (Just <$> (choice (map char [ 'L', 'R', 'C', 'B', 'T' ]) <* skipSpaces))

  vjustify <- if hjustify == Nothing
                then return Nothing
                else (eol *> return Nothing) <|>
                     (Just <$> choice (map char [ 'L', 'R', 'C', 'B', 'T' ]))

  style <- if vjustify == Nothing
             then return Nothing
             else (eol *> return Nothing) <|>
                  (Just <$> ((,) <$> choice (map char [ 'I', 'N' ])
                                 <*> choice (map char [ 'B', 'N' ])))

  value <- if style == Nothing
     then return Nothing
     else
       skipSpaces *> optional lexString <* eol

  return $ LibField i lab pos dim orient vis hjustify vjustify style value

parseLibDraw,
  parseLibPoly,
  parseLibRect,
  parseLibCircle,
  parseLibArc,
  parseLibText,
  parseLibPin
    :: Parser LibDraw

parseLibDraw =
    choice [ parseLibPoly
           , parseLibRect
           , parseLibCircle
           , parseLibArc
           , parseLibText
           , parseLibPin
           ]

parseLibPoly = do
  _ <- lexMark "P"
  _ <- lexToken
  unit <- lexInt
  conv <- lexInt
  thick <- lexMil
  coords <- many (V2 <$> lexMil <*> lexMil)
  filled <- optional $ choice (map char "FfN")
  _ <- eol
  return $ LDPolygon unit conv thick coords filled

parseLibRect = do
  _ <- lexMark "S"
  s <- V2 <$> lexMil <*> lexMil
  e <- V2 <$> lexMil <*> lexMil
  unit <- lexInt
  conv <- lexInt
  thick <- lexMil
  filled <- optional $ choice (map char "FfN")
  _ <- eol
  return $ LDRectangle s e unit conv thick filled

parseLibCircle = do
  _ <- lexMark "C"
  pos <- V2 <$> lexMil <*> lexMil
  radius <- lexMil
  unit <- lexInt
  conv <- lexInt
  thick <- lexMil
  filled <- choice (map char "FfN") <* skipSpaces
  _ <- eol
  return $ LDCircle pos radius unit conv thick filled

parseLibArc = do
  _ <- lexMark "A"
  pos <- V2 <$> lexMil <*> lexMil
  radius <- lexMil
  sa <- lexInt
  ea <- lexInt
  unit <- lexInt
  conv <- lexInt
  thick <- lexMil
  filled <- optional $ choice (map char "FfN") <* skipSpaces
  se <- optional $ (,) <$> (V2 <$> lexMil <*> lexMil)
                       <*> (V2 <$> lexMil <*> lexMil)
  _ <- eol
  return $ LDArc pos radius sa ea unit conv thick filled se

parseLibText = do
  _ <- lexMark "T"
  orient <- lexInt
  pos <- V2 <$> lexMil <*> lexMil
  thick <- lexMil
  _ <- lexInt :: Parser Int
  unit <- lexInt
  conv <- lexInt
  text <- lexToken
  style <- optional $ (,,,)
    <$> (string "Italic" <|> string "Normal") <* skipSpaces
    <*> lexBool "1" "0"
    <*> choice (map char "CLR") <* skipSpaces
    <*> choice (map char "CBT")
  _ <- eol
  return $ LDText orient pos thick unit conv text style

-- T 0 60 0 100 0 0 0 1

parseLibPin = do
  _ <- lexMark "X"
  name <- lexToken
  num <- lexToken
  pos <- V2 <$> lexMil <*> lexMil
  len <- lexMil
  orient <- choice $ map char [ 'U', 'D', 'L', 'R' ]
  _ <- skipSpaces
  numsz <- lexMil
  namsz <- lexMil

  unit <- lexInt
  conv <- lexInt

  etype <- choice $ map char "IOBTPUWwCEN"

  _ <- skipSpaces

  visible <- (char 'N' *> return False <* skipSpaces) <|> return True

  shape <- optional $ choice $ map string
             [ "IC", "I", "CL", "C", "L", "V", "F", "X" ]
  _ <- eol

  return $ LDPin name num pos len orient numsz namsz unit conv etype visible shape

parseLibAlias :: Parser [String]
parseLibAlias = do
  _ <- lexMark "ALIAS"
  aliases <- many lexToken
  _ <- eol
  return aliases

parseLibDef:: Parser LibDef
parseLibDef = do
  _ <- lexMark "DEF"
  name <- lexToken
  ref <- lexToken
  _unused <- lexToken
  text_offset <- lexMil
  draw_pinnum <- lexBool "Y" "N"
  draw_pinname <- lexBool "Y" "N"
  unit_count <- lexInt
  units_locked <- (lexMark "L" *> return True) <|> (lexToken *> return False)
  option_flag <- optional $ lexToken
  _ <- eol

  fields <- many parseLibField
  aliases <- fromMaybe [] <$> optional parseLibAlias

  fplist <- optional $ do
    lexBlockMark "FPLIST" *> eol *>
      many (char ' ' *> lexToken <* eol)
      <* lexBlockMark "ENDFPLIST" <* eol


  _ <- lexMark "DRAW"
  _ <- eol
  draw_cmds <- parseLibDraw `manyTill` (lexMark "ENDDRAW" >> eol)

  _ <- lexMark "ENDDEF" >> eol

  return $ LibDef name
                  ref
                  text_offset
                  draw_pinnum
                  draw_pinname
                  unit_count
                  units_locked
                  option_flag
                  fields
                  aliases
                  fplist
                  draw_cmds

parseLib :: Parser Lib
parseLib = do
  _ <- string "EESchema-LIBRARY Version 2."
  _ <- anyChar `manyTill` eol

  lib <- catMaybes <$> many ((Just <$> parseLibDef) <|> (eol *> return Nothing))

  eof

  return lib

test :: IO ()
test = sequence_ [ testDescr
                 , testComp
                 , testSheet
                 , testBitmap
                 , testLine
                 , testEntry
                 , testMarker
                 , testText
                 ]

spaceCat :: Parser Char
spaceCat = charCategory Space

line :: Parser a -> Parser a
line p = p <* eol

skipSpaces :: Parser ()
skipSpaces = void (lookAhead eol) <|> skipSome spaceCat

lexMark :: String -> Parser String
lexMark = symbol skipSpaces

lexBlockMark :: String -> Parser String
lexBlockMark l = lexMark ("$" ++ l)

lexDigits :: Parser String
lexDigits =
  lexeme skipSpaces (some digitChar) <?> "digits"

lexToken :: Parser String
lexToken =
  lexeme skipSpaces (some (alphaNumChar <|> punctuationChar <|> symbolChar))

lexInt :: (Read i, Integral i) => Parser i
lexInt = do
  read <$> (((:) <$> char '-' <*> lexDigits) <|> lexDigits)

lexDouble :: (Read d, RealFloat d) => Parser d
lexDouble = lexeme skipSpaces $ do
  a <- some digitChar <?> "digits"
  _ <- char '.'
  b <- some digitChar <?> "digits"
  return $ read $ a ++ "." ++ b

lexMil :: Parser Mil
lexMil = Mil <$> (lexInt :: Parser Integer)

printMil :: Mil -> String
printMil (Mil i) = show i

lexString :: Parser String
lexString =
  lexeme skipSpaces lexString'

lexString' :: Parser String
lexString' = between (char '"') (char '"') $ many (satisfy (/='"'))

parseKeyValue :: String -> Parser a -> Parser a
parseKeyValue k v = lexMark k *> v <* eol

lexKeyValueEq :: String -> Parser a -> Parser a
lexKeyValueEq k v = lexeme skipSpaces (string (k++"=") *> v)

lexBool :: String -> String -> Parser Bool
lexBool y n = (lexMark y *> return True) <|> (lexMark n *> return True)

testParser :: Show a => Parser a -> String -> IO a
testParser p src = do
  case runParser p "" src of
    Left err -> putStr (parseErrorPretty err) >> error "parse error"
    Right a -> pPrint a >> return a

printString :: String -> String
printString s = "\"" ++ s ++ "\""
