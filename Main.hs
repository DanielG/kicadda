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


-- {-# OPTIONS_GHC -fdefer-typed-holes #-}
{-# LANGUAGE
  LambdaCase,
  ViewPatterns,
  NamedFieldPuns,
  RecordWildCards,
  ParallelListComp,
  TransformListComp
  #-}
module Main where

import Control.Arrow
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List
import Data.List.Split
import Data.Either
import Data.Maybe
import Data.Tuple
import Text.Megaparsec
import Text.Show.Pretty
import System.Environment
import System.IO
import Interfaces.FZSolutionParser

import Linear
import EESchemaData
import Solver
import Types


roundtrip :: IO ()
roundtrip = do
  f <- getContents
  case runParser parseSchematic "<stdio>" f of
    Left err -> hPutStr stderr (parseErrorPretty err)
    Right a -> putStr $ printSchematic a

doLib :: IO ()
doLib = do
  f <- getContents
  let f' = unlines $ map (\case ('#':_) -> ""; l -> l) $ lines f
  case runParser (parseLib <* eof) "<stdio>" f' of
    Left err -> hPutStr stderr (parseErrorPretty err)
    Right a -> pPrint a


libPins :: IO ()
libPins = do
  f <- getContents
  let f' = unlines $ map (\case ('#':_) -> ""; l -> l) $ lines f
  case runParser (parseLib <* eof) "<stdio>" f' of
    Left err -> hPutStr stderr (parseErrorPretty err)
    Right a -> mapM_ putStrLn $ concat $ map (concatMap (\case LDPin {ldpName} -> [ldpName]; _ -> []) . ldDrawCmds) a

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["roundtrip"] -> roundtrip
    ["lib"]       -> doLib
    ["lib-pins"]  -> libPins

    libf:schf:ref:"--":wanted -> do
--      let clean_regex = ".*\((.*)\)|(.*)"
--          split_regex = "/"

      schc <- readFile schf
      let delComments = unlines . map (\case ('#':_) -> ""; l -> l) . lines
      libc <-  delComments <$> readFile libf
      case do
          lib <- runParser parseLib libf libc
          sch <- runParser parseSchematic "<stdio>" schc
          return (lib,sch) of
        Left err -> hPutStr stderr (parseErrorPretty err)
        Right (lib,sch) -> do
          let
              [comp] = findSchCompRef ref (sItems sch)
              Just def = find ((==scPart comp) . ldName) lib
                     <|> find ((scPart comp `elem`) . ldAliases) lib

              funcalist :: [(PinId, [PinFunction])]
              funcalist =
                Map.toList $ Map.map splitPinFunction $ makePinMap def ldpName

              pinids = map fst funcalist

              idToInt :: [(PinId, Int)]
              idToInt = pinids `zip` [1..]

              intToId :: [(Int, PinId)]
              intToId = map swap idToInt

              revfuncmap :: Map PinFunction (Set Int)
              revfuncmap =
                Map.map (Set.fromList . catMaybes . map (flip lookup idToInt)) $
                  Map.fromListWith (++) $
                    concatMap (\(i,fs) -> map (flip (,) [i]) fs) $
                      funcalist

          let src = case genSolver revfuncmap (Set.fromList wanted) of
                      Left ukn -> error $ "unknown pins: " ++ show ukn
                      Right src -> src

          [sol] <- runSolver (SolverOptions False) src

          let pinalist :: [(PinId, PinFunction)]
              pinalist = catMaybes $ flip map sol $ \(func,MInt i) -> do
                             id' <- lookup i intToId
                             return (id', func)
              pos = scPos comp
              mat = scMatrixToM22 (scMatrix comp)
              rsch = extractRSch sch
              rlib = extractRLibDef def mat pos
              RSch its txts = updatePinLabels rlib rsch (Map.fromList pinalist)
          putStr $ printSchematic sch { sItems = its ++ map SIT txts }

-- TODO: make configurable
splitPinFunction :: PinName -> [PinFunction]
splitPinFunction = splitOn "/"

data RLibDef = RLibDef {
      rldPart   :: Part,
      rldOrient :: Map PinId Int,      -- ^ Orientation
      rldPos    :: Map PinId (V2 Mil)  -- ^ Already transformed
    } deriving (Eq, Ord, Show)

data RSch = RSch [SchItem] [SchText] deriving (Eq, Ord, Show)

type Wanted = [PinFunction]

sPosToV2 (px,py) = V2 px py
scMatrixToM22 [a,b,c,d] = intToMil <$> M22 (V2 a b) (V2 c d)
intToMil i = Mil (fromIntegral i)

extractRLibDef :: LibDef -> M22 Mil -> V2 Mil -> RLibDef
extractRLibDef def mat pos =
    RLibDef
      (ldName def)
      (Map.map (libToSchOrient . oppositePinOrient) $ pinmap ldpOrient)
      (Map.map (\v -> mat ⊙ v + pos) $ pinmap ldpPos)
  where
    pinmap = makePinMap def

makePinMap :: LibDef -> (LibDraw -> a) -> Map PinId a
makePinMap (ldDrawCmds -> ds) f = Map.fromList $ catMaybes $ map go ds
 where
   go pin@LDPin {ldpId} = Just (ldpId, f pin)
   go _ = Nothing

oppositePinOrient :: Char -> Char
oppositePinOrient 'U' = 'D'
oppositePinOrient 'D' = 'U'
oppositePinOrient 'L' = 'R'
oppositePinOrient 'R' = 'L'


-- sch_text.h
libToSchOrient :: Char -> Int
libToSchOrient 'L' = 2
libToSchOrient 'R' = 0
libToSchOrient 'U' = 1
libToSchOrient 'D' = 3

findSchCompRef :: Ref -> [SchItem] -> [SchComp]
findSchCompRef ref its =
    catMaybes $ flip map its $ \case (SIC c) | scRef c == ref -> Just c; _ -> Nothing

extractRSch :: Sch -> RSch
extractRSch (Sch _ _ _ its) = let
    (its', texts) = partitionEithers $ flip map its $
        \case SIT t | stType t /= "Notes" -> Right t
              i                           -> Left i
  in
    RSch its' texts

-- | @updatePins wanted sch@ add/remove/move signal labels connected to pins
-- of the part in 'rsch' such that all pin functions in 'wanted' are connected
-- to a pin which provides this function and are the only label connected to
-- this pin.
updatePinLabels :: RLibDef -> RSch -> Map PinId PinFunction -> RSch
updatePinLabels (RLibDef _part orientm posm) (RSch oits texts) needed = let
    (_, not_connected) =
        partition (\a -> any (coincide a) (Map.elems posm)) texts
    new = Map.map (\(pos, (func, orient)) -> newLabel pos func orient) $
            Map.intersectionWith (,) posm (Map.intersectionWith (,) needed orientm)
  in
    RSch oits (not_connected ++ Map.elems new)

  where
    newLabel p n o = SchText "Label" p o 50 Nothing "~" 0 n
