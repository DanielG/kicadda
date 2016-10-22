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

{-# LANGUAGE RecordWildCards #-}
module Solver where

import Control.Arrow
import Control.Monad
import Data.List
import Data.List.Split
import Data.Function
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Interfaces.FZSolutionParser
import Text.EditDistance
import Text.Encoding.Z
import System.Environment
import System.Process
import System.IO
import System.IO.Temp

import Types

genSolver :: Map PinFunction (Set Int)
          -> Set PinFunction
          -> Either (Set PinFunction) String
genSolver funcmap wanted = let
    funcs_set = Map.keysSet funcmap
    unknowns = wanted `Set.difference` Map.keysSet funcmap
    edwanted =
        (Set.toList wanted)
        & selfcrossprod
        & map (uncurry (levenshteinDistance defaultEditCosts))
    edwantedmatrix =
        edwanted
        & chunksOf (Set.size wanted)
        & matrix
    vars = funcmap
      & Map.mapWithKey (\func pins ->
          "var " ++ set pins ++ ": " ++ zEncodeString func ++ "; % " ++ func)
      & flip Map.restrictKeys wanted
    array xs = "[ " ++ intercalate ", " xs ++ " ]"
    set xs = "{ " ++ intercalate ", " (map show $ Set.toList xs) ++ " }"
    matrix xs = concat
      [ "[| "
      , intercalate " |\n   " $
        map (intercalate ", " . map show) xs
      , " |]"
      ]
  in
    case Set.toList unknowns of
      (_:_) -> Left unknowns
      [] -> Right $ unlines $ concat
        [ ["include \"alldifferent.mzn\";"]
        , Map.elems vars
        , ["array[int,int] of int: edit_distance = \n"
           ++ edwantedmatrix ++ ";"]
        , ["array[int] of var int: wanted = "
           ++ (array $ map zEncodeString $ Set.toList wanted) ++ ";"]
        , ["\
           \var int: distance =\n\
           \  sum (i in 1..length(wanted), j in 1..length(wanted) where i > j)\n\
           \      (edit_distance[i, j] + abs(wanted[i] - wanted[j]));\n\
           \\
           \constraint alldifferent(wanted);\n\
           \solve minimize distance;\n"]
        ]
  where
    crossprod xs ys = [ (x, y) | x <- xs, y <- ys ]
    selfcrossprod xs = crossprod xs xs

data SolverOptions = SolverOptions {
      soDebug :: Bool
    } deriving (Eq, Ord, Read, Show)

runSolver :: SolverOptions -> String -> IO [Solution]
runSolver SolverOptions {..} mzn = do
  let args = [ "--input-from-stdin"
             , "--output-fzn-to-stdout"
             , "--no-output-ozn"
             ]

  when soDebug $ hPutStrLn stderr $ unlines [ mzn, "----" ]
  fzn <- readProcess "mzn2fzn" args mzn
  when soDebug $ hPutStrLn stderr $ unlines [ fzn, "----" ]
  sol <- withSystemTempFile "eezinc.fzn" $ \f hdl -> do
    hPutStrLn hdl fzn >> hClose hdl
    readProcess "fzn-gecode" [f] fzn
  when soDebug $ hPutStrLn stderr $ unlines [ sol, "----" ]
  let Right res = getSolution 1000 sol
  when soDebug $ hPutStrLn stderr $ unlines [ show res, "----" ]
  return $ map (map (first zDecodeString)) res
