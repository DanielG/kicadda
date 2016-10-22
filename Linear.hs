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
  DeriveFunctor,
  MultiParamTypeClasses,
  FlexibleInstances,
  GeneralizedNewtypeDeriving,
  StandaloneDeriving,
  FunctionalDependencies,
  KindSignatures
  #-}
module Linear where

import Data.Ratio

data V2 a = V2 !a !a
          deriving (Eq, Ord, Show, Read, Functor)

data V3 a = V3 !a !a !a
          deriving (Eq, Ord, Show, Read, Functor)

-- Row major
data M33 a = M33 !(V3 a) !(V3 a) !(V3 a) deriving (Eq, Ord, Show, Read, Functor)
data M22 a = M22 !(V2 a) !(V2 a) deriving (Eq, Ord, Show, Read, Functor)

instance Num a => Num (V2 a) where
    (V2 a b) * (V2 a' b') = V2 (a * a') (b * b')
    (V2 a b) + (V2 a' b') = V2 (a + a') (b + b')
    negate (V2 a b) = V2 (negate a) (negate b)
    abs (V2 a b) = V2 (abs a) (abs b)
    signum (V2 a b) = V2 (signum a) (signum b)
    fromInteger i = V2 (fromInteger i) (fromInteger i)

instance Fractional a => Fractional (V2 a) where
    (V2 a b) / (V2 a' b') = V2 (a / a') (b / b')
    recip (V2 a b) = V2 (recip a) (recip b)
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Num a => Num (V3 a) where
    (V3 a b c) * (V3 a' b' c') = V3 (a * a') (b * b') (c * c')
    (V3 a b c) + (V3 a' b' c') = V3 (a + a') (b + b') (c * c')
    negate (V3 a b c) = V3 (negate a) (negate b) (negate c)
    abs (V3 a b c) = V3 (abs a) (abs b) (abs c)
    signum (V3 a b c) = V3 (signum a) (signum b) (signum c)
    fromInteger i = V3 (fromInteger i) (fromInteger i) (fromInteger i)

instance Num a => Num (M22 a) where
    M22 r1 r2 * M22 r1' r2' = M22 (r1 * r1') (r2 * r2')
    M22 r1 r2 + M22 r1' r2' = M22 (r1 + r1') (r2 + r2')
    negate (M22 r1 r2) = M22 (negate r1) (negate r2)
    abs (M22 r1 r2) = M22 (abs r1) (abs r2)
    signum (M22 r1 r2) = M22 (signum r1) (signum r2)
    fromInteger i = M22 (fromInteger i) (fromInteger i)

instance Num a => Num (M33 a) where
    M33 r1 r2 r3 * M33 r1' r2' r3' = M33 (r1 * r1') (r2 * r2') (r3 * r3')
    M33 r1 r2 r3 + M33 r1' r2' r3' = M33 (r1 + r1') (r2 + r2') (r3 + r3')
    negate (M33 r1 r2 r3) = M33 (negate r1) (negate r2) (negate  r3)
    abs (M33 r1 r2 r3) = M33 (abs r1) (abs r2) (abs r3)
    signum (M33 r1 r2 r3) = M33 (signum r1) (signum r2) (signum r3)
    fromInteger i = M33 (fromInteger i) (fromInteger i) (fromInteger i)


instance Fractional a => Fractional (V3 a) where
    (V3 a b c) / (V3 a' b' c') = V3 (a / a') (b / b') (c / c')
    recip (V3 a b c) = V3 (recip a) (recip b) (recip c)
    fromRational r = fromInteger (numerator r) / fromInteger (denominator r)

instance Foldable V2 where
    foldr f i (V2 a b) = f b (f a i)

instance Foldable V3 where
    foldr f i (V3 a b c) = f c (f b (f a i))

class Cross a where
    (×) :: a -> a -> a

instance Num a => Cross (V3 a) where
    V3 u1 u2 u3 × V3 v1 v2 v3 =
        V3 (u2 * v3 - u3 * v2) (u3 * v1 - u1 * v3) (u1 * v2 - u2 * v1)


class DotProd s v | v -> s where
    (·) :: v -> v -> s

instance Num s => DotProd s (V3 s) where
    (V3 a b c) · (V3 a' b' c') = a * a' + b * b' + c * c'

instance Num s => DotProd s (V2 s) where
    (V2 a b) · (V2 a' b') = a * a' + b * b'


class MatMul (m :: *) s (v :: * -> *) where
    (⊙) :: m -> v s -> v s

instance Num s => MatMul (M22 s) s V2 where
    (M22 r1 r2) ⊙ v = V2 (r1 · v) (r2 · v)

instance Num s => MatMul (M33 s) s V3 where
    (M33 r1 r2 r3) ⊙ v = V3 (r1 · v) (r2 · v) (r3 · v)


class Det m s where
    det :: m -> s

instance Num s => Det (M33 s) s where
    det (M33 (V3 a b c) (V3 d e f) (V3 g h i)) =
        a*e*i + b*f*g + c*d*h - c*e*g - b*d*i - a*f*h

instance Num s => Det (M22 s) s where
    det (M22 (V2 a b) (V2 c d)) = a*d - b*c


class Vector s v where
    vabs :: v s -> s

instance Floating s => Vector s V3 where
    vabs (V3 x y z) = sqrt (x*x + y*y + z*z)

instance Floating s => Vector s V2 where
    vabs (V2 x y) = sqrt (x*x + y*y)
