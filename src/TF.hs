{-# LANGUAGE AllowAmbiguousTypes       #-}
{-# LANGUAGE ConstraintKinds           #-}
{-# LANGUAGE DataKinds                 #-}
{-# LANGUAGE FlexibleContexts          #-}
{-# LANGUAGE FlexibleInstances         #-}
{-# LANGUAGE InstanceSigs              #-}
{-# LANGUAGE KindSignatures            #-}
{-# LANGUAGE MultiParamTypeClasses     #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE PolyKinds                 #-}
{-# LANGUAGE RankNTypes                #-}
{-# LANGUAGE ScopedTypeVariables       #-}
{-# LANGUAGE TypeApplications          #-}
{-# LANGUAGE TypeFamilies              #-}



module TF where

import           Data.Time (NominalDiffTime, UTCTime)
import qualified Data.Time as Time


class Add a b where
    type Res a b
    add :: a -> b -> Res a b

instance Add NominalDiffTime NominalDiffTime where
    type Res NominalDiffTime NominalDiffTime = NominalDiffTime
    add = (+)

instance Add UTCTime NominalDiffTime where
    type Res UTCTime NominalDiffTime = UTCTime
    add = flip Time.addUTCTime

instance Add NominalDiffTime UTCTime where
    type Res NominalDiffTime UTCTime = UTCTime
    add = Time.addUTCTime



data LT
data GT
data EQ

instance Show LT where
    show _ = "LT"

instance Show GT where
    show _ = "GT"

instance Show EQ where
    show _ = "EQ"

type family TCompare a b

type instance TCompare Int   Int   = EQ
type instance TCompare Float Int   = GT
type instance TCompare Int   Float = LT
type instance TCompare Float Float = EQ

tcompare :: forall a b c. (c ~ TCompare a b, Show c) => a -> b -> String
tcompare _ _ = show (undefined :: c)

tcompare' :: forall a b c. (c ~ TCompare a b, Show c) => String
tcompare' = show (undefined :: c)

poly :: (forall a. a -> a) -> Bool
poly f = (f 0 < 1) == f True
