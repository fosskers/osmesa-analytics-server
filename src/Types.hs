{-# LANGUAGE DeriveGeneric, DeriveAnyClass #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}

module Types ( User(..), LightUser, Campaign(tag), simplify ) where

import           Data.Aeson
import           Data.Foldable (fold)
import           Data.List (groupBy, sort, sortBy)
import           Data.Monoid ((<>))
import           Data.Swagger.Schema
import qualified Data.Text as T
import           Data.Time.Calendar (Day(..))
import           Data.Time.Clock
import           Data.Word
import           GHC.Generics
import           Generic.Random.Generic
import           Test.QuickCheck

---

data Country = Country { country :: T.Text, count :: Word32 } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Monoid Country where
  mempty = Country "" 0
  Country n c1 `mappend` Country _ c2 = Country n (c1 + c2)

instance Arbitrary Country where
  arbitrary = Country <$> cs <*> arbitrary
    where cs = elements [ "AF", "AX", "AL", "DZ", "AS", "AD", "AO", "AI", "AQ", "AG", "AR"
                        , "AM", "AW", "AU", "AT", "AZ", "BS", "BH", "BD", "BB", "BY", "BE"
                        , "BZ", "BJ", "BM", "BT", "BO", "BA", "BW", "BV", "BR", "VG", "IO" ]

data Hashtag = Hashtag { tag :: T.Text, count :: Word32 } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Monoid Hashtag where
  mempty = Hashtag "" 0
  Hashtag n c1 `mappend` Hashtag _ c2 = Hashtag n (c1 + c2)

instance Arbitrary Hashtag where
  arbitrary = Hashtag <$> ts <*> arbitrary
    where ts = elements [ "hotosm", "missingmaps" ]

newtype Name = Name T.Text deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Arbitrary Name where
  arbitrary = (\f l -> Name $ f <> " " <> l) <$> firsts <*> lasts
    where firsts = elements [ "Abe", "Bob", "Charles", "David", "Evan", "Frank", "George"
                            , "Hector", "Issac", "Jack", "Kevin", "Lance", "Morgan", "Nathan"
                            , "Oliver", "Paul", "Quinn", "Roger", "Steve", "Thomas", "Urman"
                            , "Victor", "Wallace", "Xavier", "Yanis", "Zack" ]
          lasts = elements [ "Adams", "Baker", "Clark", "Davis", "Engel", "Farley", "Gallager"
                           , "Hall", "Ingram", "Jones", "King", "Lee", "Manning", "Nelson"
                           , "O'Connor", "Parry", "Quail", "Redmond", "Schneider", "Thompson", "Urban"
                           , "Vallentine", "Wagner", "Xu", "Young", "Zuke" ]

newtype URL = URL T.Text deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Arbitrary URL where
  arbitrary = pure $ URL "https://s3.amazonaws.com/vectortiles/test-vts/peruser-2/piaco_dk/{z}/{x}/{y}.mvt"

data Editor = Editor { tag :: T.Text, count :: Word32 } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Monoid Editor where
  mempty = Editor "" 0
  Editor t c1 `mappend` Editor _ c2 = Editor t (c1 + c2)

instance Arbitrary Editor where
  arbitrary = Editor <$> es <*> arbitrary
    where es = elements [ "josm", "iD" ]

-- | Result of a @/users/{uid}@ call.
data User = User { uid                :: Word32   -- Called `uid` to match OSM.
                 , name               :: Name
                 , extent_uri         :: URL
                 , buildings_add      :: Word32
                 , buildings_mod      :: Word32
                 , roads_add          :: Word32
                 , km_roads_add       :: Double
                 , roads_mod          :: Word32
                 , km_roads_mod       :: Double
                 , waterways_add      :: Word32
                 , km_waterways_add   :: Double
                 , poi_add            :: Word32
                 , changeset_count    :: Word32
                 , edit_count         :: Word32
                 , editors            :: [Editor]
                 , edit_times         :: [UTCTime]
                 , country_list       :: [Country]
                 , hashtags           :: [Hashtag]
                 } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Arbitrary User where
  arbitrary = User
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> fmap abs arbitrary
    <*> arbitrary
    <*> fmap abs arbitrary
    <*> arbitrary
    <*> fmap abs arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> fmap (simplify (tag :: Editor -> T.Text)) arbitrary
    <*> fmap sort (resize 600 $ listOf arbitrary)
    <*> fmap (simplify country) arbitrary
    <*> fmap (simplify (tag :: Hashtag -> T.Text)) arbitrary

simplify :: (Monoid a, Ord b) => (a -> b) -> [a] -> [a]
simplify f xs = map fold . groupBy (\x1 x2 -> f x1 == f x2) $ sortBy (\x1 x2 -> compare (f x1) (f x2)) xs

instance Arbitrary UTCTime where
  arbitrary = (\n m -> UTCTime (ModifiedJulianDay n) (secondsToDiffTime m)) <$> choose (55000, 57500) <*> fmap abs arbitrary

data Distance = Distance { uid :: Word32, distance :: Float } deriving (Eq, Show, Generic, ToJSON)

instance Arbitrary Distance where
  arbitrary = Distance <$> arbitrary <*> fmap abs arbitrary

data LightUser = LightUser { uid        :: Word32
                           , name       :: Name
                           , roads      :: Word32
                           , buildings  :: Word32
                           , changesets :: Word32 } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Arbitrary LightUser where
  arbitrary = genericArbitrarySingle

data Campaign = Campaign { tag                :: T.Text
                         , road_count_add     :: Word32
                         , road_count_mod     :: Word32
                         , building_count_add :: Word32
                         , building_count_mod :: Word32
                         , waterway_count_add :: Word32
                         , poi_count_add      :: Word32
                         , road_km_add        :: Double
                         , road_km_mod        :: Double
                         , waterway_km_add    :: Double
                         , users              :: [LightUser] } deriving (Eq, Show, Generic, ToJSON, ToSchema)

instance Monoid Campaign where
  mempty = Campaign "" 0 0 0 0 0 0 0 0 0 []

  Campaign t rca rcm bca bcm wca pca rka rkm wka us `mappend` Campaign _ rca' rcm' bca' bcm' wca' pca' rka' rkm' wka' us' =
    Campaign t (rca + rca') (rcm + rcm') (bca + bca') (bcm + bcm') (wca + wca') (pca + pca') (rka + rka') (rkm + rkm') (wka + wka') (us <> us')

instance Arbitrary Campaign where
  arbitrary = Campaign
    <$> elements ["hotosm", "missingmaps"]
    <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary <*> arbitrary
    <*> fmap abs arbitrary <*> fmap abs arbitrary <*> fmap abs arbitrary
    <*> arbitrary

time :: Integer -> UTCTime
time n = UTCTime (ModifiedJulianDay n) (secondsToDiffTime 0)
