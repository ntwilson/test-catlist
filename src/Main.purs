module Main where

import Prelude

import Data.Array (foldMap, (..))
import Data.CatList (CatList)
import Data.CatList as Cat
import Data.Foldable (class Foldable, foldl)
import Data.Int as Int
import Effect (Effect)
import Effect.Console (log)
import Math (sqrt)
import Performance.Minibench (bench, benchWith)

foreign import arrayFromFoldableImpl :: ∀ a. ((Array a -> a -> Array a) -> Array a) -> Array a

arrayFromFoldable :: ∀ f. Foldable f => f ~> Array
arrayFromFoldable xs = arrayFromFoldableImpl (\fn -> foldl fn [] xs)

catOfSize :: Int -> CatList Int
catOfSize n = 
  -- many lists of many elements
  let i = Int.floor $ sqrt $ Int.toNumber n
  in (1 .. i) # foldMap (const (Cat.fromFoldable (1 .. i)))

  -- many lists of single elements
  -- (1 .. n) # foldMap (const (Cat.singleton n))

  -- one list of many elements
  -- Cat.fromFoldable (1 .. n)
    

cat1_000 :: CatList Int
cat1_000 = catOfSize 1_000
cat10_000 :: CatList Int
cat10_000 = catOfSize 10_000
cat100_000 :: CatList Int
cat100_000 = catOfSize 100_000
cat1_000_000 :: CatList Int
cat1_000_000 = catOfSize 1_000_000

main :: Effect Unit
main = do
  log "for 1,000:"
  bench \_ -> arrayFromFoldable cat1_000
  log "for 10,000:"
  bench \_ -> arrayFromFoldable cat10_000
  log "for 100,000:"
  benchWith 200 \_ -> arrayFromFoldable cat100_000
  log "for 1,000,000:"
  benchWith 50 \_ -> arrayFromFoldable cat1_000_000
