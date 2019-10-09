{-# OPTIONS_GHC -Wall #-}

module Party where

import           Data.Tree
import           Data.List
import           Text.Read
import           Employee

-- Exercise 1

glCons :: Employee -> GuestList -> GuestList
glCons e (GL es f) = GL (e : es) (f + empFun e)

instance Semigroup GuestList where
  (GL es1 f1) <> (GL es2 f2) = GL (es1 <> es2) (f1 + f2)

instance Monoid GuestList where
  mempty = (GL mempty 0)

moreFun :: GuestList -> GuestList -> GuestList
moreFun = max

-- Exercise 2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f Node { rootLabel = a, subForest = ns } = f a . map (treeFold f) $ ns

-- Exercise 3
getFun :: GuestList -> Fun
getFun (GL _ f) = f

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel e gs = (glCons e . foldMap snd $ gs, foldMap (uncurry moreFun) gs)

-- Exercise 4
maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- Exercise 5
parseCompany :: String -> Maybe (Tree Employee)
parseCompany = readMaybe

formatGuestList :: GuestList -> String
formatGuestList (GL es f) =
  unlines $ ("Total fun: " ++ show f) : (sort . map empName $ es)

handleFailure :: Maybe String -> String
handleFailure (Just x) = x
handleFailure Nothing  = "Failed to load guest list\n"

main :: IO ()
main =
  readFile "../company.txt"
    >>= putStr . handleFailure . (fmap formatGuestList) . (fmap maxFun) . parseCompany
