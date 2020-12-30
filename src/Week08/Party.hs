{-# OPTIONS_GHC -fno-warn-orphans #-}

module Week08.Party where

import qualified Data.List as L
import           Data.Tree (Tree(..))

import Week08.Employee

glCons :: Employee -> GuestList -> GuestList
glCons emp (GL emps fun) = GL (emp:emps) (fun + empFun emp)

instance Semigroup GuestList where
  (GL xs f1) <> (GL ys f2) = GL (xs ++ ys) (f1 + f2)

instance Monoid GuestList where
  mempty = GL [] 0

moreFun :: GuestList -> GuestList -> GuestList
moreFun gl1@(GL _ f1) gl2@(GL _ f2)
  | f1 > f2   = gl1
  | otherwise = gl2

treeFold :: (a -> [b] -> b) -> Tree a -> b
treeFold f t = f (rootLabel t) (treeFold f <$> subForest t)

nextLevel :: Employee -> [(GuestList, GuestList)] -> (GuestList, GuestList)
nextLevel boss glts = (withBoss, withoutBoss)
  where
    withBoss    = glCons boss $ joinGuestLists snd glts
    withoutBoss = joinGuestLists (uncurry moreFun) glts

joinGuestLists :: ((GuestList, GuestList) -> GuestList)
               -> [(GuestList, GuestList)]
               -> GuestList
joinGuestLists chooseList = mconcat . map chooseList

maxFun :: Tree Employee -> GuestList
maxFun = uncurry moreFun . treeFold nextLevel

-- | runhaskell -isrc src/Week08/Party.hs
main :: IO ()
main = readFile "data/week08/company.txt" >>= (printGuestList . read)

printGuestList :: Tree Employee -> IO ()
printGuestList t = do
  putStrLn $ "Total fun: " <> show fun
  putStr   $ unlines empNames
  where 
    GL emps fun = maxFun t
    empNames    = empName <$> L.sortOn empName emps
