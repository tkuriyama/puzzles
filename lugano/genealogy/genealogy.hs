{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO (readFile)
import Data.List (sortBy)
import Data.Function (on)

type Person = Text.Text
type DOB = Text.Text

data Side = F | M
data Ancestors = Node Ancestors Person DOB Ancestors
               | Leaf
               deriving (Show, Eq, Ord)

instance Show Side where
  show F = "Father"
  show M = "Mother"

-- Genealogy Tree

genTree :: [[Text.Text]] -> Ancestors
genTree xs = foldl f Leaf xs
  where f t x = insert x t

family :: [Text.Text] -> Ancestors -> Bool
family (_:a:_) (Node _ b _ _) = last a' == last b'
  where (a', b') = (Text.words a, Text.words b)
  
insert :: [Text.Text] ->  Ancestors -> Ancestors
insert [] t = t
insert (d:n:l:r:[]) Leaf = Node (leaf l) n d (leaf r)
  where leaf x = Node Leaf x "" Leaf
insert x (Node Leaf n "" Leaf) = insert x Leaf
insert x (Node lt n d rt) 
  | family x lt = Node (insert x lt) n d rt
  | otherwise = Node lt n d (insert x rt)

-- Ancestral Relationships

genList :: Ancestors -> Int -> Side -> [(Person, DOB, String)]
genList Leaf _ _ = []
genList (Node lt n d rt) i s =
  [(n, d, f i s)] ++ genList lt (i+1) F ++ genList rt (i+1) M
  where f 0 _ = "Self"
        f n s = if n == 1 then show s
                  else if n == 2 then "Grand " ++ show s
                  else (unwords $ replicate (n-2) "Great") ++ " Grand " ++ show s
  

ancestors :: Ancestors -> Text.Text -> [(Person, DOB, String)]
ancestors Leaf x = []
ancestors t@(Node lt n _ rt) x
  | n /= x = ancestors lt x ++ ancestors rt x
  | otherwise = genList t 0 F

-- Main

birthday (_, b, _)
  | b == "" = ""
  | otherwise = let (d:m:y:[]) = Text.splitOn "/" b
                in Text.unwords [y, m, d]

main = do
  t <- TextIO.readFile "ancestors.csv"
  let lines = Text.splitOn "\n" t
      records = map (Text.splitOn ",") lines
      tree = genTree $ filter (not . (==) [""]) records
      results = sortBy (compare `on` birthday) $
                ancestors tree "Mario Eco"
  putStrLn $ show tree ++ "\n\n" ++ show results


