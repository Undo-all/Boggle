{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Extra
import Data.List
import Data.Monoid
import Control.Monad
import System.Random
import Data.Trie (Trie)
import Data.Vector (Vector)
import Data.ByteString.Builder
import qualified Data.Trie as T
import qualified Data.Vector as V
import Data.ByteString (ByteString)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as L

type Dictionary = Trie ()

getDictionary :: IO Dictionary
getDictionary = 
    let dict = BS.readFile "/usr/share/dict/words"
    in T.fromList . zipUnit . filter good . BS.split 10 <$> dict
  where good s = BS.length s <= 16 && BS.all (`elem` [97..122]) s
        zipUnit xs = zip xs (repeat ())

-- ...maybe not the BEST representation.
data Space = A
           | B
           | C
           | D
           | E
           | F
           | G
           | H
           | I
           | J
           | K
           | L
           | M
           | N
           | O
           | P
           | Qu
           | R
           | S
           | T
           | U
           | V
           | W
           | X
           | Y
           | Z
           deriving (Eq, Enum, Bounded, Read) 

instance Show Space where
    show s = case s of
        Qu -> "Qu"
        _  -> [['A'..'Z'] !! fromEnum s]

instance Random Space where
    randomR (x,y) g = let (n, g') = randomR (fromEnum x, fromEnum y) g
                      in (toEnum n, g')

    random g = randomR (minBound :: Space, maxBound :: Space) g

newtype Board = Board { getBoard :: Vector Space }

spaceBuilder :: Space -> Builder
spaceBuilder s = case s of
    Qu -> byteString "qu"
    _  -> word8 ([97..122] !! fromEnum s)

instance Show Board where
    show (Board v) = init (showBoard 3 v)
        where showBoard n v
                  | V.null v  = ""
                  | n == 0    = show (V.head v) ++ "\n" ++ showBoard 3 (V.tail v)
                  | otherwise =
                    let x = V.head v
                        s = if x == Qu then " " else "  "
                    in show x ++ s ++ showBoard (n-1) (V.tail v)

infix 9 !
(!) :: Board -> (Int, Int) -> Builder
(!) (Board v) (x,y) = spaceBuilder (v V.! (x + 4*y))

randomBoard :: IO Board
randomBoard = Board <$> V.replicateM 16 randomIO

inBounds :: (Int, Int) -> Bool
inBounds (x,y) = x < 4 && y < 4 && x >= 0 && y >= 0

directions :: [(Int, Int)] -> (Int, Int) -> [(Int, Int)]
directions history (x,y) = filter good places
  where good pos = inBounds pos && pos `notElem` history
        places   = [ (x+1, y)
                   , (x-1, y)
                   , (x, y+1)
                   , (x, y-1)
                   , (x+1, y+1)
                   , (x-1, y-1)
                   , (x+1, y-1)
                   , (x-1, y+1)
                   ]

uncurry3 :: (a -> b -> c -> d) -> (a, b, c) -> d
uncurry3 f (x,y,z) = f x y z

findPaths :: Dictionary -> Board -> (Int, Int) -> [Builder]
findPaths dict b pos =
    concatMap firstIter (directions [] pos)
  where firstIter p = case directions [pos] p of
            [] -> []
            xs -> let nstr = (b ! pos) <> (b ! p)
                      strs = map ((nstr <>) . (b !)) xs
                      hist = map (: [pos, p]) xs
                  in strs ++ concatMap (uncurry3 (findPaths' dict b)) 
                                       (zip3 hist strs xs)

findPaths' :: Dictionary -> Board -> [(Int, Int)] -> Builder -> (Int, Int) -> [Builder]
findPaths' dict b history str pos = case directions history pos of
    [] -> [str]
    xs ->
      if T.null (T.submap (L.toStrict (toLazyByteString str)) dict)
        then []
        else let strs = map ((str <>) . (b !)) xs
                 hist = map (: history) xs
             in strs ++ concatMap (uncurry3 (findPaths' dict b)) 
                                  (zip3 hist strs xs)

findWords :: Dictionary -> Board -> [ByteString]
findWords dict b =
    nubOrd (filter isWord (map toStr (concatMap (findPaths dict b) spaces)))
  where spaces = [(x,y) | x <- [0..3], y <- [0..3]]
        toStr  = L.toStrict . toLazyByteString
        isWord = (`T.member` dict)

main :: IO ()
main = do
    dict <- getDictionary
    putStrLn "Enter boggle board:"
    b <- Board . V.fromList . map read . concatMap words <$> replicateM 4 getLine
    putStrLn "Your board: "
    print b
    putStrLn "Finding words..."
    mapM_ BS.putStrLn (findWords dict b)

