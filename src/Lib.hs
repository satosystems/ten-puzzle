module Lib where

import Data.List
  ( nub
  , intersperse
  , permutations
  , sort
  , subsequences
  )
import Data.Maybe
  ( catMaybes
  , fromJust
  )
import Data.Ratio ((%))

operators =
  [ "+"
  , "-"
  , "*"
  , "/"
  ]

makeTen a b c d = catMaybes [walk x y | x <- nub (permutations [a, b, c, d]), y <- nub $ filter (\x -> length x == 3) $ subsequences $ concat $ replicate 3 operators]

(+:) :: Maybe Rational -> Maybe Rational -> Maybe Rational
Nothing +: _ = Nothing
_ +: Nothing = Nothing
Just a +: Just b = Just (a + b)

(-:) :: Maybe Rational -> Maybe Rational -> Maybe Rational
Nothing -: _ = Nothing
_ -: Nothing = Nothing
Just a -: Just b = Just (a - b)

(*:) :: Maybe Rational -> Maybe Rational -> Maybe Rational
Nothing *: _ = Nothing
_ *: Nothing = Nothing
Just a *: Just b = Just (a * b)

(/:) :: Maybe Rational -> Maybe Rational -> Maybe Rational
Nothing /: _ = Nothing
_ /: Nothing = Nothing
_ /: Just 0 = Nothing
Just a /: Just b = Just (a / b)

walk :: [Rational] -> [String] -> Maybe ([Rational], String)
walk x@[a', b', c', d'] y@[o', p', q']
  | ((a `o` b) `p` c) `q` d == Just 10 = Just (x, "((" ++ a'' ++ o' ++ b'' ++ ")" ++ p' ++ c'' ++ ")" ++ q' ++ d'')
  | (a `o` (b `p` c)) `q` d == Just 10 = Just (x, "(" ++ a'' ++ o' ++ "(" ++ b'' ++ p' ++ c'' ++ "))" ++ q' ++ d'')
  | a `o` ((b `p` c) `q` d) == Just 10 = Just (x, a'' ++ o' ++ "((" ++ b'' ++ p' ++ c'' ++ ")" ++ q' ++ d'' ++ ")")
  | a `o` (b `p` (c `q` d)) == Just 10 = Just (x, a'' ++ o' ++ "(" ++ b'' ++ p' ++ "(" ++ c'' ++ q' ++ d'' ++ "))")
  | (a `o` b) `p` (c `q` d) == Just 10 = Just (x, "(" ++ a'' ++ o' ++ b'' ++ ")" ++ p' ++ "(" ++ c'' ++ q' ++ d'' ++ ")")
  | otherwise = Nothing
 where
  a = Just a'
  b = Just b'
  c = Just c'
  d = Just d'
  a'' = [showRational a']
  b'' = [showRational b']
  c'' = [showRational c']
  d'' = [showRational d']
  ops = zip operators [(+:), (-:), (*:), (/:)]
  o = fromJust $ lookup o' ops
  p = fromJust $ lookup p' ops
  q = fromJust $ lookup q' ops

showResult r@[a, b, c, d] = sort $ map showRational r

showRational x | x == (0 % 1) = '0'
               | x == (1 % 1) = '1'
               | x == (2 % 1) = '2'
               | x == (3 % 1) = '3'
               | x == (4 % 1) = '4'
               | x == (5 % 1) = '5'
               | x == (6 % 1) = '6'
               | x == (7 % 1) = '7'
               | x == (8 % 1) = '8'
               | x == (9 % 1) = '9'
               | otherwise = undefined

makeTenAll = filter (not . null) $ map (\(a, b, c, d) -> makeTen a b c d) [(a, b, c, d) | a <- [0..9], b <- [a..9], c <- [b..9], d <- [c..9]]

main' :: IO ()
main' = mapM_ (\xs -> mapM_ (\(r, s) -> putStrLn ((showResult r) ++ ": " ++ s)) xs) makeTenAll

