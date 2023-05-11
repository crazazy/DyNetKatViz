module Data.NetKat where

import Control.Applicative (Alternative (..))
import Data.List (iterate, transpose)

data NetKat a
    = Nil
    | Pass
    | Test (a -> Bool)
    | Not (NetKat a)
    | Change (a -> a)
    | Or [NetKat a]
    | Seq (NetKat a) (NetKat a)
    | Closure (NetKat a)

interpret :: NetKat a -> a -> [a]
interpret Nil _ = mempty
interpret Pass a = pure a
interpret (Test f) a = if f a then pure a else mempty
interpret (Not p) a = if null (interpret p a) then pure a else mempty
interpret (Change f) a = pure $ f a
interpret (Or pas) a = pas >>= flip interpret a
interpret (Seq pa pb) a = interpret pa a >>= interpret pb
interpret (Closure p) a = concat $ takeWhile (not . null) $ (map (flip interpret a) $ iterate (\x -> Seq p x) Pass)

flowtable :: Eq a => [(a, a)] -> NetKat a
flowtable = Or . map (\(a, b) -> Seq (Test (a ==)) (Change (const b)))
