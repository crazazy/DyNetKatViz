module Data.NetKat where

-- import Control.Applicative (Alternative (..))
import Data.Word (Word8)

data Packet = Packet
    { src :: Word8
    , dest :: Word8
    , port :: Word8
    , typ :: Word8
    }
    deriving (Show, Read, Eq, Ord)
data Filter
    = Src
    | Dest
    | Port
    | Typ
    deriving (Show, Read, Eq, Ord)

toBinary :: Word8 -> String
toBinary x = go 8 x
  where
    go :: Int -> Word8 -> String
    go 0 _ = []
    go y x' = (if x' `mod` 2 == 1 then '1' else '0') : (go (y - 1) $ div x' 2)

-- turns a packet into a binary vector
-- >>> pToBV (Packet 4 2 23 1)
pToBV :: Packet -> String
pToBV (Packet src dest port typ) =
    toBinary src
        ++ toBinary dest
        ++ toBinary port
        ++ toBinary typ
  where

filterToBV :: Filter -> Word8 -> String
filterToBV Src x = toBinary x ++ replicate 24 '?'
filterToBV Dest x = replicate 8 '?' ++ toBinary x ++ replicate 16 '?'
filterToBV Port x = replicate 16 '?' ++ toBinary x ++ replicate 8 '?'
filterToBV Typ x = replicate 24 '?' ++ toBinary x

data NetKat
    = Nil
    | Pass
    | Test Filter Word8
    | Not NetKat
    | Change Filter Word8
    | Or NetKat NetKat
    | Seq NetKat NetKat
    | Closure NetKat
    deriving (Eq, Show, Read, Ord)
mockEval :: NetKat -> Packet -> [Packet]
mockEval Nil _ = []
mockEval Pass x = [x]
mockEval (Test f w) p =
    let
        fp = case f of
            Src -> src
            Dest -> dest
            Port -> port
            Typ -> typ
     in
        if fp p == w then [p] else []
mockEval (Not n) p = if null (mockEval n p) then [p] else []
mockEval (Change f w) p = case f of
    Src -> [p{src = w}]
    Dest -> [p{dest = w}]
    Port -> [p{port = w}]
    Typ -> [p{typ = w}]
mockEval (Or n1 n2) p = mockEval n1 p ++ mockEval n2 p
mockEval (Seq n1 n2) p = mockEval n1 p >>= mockEval n2
mockEval (Closure n) p = go Pass []
  where
    go s xs
        | null as = xs -- we stop when we find no new packes upon more iterations
        | otherwise = go (Seq n s) (as ++ xs)
      where
        as = filter (not . flip elem xs) $ mockEval s p

flowTable :: [(Word8, Word8)] -> NetKat
flowTable ft = foldr Or Nil $ map (\(a, b) -> (Test Port a) `Seq` (Change Port b)) ft

parens :: String -> String
parens s = "(" ++ s ++ ")"

toKatBV :: NetKat -> String
toKatBV Nil = "F"
toKatBV Pass = "T"
toKatBV (Test f w) = parens $ "x=" ++ filterToBV f w
toKatBV (Not p) = parens $ '~' : (toKatBV p)
toKatBV (Change f w) = parens $ "x:=" ++ filterToBV f w
toKatBV (Or p1 p2) = parens $ (toKatBV p1) ++ "+" ++ (toKatBV p2)
toKatBV (Seq p1 p2) = parens $ (toKatBV p1) ++ ";" ++ (toKatBV p2)
toKatBV (Closure p) = parens $ "(" ++ toKatBV p ++ ")*"

{-
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
-}
