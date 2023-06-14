module Result.Dot (toGraphviz) where

import Data.DyNetKat
import Data.List
import Data.NetKat

netkat2string :: NetKat -> String
netkat2string Nil = "F"
netkat2string Pass = "T"
netkat2string (Test f v) = "(" <> show f <> " == " <> show v <> ")"
netkat2string (Change f v) = "(" <> show f <> " := " <> show v <> ")"
netkat2string (Not p) = "!(" <> netkat2string p <> ")"
netkat2string (Or p1 p2) = parens $ (netkat2string p1) <> "+" <> (netkat2string p2)
netkat2string (Seq p1 p2) = parens $ (netkat2string p1) <> ";" <> (netkat2string p2)
netkat2string (Closure p) = parens $ "(" <> netkat2string p <> ")*"

dynetkat2string :: DyNetKat -> String
dynetkat2string (Prog p nx) = "((" <> netkat2string p <> ")); " <> dynetkat2string nx
dynetkat2string (Ident i) = i
dynetkat2string (Ask chn p nx) = chn <> "?" <> netkat2string p <> ";" <> dynetkat2string nx
dynetkat2string (Send chn p nx) = chn <> "!" <> netkat2string p <> ";" <> dynetkat2string nx
dynetkat2string (NonDet nx1 nx2) = "(" <> dynetkat2string nx1 <> ") o+ (" <> dynetkat2string nx2 <> ")"
dynetkat2string (Par nx1 nx2) = dynetkat2string nx1 <> " || " <> dynetkat2string nx2
dynetkat2string Bottom = "END"

elToGraphviz :: (DyNetKat, Transition, DyNetKat, [Packet], [Packet]) -> String
elToGraphviz (p1, trans, p2, fut, hist) = show (dynetkat2string p1) <> "->" <> show (dynetkat2string p2) <> "[label=" <> show (show trans) <> "]"

toGraphviz :: [(DyNetKat, Transition, DyNetKat, [Packet], [Packet])] -> String
toGraphviz els = "digraph G {\n" <> unlines result <> "\n}"
  where
    result = nub $ elToGraphviz <$> els
