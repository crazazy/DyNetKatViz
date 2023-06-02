module Data.DyNetKat (
    DyNetKat,
    Transition,
    step,
) where

import Data.NetKat

data DyNetKat
    = Prog NetKat DyNetKat
    | Ident String
    | Ask String NetKat DyNetKat
    | Send String NetKat DyNetKat
    | Disj DyNetKat DyNetKat
    | Par DyNetKat DyNetKat
    | Bottom
    deriving (Eq, Show, Read)

data Transition
    = Chg Packet [Packet]
    | TAsk String NetKat
    | TSend String NetKat
    | Rcfg String NetKat

-- | Performs a step in a DyNetKat program with all definitions loaded
step :: [(String, DyNetKat)] -> Packet -> DyNetKat -> [(Transition, DyNetKat)]
step env pa pr = case pr of
    Bottom -> [] -- end of program
    Ident s -> (maybe [] (: []) $ lookup s env) >>= step env pa -- rule 2
    Prog nk nx -> [(Chg pa (mockEval nk pa), nx)] -- rule 1
    Ask s nk nx -> [(TAsk s nk, nx)] -- rule 7
    Send s nk nx -> [(TSend s nk, nx)] -- rule 8
    Disj n1 n2 -> (step env pa n1) ++ (step env pa n2) -- rule 3 + 4
    Par n1 n2 ->
        -- rule 5 + 6 + 9 + 10
        (map (fmap (\x -> Par x n2)) $ step env pa n1)
            ++ (map (fmap (\x -> Par n1 x)) $ step env pa n2)
            ++ checkRcfg n1 n2

-- | Basically rule 9 & 10 from the paper (with symmetry)
checkRcfg :: DyNetKat -> DyNetKat -> [(Transition, DyNetKat)]
checkRcfg (Ask s1 nk1 nx1) (Send s2 nk2 nx2) | s1 == s2 && nk1 == nk2 = [(Rcfg s1 nk1, Par nx1 nx2)]
checkRcfg (Send s1 nk1 nx1) (Ask s2 nk2 nx2) | s1 == s2 && nk1 == nk2 = [(Rcfg s1 nk1, Par nx1 nx2)]
checkRcfg _ _ = [] -- we don't need to take action on the rest since that is handled by `step`
