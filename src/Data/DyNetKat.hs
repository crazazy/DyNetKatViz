module Data.DyNetKat (
    DyNetKat (..),
    Transition,
    step,
    example,
    eval,
    transformIdent,
) where

import Data.NetKat
import Data.Set qualified as Set

data DyNetKat
    = Prog NetKat DyNetKat
    | Ident String
    | Ask String NetKat DyNetKat
    | Send String NetKat DyNetKat
    | NonDet DyNetKat DyNetKat
    | Par DyNetKat DyNetKat
    | Bottom
    deriving (Eq, Show, Read, Ord)

data Transition
    = Chg Packet [Packet]
    | TAsk String NetKat
    | TSend String NetKat
    | Rcfg String NetKat
    deriving (Show, Eq, Read, Ord)

-- changes 1 layer of identifiers to their respective DyNetKat program
transformIdent :: [(String, DyNetKat)] -> DyNetKat -> DyNetKat
transformIdent _ Bottom = Bottom
transformIdent env (Ident s) = maybe Bottom id $ lookup s env
transformIdent env (Prog nk nx) = Prog nk $ transformIdent env nx
transformIdent env (Ask s nk nx) = Ask s nk $ transformIdent env nx
transformIdent env (Send s nk nx) = Send s nk $ transformIdent env nx
transformIdent env (NonDet n1 n2) = NonDet (transformIdent env n1) (transformIdent env n2)
transformIdent env (Par n1 n2) = Par (transformIdent env n1) (transformIdent env n2)

-- | Performs a step in a DyNetKat program with all definitions loaded
step :: [(String, DyNetKat)] -> [Packet] -> DyNetKat -> [(Transition, DyNetKat)]
step env pas pr = case pr of
    Bottom -> [] -- end of program
    Ident s -> (maybe [] (: []) $ lookup s env) >>= step env pas -- rule 2
    Prog nk nx -> case pas of
        [] -> []
        (pa : _) -> [(Chg pa (mockEval nk pa), nx)] -- rule 1
    Ask s nk nx -> [(TAsk s nk, nx)] -- rule 7
    Send s nk nx -> [(TSend s nk, nx)] -- rule 8
    NonDet n1 n2 -> (step env pas n1) ++ (step env pas n2) -- rule 3 + 4
    -- BUG: program doesn't see through Ident's fully yet
    Par n1 n2 ->
        let
            x1 = step env pas n1
            x2 = step env pas n2
         in
            -- rule 5 + 6 + 9 + 10
            (map (fmap (\x -> Par x n2)) x1)
                ++ (map (fmap (\x -> Par n1 x)) x2)
                ++ checkRcfg False x1 x2
                ++ checkRcfg True x2 x1

-- | check if we can emit a reconfiguration based on actions that 2 sides of a parralel composition have done
checkRcfg :: Bool -> [(Transition, DyNetKat)] -> [(Transition, DyNetKat)] -> [(Transition, DyNetKat)]
checkRcfg _ [] _ = []
checkRcfg _ _ [] = []
checkRcfg flipParts ls1@((TAsk s1 p1, nx1) : l1) ls2@((TSend s2 p2, nx2) : l2)
    | s1 == s2 && p1 == p2 =
        [(Rcfg s1 p1, if flipParts then Par nx2 nx1 else Par nx1 nx2)] ++ checkRcfg flipParts l1 ls2 ++ checkRcfg flipParts ls1 l2
checkRcfg flipParts ls1@((TAsk _ _, _) : _) (_ : ls2) = checkRcfg flipParts ls1 ls2
checkRcfg flipParts (_ : ls1) ls2@((TSend _ _, _) : _) = checkRcfg flipParts ls1 ls2
checkRcfg flipParts (_ : ls1) (_ : ls2) = checkRcfg flipParts ls1 ls2

-- | transform a set of definitions and a DyNetKat program into a set of program transition triplets
eval :: [(String, DyNetKat)] -> DyNetKat -> [Packet] -> [(DyNetKat, Transition, DyNetKat, [Packet], [Packet])]
eval env entry [] = do
    (trans, new) <- step env [] entry
    return (entry, trans, new, [], [])
eval env entry (p : packets) = Set.toList $ go Set.empty (mkHistory $ step env packets entry)
  where
    mkHistory transitions = do
        (trans, new) <- transitions
        return $ if isChg trans then (entry, trans, new, packets, [p]) else (entry, trans, new, (p : packets), [])
    -- go returns a set of tuples of the form (old program, transition, new program, future packets, past packets)
    go :: Set.Set (DyNetKat, Transition, DyNetKat, [Packet], [Packet]) -> [(DyNetKat, Transition, DyNetKat, [Packet], [Packet])] -> Set.Set (DyNetKat, Transition, DyNetKat, [Packet], [Packet])
    go transitions [] = transitions
    go transitions newTransitions =
        let
            nextTransitions = do
                (_, _, new, future, history) <- newTransitions
                (trans', new') <- step env future new
                return (new, trans', new', if isChg trans' then tail future else future, if isChg trans' then (head future) : history else history)
         in
            go (Set.union transitions $ Set.fromList newTransitions) $
                filter (\out -> not $ Set.member out transitions) nextTransitions

isChg :: Transition -> Bool
isChg (Chg _ _) = True
isChg _ = False

-- | a simple switch configuration. Port 0 denotes a packet coming from the intranet and port 1 from the internet
example :: [(String, DyNetKat)]
example =
    [ ("Init", Par (Ident "Host") (Ident "Switch"))
    , ("Host", NonDet (Send "secConReq" Pass $ Ident "Host") (Send "secConEnd" Pass $ Ident "Host"))
    ,
        ( "Switch"
        , NonDet (Prog ((Test Port 0) `Seq` (Change Port 1)) $ Ident "Switch") $
            NonDet (Prog ((Test Port 1) `Seq` Nil) $ Ident "Switch") $
                (Ask "secConReq" Pass $ Ident "Switch'")
        )
    ,
        ( "Switch'"
        , NonDet (Prog ((Test Port 0) `Seq` (Change Port 1)) $ Ident "Switch'") $
            NonDet (Prog ((Test Port 1) `Seq` (Change Port 0)) $ Ident "Switch'") $
                (Ask "secConEnd" Pass $ Ident "Switch")
        )
    ]
