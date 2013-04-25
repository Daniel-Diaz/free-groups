
import Data.Monoid
import Control.Arrow
import qualified Data.Sequence as Seq

---------------------------------------------------
--- CHAIN (WORD) CLASS

class ChainC c where
 empty :: c
 letter :: Char -> c
 inverse :: c -> c
 reduce :: c -> c
 chainl :: c -> Int

---------------------------------------------------
--- NOT-REDUCED CHAIN (WORD)

-- | Chain (word).
--
-- * /D/ for /direct/ letters. The letter by itself.
-- * /I/ for /inverse/ letters.
-- * /E/ for empty chain.
data Chain = D Char Chain | I Char Chain | E

instance Show Chain where
 show E = "1"
 show c = showChain c

showChain :: Chain -> String
showChain (D x c) = x : showChain c
showChain (I x c) = x : '\'' : showChain c
showChain _ = []

instance Monoid Chain where
 mempty = E
 mappend (D x c) c' = D x $ mappend c c'
 mappend (I x c) c' = I x $ mappend c c'
 mappend  _      c' = c'

instance ChainC Chain where
 empty = E
 letter x = D x E
 inverse (D x c) = inverse c <> I x E
 inverse (I x c) = inverse c <> D x E
 inverse _       = E
 reduce (D x c) = case c of
                   I y c' -> if x == y then reduce c'
                                       else D x $ reduce c
                   _ -> D x $ reduce c
 reduce (I x c) =
                  case c of
                   D y c' -> if x == y then reduce c'
                                       else I x $ reduce c
                   _ -> I x $ reduce c
 reduce _ = E
 chainl (D _ c) = 1 + chainl c
 chainl (I _ c) = 1 + chainl c
 chainl  _      = 0

instance Eq Chain where
 (==) = chainEq

chainEq :: Chain -> Chain -> Bool
chainEq (D x c) (D y c') = x == y && chainEq c c'
chainEq (I x c) (I y c') = x == y && chainEq c c'
chainEq  E       E       = True
chainEq  _       _       = False

---------------------------------------------------
--- REDUCED CHAIN (WORD)

newtype RChain = R (Seq.Seq (Char,Bool))

chainToRChain :: Chain -> RChain
chainToRChain = R . go Seq.empty
 where
  go rc (D x c) = go ((x,True)  Seq.<| rc) c
  go rc (I x c) = go ((x,False) Seq.<| rc) c
  go rc _       = rc

rChainToChain :: RChain -> Chain
rChainToChain (R c) = go c
 where
  go s =  case Seq.viewl s of
            Seq.EmptyL -> E
            (x,b) Seq.:< r -> if b then D x (go r)
                                   else I x (go r)

-- | Transform a 'Chain' into a reduced 'RChain'.
reduceChain :: Chain -> RChain
reduceChain = chainToRChain . reduce

instance Show RChain where
 show = show . rChainToChain

instance ChainC RChain where
 empty = R $ Seq.empty
 letter c = R $ Seq.singleton (c,True)
 inverse (R c) = R $ Seq.reverse $ fmap (second not) c
 reduce = id
 chainl (R c) = Seq.length c

instance Monoid RChain where
  mempty = empty
  mappend (R c) (R c') =
    case Seq.viewr c of
      Seq.EmptyR -> R c'
      l Seq.:> (x,b) ->
        case Seq.viewl c' of
          Seq.EmptyL -> R c
          (y,b') Seq.:< r -> if x == y && b == not b'
                                then R (l Seq.>< r)
                                else R (c Seq.>< c')
