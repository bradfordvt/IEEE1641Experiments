module Pure where
import FFT
import Complex
import Random
import Physical
infixr 7 |>

import Data.Bits
import Control.Monad

class Signal s where
    mapSignal:: (Physical a, Physical b) => (s a b) -> a -> b
    mapSigList:: (Physical a, Physical b) => (s a b) -> [a] -> [b]
    toSig:: (Physical a, Physical b) => (s a b) -> SignalRep a b
    isInactive:: (Physical a, Physical b) => (s a b)-> a -> Bool
    isOff:: (Physical a, Physical b) => (s a b)-> a -> Bool
    isZ:: (Physical a, Physical b) => (s a b)-> a -> Bool
    isX:: (Physical a, Physical b) => (s a b)-> a -> Bool
    isL:: (Physical a, Physical b) => (s a b)-> a -> Bool
    isH:: (Physical a, Physical b) => (s a b)-> a -> Bool
    mapSignal = mapSignal . toSig
    mapSigList = map . mapSignal
    toSig = FunctionRep . mapSignal
    isInactive = isInactive . toSig
    isOff = isOff . toSig
    isZ s t = (isInactive s t) && (isOff s t)
    isX s t = not (isInactive s t) && (isOff s t)
    isL s t = (isInactive s t) && not (isOff s t)
    isH s t = not (isInactive s t) && not (isOff s t)
    
data SignalRep a b =
    ZRep |
    XRep |
    NullRep |
    FunctionRep (a -> b) |
    PieceContRep (PieceCont a b)
    
instance Signal SignalRep where
    mapSignal ZRep = \t -> toPhysical (-0.0)
    mapSignal XRep = \t -> toPhysical 0.0
    mapSignal NullRep = \t -> toPhysical (-0.0)
    mapSignal (FunctionRep f) = f
    mapSignal (PieceContRep f) = mapSignal f
    mapSigList (FunctionRep f) = map f
    mapSigList (PieceContRep f) = mapSigList f
    toSig = id
    isInactive ZRep _ = True
    isInactive NullRep _ = True
    isInactive (PieceContRep f) t = isInactive f t
    isInactive _ _ = False
    isOff ZRep _ = True
    isOff XRep _ = True
    isOff (PieceContRep f) t = isOff f t
    isOff _ _ = False

get_bit :: Integer -> Integer
get_bit seed = ((seed `shiftR` 0) `xor` (seed `shiftR` 1)) .&. 1

bit_string :: Integer -> String
bit_string seed = show (get_bit seed)

lfsr :: Integer -> Integer -> Integer
lfsr seed feedback = (seed `shiftR` 1) .|. (feedback `shiftL` 10) .&. 0x7FF

lsfr :: (Physical tapndx0, Physical tapndx1, Physical width, Signal clk, Signal seed, Signal feedback) =>
    tap0 -> tap1 -> width -> clk -> seed -> feedback -> ((SignalRep a b), (SignalRep a b))
lsfr tapndx0 tapndx1 width clk seed feedback =