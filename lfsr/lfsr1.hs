import Data.Bits
import Control.Monad
get_bit :: Integer -> Integer
get_bit seed = ((seed `shiftR` 0) `xor` (seed `shiftR` 1)) .&. 1

bit_string :: Integer -> String
bit_string seed = show (get_bit seed)

lfsr :: Integer -> Integer -> Integer
lfsr seed feedback = (seed `shiftR` 1) .|. (feedback `shiftL` 10) .&. 0x7FF

gen_pattern :: Integer -> String -> String
gen_pattern seed pattern = do
    (show (get_bit seed)) ++ pattern

prbs :: Integer -> Integer -> String -> String
prbs seed latch pattern = do
    let new_val = lfsr latch (get_bit latch)
    if new_val == seed
        then (show (get_bit seed))
        else (gen_pattern new_val (prbs seed new_val pattern))

main = do
    putStrLn (show (prbs 2 2 ""))
