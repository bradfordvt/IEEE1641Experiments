import Data.Bits
import Control.Monad
get_bit :: Integer -> Integer
get_bit seed = ((seed `shiftR` 0) `xor` (seed `shiftR` 1)) .&. 1

bit_string :: Integer -> String
bit_string seed = show (get_bit seed)

lfsr :: Integer -> Integer
lfsr seed = (seed `shiftR` 1) .|. ((get_bit seed) `shiftL` 10) .&. 0x7FF

gen_pattern :: Integer -> String -> String
gen_pattern seed pattern = do
    (show (get_bit seed)) ++ pattern

lfsr2 :: Integer -> Integer
lfsr2 seed = seed

prbs :: Integer -> Integer -> String -> String
prbs seed latch pattern = do
    let new_val = lfsr latch
    if new_val == seed
        then (show (get_bit seed))
        else (gen_pattern new_val (prbs seed new_val pattern))

prbs2 :: Integer -> Integer -> [Integer]
prbs2 seed latch = do
    let new_val = lfsr latch
    -- PutStrLn (show new_val)
    if new_val == seed
        then [get_bit seed]
        else (get_bit new_val) : (prbs2 seed new_val)

main = do
    putStrLn (bit_string 2)
    putStrLn (gen_pattern 2 "")
    putStrLn (show (lfsr 2))
    putStrLn (show (prbs2 2 2))
    putStrLn (show (prbs 2 2 ""))
