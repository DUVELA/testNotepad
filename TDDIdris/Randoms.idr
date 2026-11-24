import Data.Bits

export
randoms : Int -> Stream Int
randoms seed = let seed' = 1664525 * seed + 1013904223 in
                   (seed' `shiftR` 2) :: randoms seed'

--책의 진도, ex_11_1.idr의 3번 문제에서 사용.
