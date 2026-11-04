import Data.List

data SnocList : List a -> Type where
     Empty : SnocList []
     Snoc : (x, xs : _) -> (rec : SnocList xs) -> SnocList (xs ++ [x]) -- Snoc : (rec : SnocList xs) -> SnocList (xs ++ [x])

snocListHelp : {input : _} ->
               (snoc : SnocList input) -> (rest : List a) -> SnocList (input ++ rest)
snocListHelp snoc [] = rewrite appendNilRightNeutral input in snoc --?rewriteNil snoc
snocListHelp snoc (x :: xs) = rewrite appendAssociative input [x] xs in (snocListHelp (Snoc x input snoc) xs) -- ?rewriteCons (snocListHelp (Snoc snoc {x}) xs)

snocList : (xs : List a) -> SnocList xs
snocList xs = snocListHelp Empty xs

isSuffix : Eq a => List a -> List a -> Bool
isSuffix input1 input2 with (snocList input1, snocList input2)
  isSuffix _ _ | (Snoc x xs xsrec, Snoc y ys ysrec)
   = (x == y) && (isSuffix _ _ | (xsrec, ysrec))
  isSuffix _ _ | (s, Empty) = False
  isSuffix _ _ | (Empty, s) = True
{- ChatGPT
Main.isSuffix is possibly not terminating
due to call to Main.with block in isSuffix

논리적으로는 문제가 없습니다.
1. 두 리스트의 마지막 원소를 비교합니다.
2. 같으면 각각 마지막 원소를 제거한 나머지 리스트를 비교합니다.
3. xsrec, ysrec은 원래 리스트보다 작은 리스트에 대한 재귀적 View입니다.
isSuffix _ _ | (xsrec, ysrec)

여기서 중요한 점은 실제로 무한 재귀가 발생한다는 뜻이 아니라, Idris가 종료를 증명하지 못했다는 뜻입니다.
정리하면: isSuffix의 재귀 논리는 맞지만, 튜플을 사용하는 with 블록을 통한 재귀 호출에서 Idris2가 감소 관계를 확인하지 못해 total 판정을 보류한 것입니다.
이것은 SnocList 자체가 잘못되었거나, xsrec과 ysrec이 작은 리스트를 나타내지 못한다는 뜻은 아닙니다.
=}
