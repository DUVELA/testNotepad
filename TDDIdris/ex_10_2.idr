import Data.List
import Data.List.Views
import Data.List.Views.Extra
import Data.Nat.Views

equalSuffix : Eq a => List a -> List a -> List a
equalSuffix fst snd with (snocList fst, snocList snd)
  equalSuffix _ _ | (Snoc x xs xsrec, Snoc y ys ysrec) = 
   if x == y
      then snoc (equalSuffix _ _ | (xsrec, ysrec) ) x
      else []
  equalSuffix _ _ | (Empty, s) = []
  equalSuffix _ _ | (s, Empty) = []

toBinary : Nat -> String
toBinary k with (halfRec k)
  toBinary 0 | HalfRecZ = ""
  toBinary (n + n) | (HalfRecEven n rec) = toBinary n ++ "0"
  toBinary (S (n + n)) | (HalfRecOdd n rec) =  toBinary n ++ "1"

palindrome : Eq a => List a -> Bool
palindrome xs with (vList xs)
  palindrome [] | VNil = True
  palindrome [x] | VOne = True
  palindrome (x :: (xs ++ [y])) | (VCons rec) = if x == y then palindrome xs else False

  
{- 
Main> equalSuffix [] []
Error: Can't find an implementation for Eq ?a.

(Interactive):1:1--1:18
 1 | equalSuffix [] []
List [] 에 대한 a. Ttype을 알 수 없어서 나는 에러라고 한다.
ChatGPT가 알려준 대로 한 확인
Main> :let duv = the (List Int) []
Main> :let ddt = the (List Int) []
Main> equalSuffix ddt duv
[]
또는
Main> equalSuffix (the (List Int) []) []
[]
아니면
Main> equalSuffix {a = Int} [] []
[]
Idris2는 현재처럼 두 View를 튜플로 묶어 하나의 with에 전달하고, 그 with 블록을 통해 재귀 호출하는 구조에서 감소 관계를 확인하지 못한 것입니다.

이 점은 앞서 mergeSort에서 공부했던 SplitRec와 연결됩니다. 재귀적 View를 사용한다고 해서 모든 형태의 with 재귀를 Totality Checker가 자동으로 인정하는 것은 아닙니다.
- Eq ?a: 빈 리스트만으로는 원소 타입을 추론할 수 없어서 생기는 타입 추론 문제입니다.
- possibly not terminating: 재귀 호출이 작아진다는 사실을 Totality Checker가 확인하지 못해서 생기는 문제입니다.
-}

{- palindrome
PS ...\chapter10> idris2 .\ex_10_2.idr
     ____    __     _         ___
    /  _/___/ /____(_)____   |__ \
    / // __  / ___/ / ___/   __/ /     Version 0.8.0-214eb4547
  _/ // /_/ / /  / (__  )   / __/      https://www.idris-lang.org
 /___/\__,_/_/  /_/____/   /____/      Type :? for help

Welcome to Idris 2.  Enjoy yourself!
Error: Module Data.List.Views.Extra not found
Main> :q
Bye for now!
PS ...s\chapter10> idris2 -p contrib ex_10_2.idr
     ____    __     _         ___
    /  _/___/ /____(_)____   |__ \
    / // __  / ___/ / ___/   __/ /     Version 0.8.0-214eb4547
  _/ // /_/ / /  / (__  )   / __/      https://www.idris-lang.org
 /___/\__,_/_/  /_/____/   /____/      Type :? for help

Welcome to Idris 2.  Enjoy yourself!
Main>  palindrome (unpack "abccba")
True
Main> palindrome (unpack "abcba")
True
Main> palindrome (unpack "abcb")
False

chapter10.ipkg를 만들어서 아래 내용을 넣고 돌림.
package chapter10

sourcedir = "."
depends = contrib
다른 방법 msys는 alias 를 이용하기 'idris2 -p contrib'
windows는 Windows 10의 doskey 사용.
-}
