import Data.Vect

data Format = Number Format
            | Dou  Format
            | Chr Format
            | Str Format
            | Lit String Format
            | End

PrintfType : Format -> Type
PrintfType (Number fmt) = (i: Int) -> PrintfType fmt
PrintfType (Dou  fmt) = (f: Double) -> PrintfType fmt
PrintfType (Chr fmt) = (c: Char) -> PrintfType fmt
PrintfType (Str fmt) = (str: String) -> PrintfType fmt
PrintfType (Lit str fmt) = PrintfType fmt
PrintfType End = String

printfFmt : (fmt: Format) -> (acc: String) -> PrintfType fmt
printfFmt (Number fmt) acc = \i => printfFmt fmt (acc ++ show i)
printfFmt (Dou  fmt) acc = \f => printfFmt fmt (acc ++ show f)
printfFmt (Chr fmt) acc = \c => printfFmt fmt (acc ++ show c)
printfFmt (Str fmt) acc = \str => printfFmt fmt (acc ++ str)
printfFmt (Lit lit fmt) acc = printfFmt fmt (acc ++ lit)
printfFmt End acc = acc

toFormat: (xs: List Char) -> Format
toFormat [] = End
toFormat ('%' :: 'd' :: chars) = Number (toFormat chars)
toFormat ('%' :: 'f' :: chars) = Dou  (toFormat chars)
toFormat ('%' :: 'c' :: chars) = Chr (toFormat chars)
toFormat ('%' :: 's' :: chars) = Str (toFormat chars)
toFormat ('%' ::  chars) = Lit "%" (toFormat chars)
toFormat (c ::  chars) = case toFormat chars of
                              Lit lit chars' => Lit (strCons c lit) chars'
                              fmt => Lit (strCons c "") fmt
printf : (fmt: String) -> PrintfType (toFormat (unpack fmt))
printf fmt = printfFmt _ ""

Matrix : Nat -> Nat -> Type
Matrix k j = Vect k (Vect j Double)

testMatrix : Matrix 2 3
testMatrix = [[0, 0, 0], [0, 0, 0]]

TupleVect : Nat -> Type -> Type
TupleVect 0 ty = ()
TupleVect (S k) ty = (ty, TupleVect k ty)

test : TupleVect 4 Nat
test = (1, 2, 3, 4, ())
-- test = (0, (0, (0, (0, ())))) #1

{- 
#1 - C - M a 로 나옴.
 ChatGPT:
다만 책의 표현과 당신이 처음 쓴 표현 사이에는 Idris의 Tuple 문법 설탕(syntactic sugar) 차이가 있습니다.
즉:
(1, 2, 3, 4, ())
은 내부적으로:
(1, (2, (3, (4, ()))))
로 해석됩니다.
그래서 두 표현은 완전히 같은 값입니다.

이번 문제는 앞의 printf와 연결해서 보면 더 재미있습니다.
  printf: 문자열 값 → 함수 타입 생성
  TupleVect: 숫자 값(Nat) → 자료 타입 생성
둘 다 같은 패턴입니다.

- 전엔 어떻게 했는지 모르겠다.
- ChatGPT 첨부 파일 채팅 일시 중지 돼서 새로운 채팅으로 이어가기 중.
-}
