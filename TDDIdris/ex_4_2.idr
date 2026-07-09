import Data.Vect

vectTake : (m : Nat) -> Vect (m + n) a -> Vect m a
vectTake 0 _ = []
vectTake (S k) (x :: xs) = x :: vectTake k xs

--바뀐 점https://idris2.readthedocs.io/en/latest/typedd/typedd.html#chapter-4
sumEntries : Num a => {n : _} -> (pos : Integer) -> Vect n a -> Vect n a -> Maybe a
sumEntries pos xs ys = case integerToFin pos n of
  Nothing => Nothing
  (Just idx) => Just (index idx xs + index idx ys)

{- vectTake chatGTP 답변
vectTake : (m : Nat) -> Vect (n + m) a -> Vect m a
처럼 n + m으로 적었다면, Idris는
n + S k
를 자동으로 S (...)로 줄이지 못합니다.
그러면 컴파일러는
"혹시 빈 벡터일 수도 있지 않나?"
라고 생각해서 [] 패턴을 요구합니다.

vectTake : (m : Nat) -> Vect (m + n) a -> Vect m a
라면
(S k) + n
은
S (k + n)
으로 계산되지만,
n + (S k)
은 첫 번째 인자가 변수 n 이라서 더 이상 계산할 수 없습니다.

이걸 처음 접하면 "덧셈은 교환법칙이 성립하는데 왜 m + n과 n + m이 다르지?"라는 의문이 거의 반드시 듭니다. 
수학적으로는 같지만, Idris는 계산(정의에 의한 축약)이 가능한 식만 자동으로 줄여 줍니다. 
m + n = n + m이라는 교환법칙은 증명해야 하는 정리이지, 정의 자체는 아니기 때문입니다.
  -}
