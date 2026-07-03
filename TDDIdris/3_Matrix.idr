import Data.Vect

createEmpties : {n : _} -> Vect n (Vect 0 a)
createEmpties {n} = replicate n []

transposeHelper : (x : Vect n a) ->
                  (xsTrans : Vect n (Vect k a)) ->
                  Vect n (Vect (S k) a)
transposeHelper [] [] = []
transposeHelper (x :: xs) (y :: ys) = (x :: y) :: transposeHelper xs ys

transposeMat : {n: _} -> Vect m (Vect n a) -> Vect n (Vect m a)
transposeMat {n} [] = createEmpties {n}
transposeMat {n} (x :: xs) = let xsTrans = transposeMat {n} xs in transposeHelper x xsTrans


--책과 달라진 점 https://idris2.readthedocs.io/en/stable/typedd/typedd.html#chapter-3
{- copilot 답변
m은 패턴 매칭으로 자동으로 드러나는 값이에요.
예를 들어
-> [] 패턴을 쓰면 m = 0임을 Idris가 알 수 있고,
-> (x :: xs) 패턴을 쓰면 m = S k 형태임을 알 수 있습니다.
n은 내부 벡터의 길이로, 패턴 매칭으로 드러나지 않고 타입 인덱스로만 존재합니다. 
그래서 {n}을 명시해야 replicate n [] 같은 값 수준 연산에 쓸 수 있습니다.
-}

addMatrix : Num a => {n: _} -> {m: _} -> Vect m (Vect n a) -> Vect m (Vect n a) -> Vect m (Vect n a)
addMatrix xs [] = xs
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

--AI 도움을 받아도 행렬 자체를 이해 못함. 
dot : Num a => Vect n a -> Vect n a -> a
dot [] [] = 0
dot (x :: xs) (y :: ys) = x * y + dot xs ys

multMatrix : Num a => {n: _} -> {m: _} -> {z: _} -> Vect m (Vect n a) -> Vect n (Vect z a) -> Vect m (Vect z a)
multMatrix [] _ = []
multMatrix (row :: rows) cols = newRow :: multMatrix rows cols
  where 
    newCols = transposeMat cols
    newRow = map (dot row) newCols
