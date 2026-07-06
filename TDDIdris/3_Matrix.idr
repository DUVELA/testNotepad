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
{-copilot 답변
multMatrix [] _ = []는 “재귀가 종료된다”는 뜻인데, 
그게 단순히 멈춘다는 의미만 있는 게 아니라 앞에서 이미 계산된 결과들을 반환하면서 종료한다는 거야.
multMatrix [[1,2],[3,4]] B
│
├─ newRow = map (dot [1,2]) (transpose B)
│   ├─ dot [1,2] [5,7] = 19
│   └─ dot [1,2] [6,8] = 22
│   → newRow = [19,22]
│
└─ multMatrix [[3,4]] B
    │
    ├─ newRow = map (dot [3,4]) (transpose B)
    │   ├─ dot [3,4] [5,7] = 43
    │   └─ dot [3,4] [6,8] = 50
    │   → newRow = [43,50]
    │
    └─ multMatrix [] B
        → []
[ [19,22] :: [ [43,50] :: [] ] ]
= [ [19,22], [43,50] ]

행렬 곱셈은 "앞 행렬의 행 × 뒤 행렬의 열"이 기본 규칙.
- 하지만 코드에서 행렬은 "행들의 리스트"로만 표현돼 있어서 열을 직접 꺼내기 어렵다.
- 그래서 transposeMat을 써서 뒤 행렬의 열을 행처럼 바꿔주고, 내적 계산을 쉽게 한다.
- 즉, newCols = transposeMat cols는 행렬 곱셈을 가능하게 만드는 핵심 준비 단계야.
-}
{- copilot 답변
cols는 안 줄어든다 → 두 번째 행렬 전체를 계속 참조해야 하니까.
dot은 한 행과 한 열을 내적하는 함수.
map이 그 행과 모든 열을 돌면서 dot을 적용하기 때문에, 결과가 행 단위로 만들어진다.
-}
