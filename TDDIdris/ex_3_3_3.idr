import Data.Vect

tMat_rhs_1 : Vect n (Vect 0 elem)
tMat_rhs_1 = replicate _ []

transposeMat : Vect m (Vect n elem) -> Vect n (Vect m elem)
transposeMat [] = tMat_rhs_1
transposeMat (x :: xs) = let xsTrans = transposeMat xs in
							zipWith (::) x xsTrans

addMatrix : Num a => Vect n (Vect m a) -> Vect n (Vect m a) -> Vect n (Vect m a)
addMatrix xs [] = xs
addMatrix (x :: xs) (y :: ys) = zipWith (+) x y :: addMatrix xs ys

multMatrix_rhs_1 : Num a => (ys : Vect m (Vect p a)) -> Vect 0 (Vect p a)
multMatrix_rhs_1 [] = []
multMatrix_rhs_1 (x :: xs) = []

multMatrix : Num a => Vect n (Vect m a) -> Vect m (Vect p a) -> Vect n (Vect p a)
multMatrix [] ys = multMatrix_rhs_1 ys
multMatrix (x :: xs) ys = matVectMult x ys :: multMatrix xs ys
	where
		matMult : Num a => Vect p a -> Vect p a -> Vect p a
		matMult xs ys = zipWith (*) xs ys

		matVectMult : Num a =>  Vect m a -> Vect m (Vect p a) -> Vect p a
		matVectMult xs ys = map sum (map(matMult xs) (transposeMat ys))
