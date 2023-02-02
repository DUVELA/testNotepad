fib : Nat -> Nat
fib Z = 0
fib (S Z) = 1
fib (S (S k)) = fib(S k) + fib(k)
