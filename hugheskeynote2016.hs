--     hugheskeynote2016.hs
module Hugheskeynote2016 where
-- Erlang Factroy SF 2016 - Keynote - John Hughes - Why Functional Programming Matters
-- https://www.youtube.com/watch?v=Z35Tt87pIpg   from 1:24 to 9:40

-- Church encodings: booleans
-- Booleans
true  x y = x
false x y = y

-- if-then-else
ifte bool t e =
  bool t e

-- ghci> ifte true 6 8
-- 6
-- ghci> ifte false 6 8
-- 8

-- Church encodings: integers
two  f x = f (f x)
one  f x =    f x
zero f x =      x

three   f x =           f(f(f x))
four    f x =         f(f(f(f x)))
five    f x =       f(f(f(f(f x))))
six     f x =     f(f(f(f(f(f x)))))
seven   f x =   f(f(f(f(f(f(f x))))))
eight   f x = f(f(f(f(f(f(f(f x)))))))

add m n f x = m f (n f x)
mul m n f x = m (n f) x

-- ghci> add one (mul two two) (+1) 0
-- 5
-- ghci> add (add seven two) (mul two eight) (+1) 0
-- 25
