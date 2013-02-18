-- | With the traditional Scheme definition of "append", the second
-- argument doesn't have to be a list.  For instance:
-- 
--   (define append
--     (lambda (ls1 ls2)
--       (cond
--         [(null? ls1) ls2]
--         [else (cons (car ls1) (append (cdr ls1) ls2))])))
-- 
--   > (append '() 3)
--   3
-- 
-- In Haskell, though, if we define append in the obvious way, that's
-- not the case.

myappend :: [a] -> [a] -> [a]
myappend [] x = x
myappend (x:xs) y = x : (myappend xs y)

-- | What if, for some reason, we wanted to fake the Scheme behavior?
-- Let's make a disjoint sum type for the "ls2" argument.  It's either
-- going to be a list or an atom.  A first cut:

-- data EitherOne a b = Left a | Right b
-- type Appendee a = EitherOne [a] a

-- scheme_append :: [a] -> (Appendee a) -> (Appendee a)
-- scheme_append [] x = x
-- scheme_append (x:xs) y = x : (scheme_append xs y)

-- | But this doesn't work.



