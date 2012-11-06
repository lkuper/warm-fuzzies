{-# LANGUAGE Rank2Types #-}

-- | To make sense of what's going on with rank-2 types, first, let's
-- just look at a rank-1 "apply" function.

apply :: (a -> a) -> a -> a
apply f v = f v

-- | Incidentally, because we have Rank2Types turned on, We
-- could've also written it with an explicit 'forall':

applyExplicitForall :: forall a . (a -> a) -> a -> a
applyExplicitForall f v = f v

-- | OK, great.  You can do, e.g., 'apply double 5' and get 10.

double :: Int -> Int
double x = 2 * x

-- | It will work if f is itself polymorphic, too.  There's only one
-- polymorphic function from a to a, of course!

identity :: a -> a
identity x = x

-- | How about a slightly more interesting polymorphic function?  Say,
-- one from a to [a]?  Like these, for instance:

listify :: a -> [a]
listify x = [x]

listifyFour :: a -> [a]
listifyFour x = [x, x, x, x]

-- | So, then, our "apply" function would look like this:

applyListifySimple :: (a -> [a]) -> a -> [a]
applyListifySimple f v = f v

-- | That works fine.  But what if we want to use our polymorphic
-- "listify" function in two ways in the same function?  Say, we want
-- to listify the second argument, v, but we also want to listify a
-- third argument, n, which is an Int, and then return a tuple of
-- both.

-- applyListifyTwoWaysWrong :: (a -> [a]) -> a -> Int -> ([a], [Int])
-- applyListifyTwoWaysWrong f v n = (f v, f n)

-- | Something like that should be fine, because f can take any type.
-- Except it doesn't work:

-- rank2.hs:47:40:
--     Couldn't match type `a' with `Int'
--       `a' is a rigid type variable bound by
--           the type signature for
--             applyListifyTwoWaysWrong :: (a -> [a]) -> a -> Int -> ([a], [Int])
--           at rank2.hs:46:29
--     Expected type: [Int]
--       Actual type: [a]
--     In the return type of a call of `f'
--     In the expression: f n
--     In the expression: (f v, f n)

-- | The problem is that the scope of the implicit "forall a" at the
-- beginning of the type extends over the whole type.  We can't leave
-- 'a' abstract in the call 'f v' but specialize it in the call 'f n'.
-- Either it's abstract everywhere in its scope, or it's concrete
-- everywhere in its scope.
-- 
-- The solution is to limit the scope of a, like this:

applyListifyTwoWays :: (forall a . a -> [a]) -> a -> Int -> ([a], [Int])
applyListifyTwoWays f v n = (f v, f n)

-- | And once we've done that, it might make sense to give our
-- different type variables different names, so it's more clear that
-- they have different scopes.  We can also generalize Int to a type
-- variable, if we want:

applyListify :: (forall a . a -> [a]) -> b -> c -> ([b], [c])
applyListify f v n = (f v, f n)

-- | Now, things like this will all work:

-- *Main> applyListify listify "hi" "hello"
-- (["hi"],["hello"])
-- *Main> applyListify listify "hi" 3
-- (["hi"],[3])
-- *Main> applyListify listify 3 "hi"
-- ([3],["hi"])

-- | The type of applyListify is a rank-2 type because the (forall a
-- . a -> [a]) part of it appears on the left side of an arrow inside
-- another quantifier, and therefore can't be moved to the outside, as
-- the other "forall"s can.  We can spell out the type with all its
-- "forall"s to make this more obvious:

applyListifyExplicitForall :: forall b c . (forall a . a -> [a]) -> b -> c -> ([b], [c])
applyListifyExplicitForall f v n = (f v, f n)
