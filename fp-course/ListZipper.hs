{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.ListZipper where

import Course.Core
import Course.List
import Course.Optional
import Course.Functor
import Course.Applicative
import Course.Extend
import Course.Comonad
import Course.Traversable
import qualified Prelude as P

-- $setup
-- >>> import Test.QuickCheck
-- >>> import Data.Maybe(maybe)
-- >>> import Course.Core
-- >>> import qualified Prelude as P
-- >>> let optional e _ Empty = e; optional _ f (Full a) = f a
-- >>> instance Arbitrary a => Arbitrary (Optional a) where arbitrary = P.fmap (maybe Empty Full) arbitrary
-- >>> instance Arbitrary a => Arbitrary (List a) where arbitrary = P.fmap (P.foldr (:.) Nil :: ([a] -> List a)) arbitrary
-- >>> instance Arbitrary a => Arbitrary (ListZipper a) where arbitrary = do l <- arbitrary; x <- arbitrary; r <- arbitrary; P.return (ListZipper l x r)

-- A `ListZipper` is a focussed position, with a list of values to the left and to the right.
--
-- For example, taking the list [0,1,2,3,4,5,6], the moving focus to the third position, the zipper looks like:
-- ListZipper [2,1,0] 3 [4,5,6]
--
-- Supposing then we move left on this zipper:
-- ListZipper [1,0] 2 [3,4,5,6]
--
-- then suppose we add 17 to the focus of this zipper:
-- ListZipper [1,0] 19 [3,4,5,6]
data ListZipper a =
  ListZipper (List a) a (List a)
  deriving Eq

lefts ::
  ListZipper a
  -> List a
lefts (ListZipper l _ _) =
  l

rights ::
  ListZipper a
  -> List a
rights (ListZipper _ _ r) =
  r

-- A `MaybeListZipper` is a data structure that allows us to "fail" zipper operations.
-- e.g. Moving left when there are no values to the left.
--
-- We then overload operations polymorphically to operate on both `ListZipper` and `MaybeListZipper`
-- using the `ListZipper'` type-class below.
data MaybeListZipper a =
  IsZ (ListZipper a)
  | IsNotZ
  deriving Eq

-- | Implement the `Functor` instance for `ListZipper`.
--
-- >>> (+1) <$> (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2] >5< [6,7,8]
instance Functor ListZipper where
  (<$>) f (ListZipper l a r) = ListZipper (f <$> l) (f a) (f <$> r)

-- | Implement the `Functor` instance for `MaybeListZipper`.
--
-- >>> (+1) <$> (IsZ (zipper [3,2,1] 4 [5,6,7]))
-- [4,3,2] >5< [6,7,8]
instance Functor MaybeListZipper where
  (<$>) _ IsNotZ = IsNotZ
  (<$>) f (IsZ lz) = IsZ $ f <$> lz

-- | Convert the given zipper back to a list
--
-- >>> toList <$> toOptional (fromList Nil)
-- Empty
--
-- >>> toList (ListZipper Nil 1 (2:.3:.4:.Nil))
-- [1,2,3,4]
--
-- >>> toList (ListZipper (3:.2:.1:.Nil) 4 (5:.6:.7:.Nil))
-- [1,2,3,4,5,6,7]
toList :: ListZipper a -> List a
toList (ListZipper l a r) = reverse l ++ (a :. Nil) ++ r

-- | Convert the given (maybe) zipper back to a list.
toListZ :: MaybeListZipper a -> List a
toListZ IsNotZ = Nil
toListZ (IsZ z) = toList z

-- | Create a `MaybeListZipper` positioning the focus at the head.
--
-- ->>> fromList (1 :. 2 :. 3 :. Nil)
-- [] >1< [2,3]
--
-- >>> fromList Nil
-- ><
--
-- prop> \xs -> xs == toListZ (fromList xs)
fromList :: List a -> MaybeListZipper a
fromList Nil = IsNotZ
fromList (x :. xs) = IsZ $ ListZipper Nil x xs

-- | Retrieve the `ListZipper` from the `MaybeListZipper` if there is one.
--
-- prop> \xs -> isEmpty xs == (toOptional (fromList xs) == Empty)
--
-- prop> \z -> toOptional (fromOptional z) == z
toOptional :: MaybeListZipper a -> Optional (ListZipper a)
toOptional IsNotZ = Empty
toOptional (IsZ z) = Full z

zipper :: [a] -> a -> [a] -> ListZipper a
zipper l x r = ListZipper (listh l) x (listh r)

fromOptional :: Optional (ListZipper a) -> MaybeListZipper a
fromOptional Empty = IsNotZ
fromOptional (Full z) = IsZ z

asZipper :: (ListZipper a -> ListZipper a) -> MaybeListZipper a -> MaybeListZipper a
asZipper f = asMaybeZipper (IsZ . f)

(>$>):: (ListZipper a -> ListZipper a) -> MaybeListZipper a -> MaybeListZipper a
(>$>) = asZipper

asMaybeZipper :: (ListZipper a -> MaybeListZipper a) -> MaybeListZipper a -> MaybeListZipper a
asMaybeZipper _ IsNotZ = IsNotZ
asMaybeZipper f (IsZ z) = f z

(-<<) :: (ListZipper a -> MaybeListZipper a) -> MaybeListZipper a -> MaybeListZipper a
(-<<) = asMaybeZipper

-- | Update the focus of the zipper with the given function on the current focus.
--
-- >>> withFocus (+1) (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> withFocus (+1) (zipper [1,0] 2 [3,4])
-- [1,0] >3< [3,4]
withFocus :: (a -> a) -> ListZipper a -> ListZipper a
withFocus f (ListZipper l a r) = ListZipper l (f a) r

-- | Set the focus of the zipper to the given value.
-- /Tip:/ Use `withFocus`.
--
-- >>> setFocus 1 (zipper [] 0 [1])
-- [] >1< [1]
--
-- >>> setFocus 1 (zipper [1,0] 2 [3,4])
-- [1,0] >1< [3,4]
setFocus :: a -> ListZipper a -> ListZipper a
setFocus a z = withFocus (const a) z

-- A flipped infix alias for `setFocus`. This allows:
--
-- z .= "abc" -- sets the focus on the zipper z to the value "abc".
(.=) :: ListZipper a -> a -> ListZipper a
(.=) = flip setFocus

-- | Returns whether there are values to the left of focus.
--
-- >>> hasLeft (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasLeft (zipper [] 0 [1,2])
-- False
hasLeft :: ListZipper a -> Bool
hasLeft (ListZipper Nil _ _) = False 
hasLeft _ = True

-- | Returns whether there are values to the right of focus.
--
-- >>> hasRight (zipper [1,0] 2 [3,4])
-- True
--
-- >>> hasRight (zipper [1,0] 2 [])
-- False
hasRight :: ListZipper a -> Bool
hasRight (ListZipper _ _ Nil) = False
hasRight _ = True

-- | Seek to the left for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> \xs p -> findLeft (const p) -<< fromList xs == IsNotZ
--
-- >>> findLeft (== 1) (zipper [2, 1] 3 [4, 5])
-- [] >1< [2,3,4,5]
--
-- >>> findLeft (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findLeft (== 1) (zipper [2, 1] 1 [4, 5])
-- [] >1< [2,1,4,5]
--
-- >>> findLeft (== 1) (zipper [1, 2, 1] 3 [4, 5])
-- [2,1] >1< [3,4,5]
--
-- >>> findLeft (== 1) (zipper [3, 4, 1, 5] 9 [2, 7])
-- [5] >1< [4,3,9,2,7]
-- break l == ([3,4], [1,5])

findLeft :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findLeft p (ListZipper l a r) =
  case (break p l) of
    (_,  Nil) -> IsNotZ
    (xs, ys) -> let (y' :. ys') = ys
                in IsZ $ ListZipper ys' y' (reverse xs ++ a :. r)
    
-- | Seek to the right for a location matching a predicate, starting from the
-- current one.
--
-- /Tip:/ Use `break`
--
-- prop> \xs -> findRight (const False) -<< fromList xs == IsNotZ
--
-- >>> findRight (== 5) (zipper [2, 1] 3 [4, 5])
-- [4,3,2,1] >5< []
--
-- >>> findRight (== 6) (zipper [2, 1] 3 [4, 5])
-- ><
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [4, 5, 1])
-- [5,4,1,2,3] >1< []
--
-- >>> findRight (== 1) (zipper [2, 3] 1 [1, 4, 5, 1])
-- [1,2,3] >1< [4,5,1]
-- breal r == ([1], [4,5,1])
findRight :: (a -> Bool) -> ListZipper a -> MaybeListZipper a
findRight p (ListZipper l a r) =
  case (break p r) of
    (_,  Nil) -> IsNotZ
    (xs, ys) -> let (y' :. ys') = ys
                in IsZ $ ListZipper (reverse xs ++ a :. l) y' ys'

-- | Move the zipper left, or if there are no elements to the left, go to the far right.
--
-- >>> moveLeftLoop (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftLoop (zipper [] 1 [2,3,4])
-- [3,2,1] >4< []
moveLeftLoop :: ListZipper a -> ListZipper a
moveLeftLoop (ListZipper Nil a r) = let (x :. xs) = reverse $ a :. r
                                    in ListZipper xs x Nil
moveLeftLoop (ListZipper (x :. xs) a r) = ListZipper xs x (a :. r)

-- | Move the zipper right, or if there are no elements to the right, go to the far left.
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRightLoop (zipper [3,2,1] 4 [])
-- [] >1< [2,3,4]
moveRightLoop ::
  ListZipper a
  -> ListZipper a
moveRightLoop (ListZipper l a Nil) = let (x :. xs) = reverse $ a :. l
                                     in ListZipper Nil x xs
moveRightLoop (ListZipper l a (x :. xs)) = ListZipper (a :. l) x xs

-- | Move the zipper one position to the left.
--
-- >>> moveLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [4,5,6,7]
--
-- >>> moveLeft (zipper [] 1 [2,3,4])
-- ><
moveLeft :: ListZipper a -> MaybeListZipper a
moveLeft z | not $ hasLeft z = IsNotZ
           | otherwise = IsZ $ moveLeftLoop z

-- | Move the zipper one position to the right.
--
-- >>> moveRight (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >5< [6,7]
--
-- >>> moveRight (zipper [3,2,1] 4 [])
-- ><
moveRight :: ListZipper a -> MaybeListZipper a
moveRight z | not $ hasRight z = IsNotZ
            | otherwise = IsZ $ moveRightLoop z

-- | Swap the current focus with the value to the left of focus.
--
-- >>> swapLeft (zipper [3,2,1] 4 [5,6,7])
-- [4,2,1] >3< [5,6,7]
--
-- >>> swapLeft (zipper [] 1 [2,3,4])
-- ><
swapLeft :: ListZipper a -> MaybeListZipper a
swapLeft (ListZipper Nil _ _) = IsNotZ
swapLeft (ListZipper (x :. xs) a r) = IsZ $ ListZipper (a :. xs) x r

-- | Swap the current focus with the value to the right of focus.
--
-- >>> swapRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [4,6,7]
--
-- >>> swapRight (zipper [3,2,1] 4 [])
-- ><
swapRight :: ListZipper a -> MaybeListZipper a
swapRight (ListZipper _ _ Nil) = IsNotZ
swapRight (ListZipper l a (x :. xs)) = IsZ $ ListZipper l x (a :. xs)

-- | Drop all values to the left of the focus.
--
-- >>> dropLefts (zipper [3,2,1] 4 [5,6,7])
-- [] >4< [5,6,7]
--
-- >>> dropLefts (zipper [] 1 [2,3,4])
-- [] >1< [2,3,4]
--
-- prop> \l x r -> dropLefts (zipper l x r) == zipper [] x r
dropLefts :: ListZipper a -> ListZipper a
dropLefts (ListZipper _ a r) = ListZipper Nil a r

-- | Drop all values to the right of the focus.
--
-- >>> dropRights (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >4< []
--
-- >>> dropRights (zipper [3,2,1] 4 [])
-- [3,2,1] >4< []
--
-- prop> \l x r -> dropRights (zipper l x r) == zipper l x []
dropRights :: ListZipper a -> ListZipper a
dropRights (ListZipper l a _) = ListZipper l a Nil

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
--
-- >>> moveLeftN 2 (zipper [2,1,0] 3 [4,5,6])
-- [0] >1< [2,3,4,5,6]
--
-- >>> moveLeftN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [3,2,1,0] >4< [5,6]
moveLeftN :: Int -> ListZipper a -> MaybeListZipper a
moveLeftN n z@(ListZipper l _ _)
  | n == 0 = IsZ z
  | n < 0 = moveRightN (n * (-1)) z
  | n < length l = asMaybeZipper (moveLeftN (n-1)) (moveLeft z)
  | otherwise = IsNotZ
  

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
--
-- >>> moveRightN 1 (zipper [2,1,0] 3 [4,5,6])
-- [3,2,1,0] >4< [5,6]
--
-- >>> moveRightN (-1) $ zipper [2,1,0] 3 [4,5,6]
-- [1,0] >2< [3,4,5,6]
moveRightN :: Int -> ListZipper a -> MaybeListZipper a
moveRightN n z@(ListZipper _ _ r)
  | n == 0 = IsZ z
  | n < 0 = moveLeftN (n * (-1)) z
  | n < length r = asMaybeZipper (moveRightN (n-1)) (moveRight z)
  | otherwise = IsNotZ

-- | Move the focus left the given number of positions. If the value is negative, move right instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [2,1] >3< [4,5,6,7]
--
-- >>> moveLeftN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveLeftN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [5,4,3,2,1] >6< [7]
--
-- >>> moveLeftN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveLeftN' 4 (zipper [3,2,1] 4 [5,6,7,8,9])
-- Left 3
--
-- >>> moveLeftN' (-4) (zipper [5,4,3,2,1] 6 [7,8,9])
-- Left 3
moveLeftN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveLeftN' n z@(ListZipper l _ _)
  | n == 0 = Right z
  | n < 0 = moveRightN' (-1 * n) z
  | otherwise = case moveLeftN n z of
                  IsNotZ -> Left $ length l
                  IsZ z' -> Right z'

-- | Move the focus right the given number of positions. If the value is negative, move left instead.
-- If the focus cannot be moved, the given number of times, return the value by which it can be moved instead.
--
-- >>> moveRightN' 4 (zipper [3,2,1] 4 [5,6,7])
-- Left 3
--
-- >>> moveRightN' 1 (zipper [3,2,1] 4 [5,6,7])
-- Right [4,3,2,1] >5< [6,7]
--
-- >>> moveRightN' 0 (zipper [3,2,1] 4 [5,6,7])
-- Right [3,2,1] >4< [5,6,7]
--
-- >>> moveRightN' (-2) (zipper [3,2,1] 4 [5,6,7])
-- Right [1] >2< [3,4,5,6,7]
--
-- >>> moveRightN' (-4) (zipper [3,2,1] 4 [5,6,7])
-- Left 3
moveRightN' :: Int -> ListZipper a -> Either Int (ListZipper a)
moveRightN' n z@(ListZipper _ _ r)
  | n == 0 = Right z
  | n < 0 = moveLeftN' (-1 * n) z
  | otherwise = case moveRightN n z of
                  IsNotZ -> Left $ length r
                  IsZ z' -> Right z'

-- | Move the focus to the given absolute position in the zipper. Traverse the zipper only to the extent required.
--
-- >>> nth 1 (zipper [3,2,1] 4 [5,6,7])
-- [1] >2< [3,4,5,6,7]
--
-- >>> nth 5 (zipper [3,2,1] 4 [5,6,7])
-- [5,4,3,2,1] >6< [7]
--
-- >>> nth 8 (zipper [3,2,1] 4 [5,6,7])
-- ><
nth :: Int -> ListZipper a -> MaybeListZipper a
nth n z@(ListZipper l _ _)
  | n == len = IsZ z
  | n < len = moveLeftN (len - n) z
  | otherwise = moveRightN (n - len) z
  where len = length l

  
-- | Return the absolute position of the current focus in the zipper.
--
-- >>> index (zipper [3,2,1] 4 [5,6,7])
-- 3
--
-- prop> \i z z' -> optional True (\z' -> index z' == i) (toOptional (nth i z))
index :: ListZipper a -> Int
index (ListZipper l _ _) = length l
  

-- | Move the focus to the end of the zipper.
--
-- >>> end (zipper [3,2,1] 4 [5,6,7])
-- [6,5,4,3,2,1] >7< []
--
-- prop> \lz -> toList lz == toList (end lz)
--
-- prop> \lz -> rights (end lz) == Nil
end :: ListZipper a -> ListZipper a
end (ListZipper l a r) =
  let (x :. xs) = reverse r ++ a :. l
  in ListZipper xs x Nil
  

-- | Move the focus to the start of the zipper.
--
-- >>> start (zipper [3,2,1] 4 [5,6,7])
-- [] >1< [2,3,4,5,6,7]
--
-- prop> \lz -> toList lz == toList (start lz)
--
-- prop> \lz -> lefts (start lz) == Nil
start :: ListZipper a -> ListZipper a
start (ListZipper l a r) =
  let (x :. xs) = reverse l ++ a :. r
  in ListZipper Nil x xs


-- | Delete the current focus and pull the left values to take the empty position.
--
-- >>> deletePullLeft (zipper [3,2,1] 4 [5,6,7])
-- [2,1] >3< [5,6,7]
--
-- >>> deletePullLeft (zipper [] 1 [2,3,4])
-- ><
deletePullLeft :: ListZipper a -> MaybeListZipper a
deletePullLeft (ListZipper Nil _ _) = IsNotZ
deletePullLeft (ListZipper (x :. xs) _ r) = IsZ $ ListZipper xs x r

-- | Delete the current focus and pull the right values to take the empty position.
--
-- >>> deletePullRight (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >5< [6,7]
--
-- >>> deletePullRight (zipper [3,2,1] 4 [])
-- ><
deletePullRight :: ListZipper a -> MaybeListZipper a
deletePullRight (ListZipper _ _ Nil) = IsNotZ
deletePullRight (ListZipper l _ (x :. xs)) = IsZ $ ListZipper l x xs


-- | Insert at the current focus and push the left values to make way for the new position.
--
-- >>> insertPushLeft 15 (zipper [3,2,1] 4 [5,6,7])
-- [4,3,2,1] >15< [5,6,7]
--
-- >>> insertPushLeft 15 (zipper [] 1 [2,3,4])
-- [1] >15< [2,3,4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullLeft (insertPushLeft i z)))
insertPushLeft :: a -> ListZipper a -> ListZipper a
insertPushLeft a' (ListZipper l a r) = ListZipper (a :. l) a' r

-- | Insert at the current focus and push the right values to make way for the new position.
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [5,6,7])
-- [3,2,1] >15< [4,5,6,7]
--
-- >>> insertPushRight 15 (zipper [3,2,1] 4 [])
-- [3,2,1] >15< [4]
--
-- prop> \i z -> optional False (==z) (toOptional (deletePullRight (insertPushRight i z)))
insertPushRight :: a -> ListZipper a -> ListZipper a
insertPushRight a' (ListZipper l a r) = ListZipper l a' (a :. r)

-- | Implement the `Applicative` instance for `ListZipper`.
-- `pure` produces an infinite list zipper (to both left and right).
-- (<*>) zips functions with values by function application.
--
-- prop> \n -> all . (==) <*> take n . lefts . pure
--
-- prop> \n -> all . (==) <*> take n . rights . pure
--
-- >>> zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)] <*> zipper [3,2,1] 4 [5,6,7]
-- [5,12] >8< [15,24,12]
instance Applicative ListZipper where
-- /Tip:/ Use @List#repeat@.
  pure a = ListZipper (repeat a) a (repeat a)
-- /Tip:/ Use `zipWith`
  (<*>) (ListZipper lf af rf) (ListZipper l a r) =
    ListZipper (zipWith ($) lf l) (af a) (zipWith ($) rf r)

-- | Implement the `Applicative` instance for `MaybeListZipper`.
--
-- /Tip:/ Use @pure@ for `ListZipper`.
-- /Tip:/ Use `<*>` for `ListZipper`.
--
-- prop> \z n -> let is (IsZ z) = z in all . (==) <*> take n . lefts . is . pure
--
-- prop> \z n -> let is (IsZ z) = z in all . (==) <*> take n . rights . is . pure
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- [5,12] >8< [15,24,12]
--
-- >>> IsNotZ <*> IsZ (zipper [3,2,1] 4 [5,6,7])
-- ><
--
-- >>> IsZ (zipper [(+2), (+10)] (*2) [(*3), (4*), (5+)]) <*> IsNotZ
-- ><
--
-- >>> IsNotZ <*> IsNotZ
-- ><
instance Applicative MaybeListZipper where
  pure a = IsZ $ pure a
  (<*>) _ IsNotZ = IsNotZ
  (<*>) IsNotZ _ = IsNotZ
  (<*>) (IsZ fz) (IsZ z) = IsZ $ fz <*> z

-- | Implement the `Extend` instance for `ListZipper`.
-- This implementation "visits" every possible zipper value derivable from a given zipper (i.e. all zippers to the left and right).
--
-- /Tip:/ Use @List#unfoldr@.
--
-- >>> id <<= (zipper [2,1] 3 [4,5])
-- [[1] >2< [3,4,5], [] >1< [2,3,4,5]]
-- >[2,1] >3< [4,5]<
-- [[3,2,1] >4< [5], [4,3,2,1] >5< []]
instance Extend ListZipper where
  (<<=) f z = ListZipper lz (f z) rz
    where
      lz = unfoldr (ap moveLeft) $ moveLeft z
      rz = unfoldr (ap moveRight) $ moveRight z
      ap mv z' = case z' of
                   IsNotZ -> Empty
                   IsZ z'' -> Full (f z'', mv z'') 

-- | Implement the `Extend` instance for `MaybeListZipper`.
-- This instance will use the `Extend` instance for `ListZipper`.
--
--
-- id <<= IsNotZ
-- ><
--
-- >>> id <<= (IsZ (zipper [2,1] 3 [4,5]))
-- [[1] >2< [3,4,5],[] >1< [2,3,4,5]] >[2,1] >3< [4,5]< [[3,2,1] >4< [5],[4,3,2,1] >5< []]
instance Extend MaybeListZipper where
  (<<=) f mz = case mz of
                 IsNotZ -> IsNotZ
                 (IsZ z) -> IsZ $ (f . IsZ) <<= z

-- | Implement the `Comonad` instance for `ListZipper`.
-- This implementation returns the current focus of the zipper.
--
-- >>> copure (zipper [2,1] 3 [4,5])
-- 3
instance Comonad ListZipper where
  copure (ListZipper _ a _) = a

-- | Implement the `Traversable` instance for `ListZipper`.
-- This implementation traverses a zipper while running some `Applicative` effect through the zipper.
-- An effectful zipper is returned.
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7])
-- Full [1,2,3] >4< [5,6,7]
--
-- >>> traverse id (zipper [Full 1, Full 2, Full 3] (Full 4) [Empty, Full 6, Full 7])
-- Empty
instance Traversable ListZipper where
  traverse f (ListZipper l a r) =
    ListZipper <$> (reverse <$> (traverse f $ reverse l)) <*> f a <*> traverse f r
--  traverse f z = zipIndex <$> traverse f (toList z)
--    where i = index z
 --         zipIndex zl = let (IsZ z') = asMaybeZipper (nth i) $ fromList zl
--                        in z'
            
-- | Implement the `Traversable` instance for `MaybeListZipper`.
--
-- /Tip:/ Use `traverse` for `ListZipper`.
--
-- >>> traverse id IsNotZ
-- ><
--
-- >>> traverse id (IsZ (zipper [Full 1, Full 2, Full 3] (Full 4) [Full 5, Full 6, Full 7]))
-- Full [1,2,3] >4< [5,6,7]
instance Traversable MaybeListZipper where
  traverse _ IsNotZ = pure IsNotZ
  traverse f (IsZ z) = IsZ <$> traverse f z

-----------------------
-- SUPPORT LIBRARIES --
-----------------------

instance Show a => Show (ListZipper a) where
  show (ListZipper l x r) =
    stringconcat [show l, " >", show x, "< ", show r]

instance Show a => Show (MaybeListZipper a) where
  show (IsZ z) = show z
  show IsNotZ = "><"
