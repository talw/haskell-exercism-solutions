module LinkedList (nil, isNil, next, datum, fromList, toList, reverseLinkedList, new)
where

data MyList a = Empty | Cons a (MyList a)


isNil :: MyList a -> Bool
isNil Empty = True
isNil _ = False

nil :: MyList a
nil = Empty

next :: MyList a -> MyList a
next (Cons _ xs) = xs
next _ = error "Empty list"

datum :: MyList a -> a
datum (Cons x _) = x
datum Empty = error "Empty list"

fromList :: [a] -> MyList a
fromList = foldr Cons Empty

toList :: MyList a -> [a]
toList Empty = []
toList (Cons x xs) = x : toList xs

reverseLinkedList :: MyList a -> MyList a
reverseLinkedList list = rev list Empty
    where rev Empty acc = acc
          rev (Cons x xs) ys = rev xs $ Cons x ys

new :: a -> MyList a -> MyList a
new = Cons
