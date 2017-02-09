{-# Language OverloadedStrings #-}

module Handler.SearchSuggest where

import Import
import DB

getSearchSuggestR :: Handler Value
getSearchSuggestR = do
        query <- fromMaybe "" <$> lookupGetParam "term"
        users <- getUsersWithName query
        let sortedUsers = incListToList $ insertSort users
        returnJson ((Import.map (\(Entity _userId user) -> userEmailAddress user) sortedUsers) :: [Text])

{-@ data IncList a =
        Emp
      | (:<) { hd :: a, tl :: IncList {v:a | hd <= v}}  @-}
{-@ autosize IncList @-}
data IncList a =
    Emp
  | (:<) { hd :: a, tl :: IncList a }
infixr 9 :<

incListToList :: IncList a -> [a]
incListToList Emp = []
incListToList (x :< y) = x:(incListToList y)

insertSort        :: (Ord a) => [a] -> IncList a
insertSort []     = Emp
insertSort (x:xs) = insertHelper x (insertSort xs)

insertHelper :: (Ord a) => a -> IncList a -> IncList a
insertHelper y Emp       = y :< Emp
insertHelper y (x :< xs)
  | y <= x         = y :< x :< xs
  | otherwise      = x :< insertHelper y xs
