module DBLib where

{-
 Goal: Support Basic CRUD operations
    insert
    select
    update
    delete
 -}

 {- Yesod's peristent library has two fancy types for handling updates and filters: -}
 {- Update field value operation -}
 {- Filter field value Operator -}
 {- I've actually simplified the Filter type here slightly-- value can either be a or a list,
  - depending on the operator you use
  -}

 {-
  - reasonable operations to start with are = < >. It should not be harder
  - to add more in after we figure out how to support these.

  Example queries and the refinements they should produce:
  runDB $ select [UserId ==. 3] :: Handler {u:User | userId u == 3}
  runDB $ select [UserId >. 3, UserReviewer ==. true] :: Handler {u:User | userId user > 3 && userReview user}

  Update doesn't return anything, but we should verify that any assignments happening here do indeed
  satisfy the refinements on the actual Haskell representation of the DB object:
  update 2 [UserEmail .= email@domain.com] <- must check that the email fits the refinement on the userEmail field
-}
import qualified Import as I

select :: (I.PersistEntityBackend record ~ I.BaseBackend backend,
           I.PersistEntity record, I.PersistQueryRead backend, I.MonadIO m) =>
          [I.Filter record] -> I.ReaderT backend m [I.Entity record]
select filters = I.selectList filters []

update :: (I.PersistEntityBackend record ~ I.BaseBackend backend,
           I.PersistEntity record, I.MonadIO m,
           I.PersistStoreWrite backend) =>
          I.Key record -> [I.Update record] -> I.ReaderT backend m ()
update = I.update

insert :: (I.PersistEntityBackend record ~ I.BaseBackend backend,
           I.PersistEntity record, I.MonadIO m,
           I.PersistStoreWrite backend) =>
          record -> I.ReaderT backend m (I.Key record)
insert = I.insert

delete :: (I.PersistEntityBackend record ~ I.BaseBackend backend,
	    I.PersistEntity record, I.MonadIO m,
	    I.PersistStoreWrite backend) =>
	I.Key record -> I.ReaderT backend m ()
delete = I.delete
