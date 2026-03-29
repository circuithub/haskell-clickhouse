{-# LANGUAGE FunctionalDependencies #-}

module Database.ClickHouse.Stream
  ( Stream (..),
    empty,
    cons,
    singleton,
    fromList,
    fromFoldable,
    foldStream,
    ToStreamIO (..),
  )
where

import Data.List.NonEmpty (NonEmpty, toList)

-- | A CPS (Scott-encoded) stream type. Each step either yields an element
-- and a tail, or signals the end of the stream.
newtype Stream m a = Stream
  { unStream ::
      forall r.
      (a -> Stream m a -> m r) ->
      m r ->
      m r
  }

empty :: Stream m a
empty = Stream $ \_yield done -> done

cons :: a -> Stream m a -> Stream m a
cons a rest = Stream $ \yield _done -> yield a rest

singleton :: a -> Stream m a
singleton a = cons a empty

fromList :: [a] -> Stream m a
fromList [] = empty
fromList (x : xs) = cons x (fromList xs)

fromFoldable :: (Foldable f) => f a -> Stream m a
fromFoldable = fromList . foldr (:) []

foldStream :: (Monad m) => (b -> a -> m b) -> b -> Stream m a -> m b
foldStream step !acc stream =
  unStream
    stream
    ( \a rest -> do
        !acc' <- step acc a
        foldStream step acc' rest
    )
    (pure acc)

-- | Convert a collection or streaming abstraction into a 'Stream IO'.
--
-- Instances are provided for lists and @'Stream' IO@ itself. Implement this
-- class to support streaming from custom data sources in 'runInsert'.
class ToStreamIO chunk a | a -> chunk where
  toStreamIO :: a -> Stream IO chunk

instance ToStreamIO a [a] where
  toStreamIO = fromList

instance ToStreamIO a (Stream IO a) where
  toStreamIO = id

instance ToStreamIO a (NonEmpty a) where
  toStreamIO = fromList . toList
