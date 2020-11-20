{-# LANGUAGE FlexibleContexts, BangPatterns, TupleSections, Rank2Types, ScopedTypeVariables #-}

-- |Monadic Iteratees:
-- incremental input parsers, processors and transformers
--
-- This module provides many basic iteratees from which more complicated
-- iteratees can be built.  In general these iteratees parallel those in
-- @Data.List@, with some additions.

module Data.Iteratee.ListLike (
  -- * Iteratees
  -- ** Iteratee Utilities
  isFinished
  ,stream2list
  ,stream2stream
  -- ** Basic Iteratees
  ,dropWhile
  ,drop
  ,head
  ,last
  ,heads
  ,peek
  ,length
  -- ** Nested iteratee combinators
  ,breakE
  ,take
  ,takeUpTo
  ,filter
  ,groupBy
  -- ** Folds
  ,foldl'
  -- * Enumerators
  -- ** Basic enumerators
  ,enumPureNChunk
  -- ** Enumerator Combinators
  ,enumWith
  ,zip
  ,zip3
  ,sequence_
  -- ** Monadic functions
  ,mapM_
  ,foldM
  -- * Re-exported modules
  ,module Data.Iteratee.Iteratee
)
where

import Prelude hiding (mapM_, null, head, last, drop, dropWhile, take, break, foldl, foldl1, length, filter, sum, product, zip, zip3, sequence_)

import qualified Prelude as Prelude

import qualified Data.ListLike as LL
import qualified Data.ListLike.FoldableLL as FLL
import Data.Iteratee.Iteratee
import Data.Maybe (catMaybes)
import Control.Monad (liftM, liftM2, mplus, (<=<))
import Control.Monad.Trans.Class

-- Useful combinators for implementing iteratees and enumerators

-- | Check if a stream has received 'EOF'.
isFinished :: (Nullable s) => Iteratee s m Bool
isFinished = liftI check
  where
  check c@(Chunk xs)
    | nullC xs    = liftI check
    | otherwise   = idone False c
  check s@(EOF _) = idone True s
{-# INLINE isFinished #-}

-- ------------------------------------------------------------------------
-- Primitive iteratees

-- |Read a stream to the end and return all of its elements as a list.
-- This iteratee returns all data from the stream *strictly*.
stream2list :: (Nullable s, LL.ListLike s el) => Iteratee s m [el]
stream2list = liftI (step [])
  where
    step acc (Chunk ls)
      | nullC ls  = liftI (step acc)
      | otherwise = liftI (step (acc ++ LL.toList ls))
    step acc str  = idone acc str
{-# INLINE stream2list #-}

-- |Read a stream to the end and return all of its elements as a stream.
-- This iteratee returns all data from the stream *strictly*.
stream2stream :: (Nullable s, Monoid s) => Iteratee s m s
stream2stream = icont (step mempty) Nothing
  where
    step acc (Chunk ls)
      | nullC ls   = icont (step acc) Nothing
      | otherwise  = icont (step (acc `mappend` ls)) Nothing
    step acc str   = idone acc str
{-# INLINE stream2stream #-}

-- |Attempt to read the next element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated
--
-- The analogue of @List.head@
head :: (LL.ListLike s el) => Iteratee s m el
head = liftI step
  where
  step (Chunk vec)
    | LL.null vec  = icont step Nothing
    | otherwise    = idone (LL.head vec) (Chunk $ LL.tail vec)
  step stream      = icont step (Just (setEOF stream))
{-# INLINE head #-}

-- |Attempt to read the last element of the stream and return it
-- Raise a (recoverable) error if the stream is terminated
--
-- The analogue of @List.last@
last :: (LL.ListLike s el, Nullable s) => Iteratee s m el
last = liftI (step Nothing)
  where
  step l (Chunk xs)
    | nullC xs     = liftI (step l)
    | otherwise    = liftI $ step (Just $ LL.last xs)
  step l s@(EOF _) = case l of
    Nothing -> icont (step l) . Just . setEOF $ s
    Just x  -> idone x s
{-# INLINE last #-}


-- |Given a sequence of characters, attempt to match them against
-- the characters on the stream.  Return the count of how many
-- characters matched.  The matched characters are removed from the
-- stream.
-- For example, if the stream contains 'abd', then (heads 'abc')
-- will remove the characters 'ab' and return 2.
heads :: (Monad m, Nullable s, LL.ListLike s el, Eq el) => s -> Iteratee s m Int
heads st | nullC st = return 0
heads st = loop 0 st
  where
  loop cnt xs
    | nullC xs  = return cnt
    | otherwise = liftI (step cnt xs)
  step cnt str (Chunk xs) | nullC xs  = liftI (step cnt str)
  step cnt str stream     | nullC str = idone cnt stream
  step cnt str s@(Chunk xs) =
    if LL.head str == LL.head xs
       then step (succ cnt) (LL.tail str) (Chunk $ LL.tail xs)
       else idone cnt s
  step cnt _ stream         = idone cnt stream
{-# INLINE heads #-}


-- |Look ahead at the next element of the stream, without removing
-- it from the stream.
-- Return @Just c@ if successful, return @Nothing@ if the stream is
-- terminated by 'EOF'.
peek :: (LL.ListLike s el) => Iteratee s m (Maybe el)
peek = liftI step
  where
    step s@(Chunk vec)
      | LL.null vec = liftI step
      | otherwise   = idone (Just $ LL.head vec) s
    step stream     = idone Nothing stream
{-# INLINE peek #-}

-- |Drop n elements of the stream, if there are that many.
--
-- The analogue of @List.drop@
drop :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Iteratee s m ()
drop 0  = return ()
drop n' = liftI (step n')
  where
    step n (Chunk str)
      | LL.length str < n = liftI (step (n - LL.length str))
      | otherwise         = idone () (Chunk (LL.drop n str))
    step _ stream         = idone () stream
{-# INLINE drop #-}

-- |Skip all elements while the predicate is true.
--
-- The analogue of @List.dropWhile@
dropWhile :: (LL.ListLike s el) => (el -> Bool) -> Iteratee s m ()
dropWhile p = liftI step
  where
    step (Chunk str)
      | LL.null left = liftI step
      | otherwise    = idone () (Chunk left)
      where
        left = LL.dropWhile p str
    step stream      = idone () stream
{-# INLINE dropWhile #-}


-- | Return the total length of the remaining part of the stream.
--
-- This forces evaluation of the entire stream.
--
-- The analogue of @List.length@
length :: (Num a, LL.ListLike s el) => Iteratee s m a
length = liftI (step 0)
  where
    step !i (Chunk xs) = liftI (step $! i + fromIntegral (LL.length xs))
    step !i stream     = idone i stream
{-# INLINE length #-}

-- ---------------------------------------------------
-- The converters show a different way of composing two iteratees:
-- `vertical' rather than `horizontal'

-- |Takes an element predicate and an iteratee, running the iteratee
-- on all elements of the stream until the predicate is met.
--
-- the following rule relates @break@ to @breakE@
-- @break@ pred === @joinI@ (@breakE@ pred stream2stream)
--
-- @breakE@ should be used in preference to @break@ whenever possible.
breakE
  :: (Monad m, LL.ListLike s el, NullPoint s)
  => (el -> Bool)
  -> Enumeratee s s m a
breakE cpred = eneeCheckIfDone (liftI . step)
 where
  step k (Chunk s)
      | LL.null s  = liftI (step k)
      | otherwise  = case LL.break cpred s of
        (str', tail')
          | LL.null tail' -> eneeCheckIfDone (liftI . step) . k $ Chunk str'
          | otherwise     -> idone (k $ Chunk str') (Chunk tail')
  step k stream           =  idone (k stream) stream
{-# INLINE breakE #-}

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. Unless the stream is terminated early, we
-- read exactly n elements, even if the iteratee has accepted fewer.
--
-- The analogue of @List.take@
take ::
  (Monad m, Nullable s, LL.ListLike s el)
  => Int   -- ^ number of elements to consume
  -> Enumeratee s s m a
take n' iter
 | n' <= 0   = return iter
 | otherwise = Iteratee $ \od oc -> runIter iter (on_done od oc) (on_cont od oc)
  where
    on_done od oc x _ = runIter (drop n' >> return (return x)) od oc
    on_cont od oc k Nothing = if n' == 0 then od (liftI k) (Chunk mempty)
                                 else runIter (liftI (step n' k)) od oc
    on_cont od oc _ (Just e) = runIter (drop n' >> throwErr e) od oc
    step n k (Chunk str)
      | LL.null str        = liftI (step n k)
      | LL.length str <= n = take (n - LL.length str) $ k (Chunk str)
      | otherwise          = idone (k (Chunk s1)) (Chunk s2)
      where (s1, s2) = LL.splitAt n str
    step _n k stream       = idone (k stream) stream
{-# INLINABLE take #-}

-- |Read n elements from a stream and apply the given iteratee to the
-- stream of the read elements. If the given iteratee accepted fewer
-- elements, we stop.
-- This is the variation of 'take' with the early termination
-- of processing of the outer stream once the processing of the inner stream
-- finished early.
--
-- Iteratees composed with 'takeUpTo' will consume only enough elements to
-- reach a done state.  Any remaining data will be available in the outer
-- stream.
--
-- > > let iter = do
-- > h <- joinI $ takeUpTo 5 I.head
-- > t <- stream2list
-- > return (h,t)
-- >
-- > > enumPureNChunk [1..10::Int] 3 iter >>= run >>= print
-- > (1,[2,3,4,5,6,7,8,9,10])
-- >
-- > > enumPureNChunk [1..10::Int] 7 iter >>= run >>= print
-- > (1,[2,3,4,5,6,7,8,9,10])
--
-- in each case, @I.head@ consumes only one element, returning the remaining
-- 4 elements to the outer stream
takeUpTo :: (Monad m, Nullable s, LL.ListLike s el) => Int -> Enumeratee s s m a
takeUpTo i iter
 | i <= 0    = return iter
 | otherwise = Iteratee $ \od oc ->
    runIter iter (onDone od oc) (onCont od oc)
  where
    onDone od oc x str      = runIter (idone (return x) str) od oc
    onCont od oc k Nothing  = if i == 0 then od (liftI k) (Chunk mempty)
                                 else runIter (liftI (step i k)) od oc
    onCont od oc _ (Just e) = runIter (throwErr e) od oc
    step n k (Chunk str)
      | LL.null str       = liftI (step n k)
      | LL.length str < n = takeUpTo (n - LL.length str) $ k (Chunk str)
      | otherwise         =
         -- check to see if the inner iteratee has completed, and if so,
         -- grab any remaining stream to put it in the outer iteratee.
         -- the outer iteratee is always complete at this stage, although
         -- the inner may not be.
         let (s1, s2) = LL.splitAt n str
         in Iteratee $ \od' _ -> do
              res <- runIter (k (Chunk s1)) (\a s  -> return $ Left  (a, s))
                                            (\k' e -> return $ Right (k',e))
              case res of
                Left (a,Chunk s1') -> od' (return a)
                                          (Chunk $ s1' `LL.append` s2)
                Left  (a,s')       -> od' (idone a s') (Chunk s2)
                Right (k',e)       -> od' (icont k' e) (Chunk s2)
    step _ k stream       = idone (k stream) stream
{-# INLINABLE takeUpTo #-}

-- |Creates an 'enumeratee' with only elements from the stream that
-- satisfy the predicate function.  The outer stream is completely consumed.
--
-- The analogue of @List.filter@
filter
  :: (Monad m, Nullable s, LL.ListLike s el)
  => (el -> Bool)
  -> Enumeratee s s m a
filter p = convStream f'
  where
    f' = icont step Nothing
    step (Chunk xs)
      | LL.null xs = f'
      | otherwise  = idone (LL.filter p xs) mempty
    step _ = f'
{-# INLINE filter #-}

-- | Creates an 'enumeratee' in which elements are grouped into
-- contiguous blocks that are equal according to a predicate.
--
-- The analogue of 'List.groupBy'
groupBy
  :: forall s el m a. (LL.ListLike s el, Monad m, Nullable s)
  => (el -> el -> Bool)
  -> Enumeratee s [s] m a
groupBy same iinit = liftI $ go iinit LL.empty
    where go icurr pfx (Chunk s) = case gsplit (pfx `LL.append` s) of
                                          (full, partial)
                                              | LL.null full -> liftI $ go icurr partial
                                              | otherwise -> do inext <- lift . enumPure1Chunk full $ icurr
                                                                liftI $ go inext partial
          go icurr pfx (EOF mex)
            | LL.null pfx = lift . enumChunk (EOF mex) $ icurr
            | otherwise = do inext <- lift . enumPure1Chunk (LL.singleton pfx) $ icurr
                             lift . enumChunk (EOF mex) $ inext
          gsplit :: s -> ([s], s)
          gsplit ll | LL.null ll = (LL.empty, LL.empty)
                    | otherwise = let groups = llGroupBy same ll
                                      full = LL.init groups
                                      partial = LL.last groups
                                  in full `seq` partial `seq` (full, partial)
          llGroupBy eq l -- Copied from Data.ListLike, avoid spurious (Eq el) constraint
              | LL.null l = LL.empty
              | otherwise = LL.cons (LL.cons x ys) (llGroupBy eq zs)
              where (ys, zs) = LL.span (eq x) xs
                    x = LL.head l
                    xs = LL.tail l
{-# INLINE groupBy #-}

-- | Left-associative fold that is strict in the accumulator.
-- This function should be used in preference to 'foldl' whenever possible.
--
-- The analogue of @List.foldl'@.
foldl'
  :: (LL.ListLike s el)
  => (a -> el -> a)
  -> a
  -> Iteratee s m a
foldl' f i = liftI (step i)
  where
    step acc (Chunk xs)
      | LL.null xs = liftI (step acc)
      | otherwise  = liftI (step $! FLL.foldl' f acc xs)
    step acc stream = idone acc stream
{-# INLINE foldl' #-}

-- ------------------------------------------------------------------------
-- Zips

-- |Enumerate two iteratees over a single stream simultaneously.
--
-- Compare to @List.zip@.
zip
  :: (Monad m, Nullable s, LL.ListLike s el)
  => Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a, b)
zip x y = liftI step
  where
    step (Chunk xs) | nullC xs = liftI step
    step (Chunk xs) = do
      (a', x') <- lift $ (\i -> runIter i od oc) =<< enumPure1Chunk xs x
      (b', y') <- lift $ (\i -> runIter i od oc) =<< enumPure1Chunk xs y
      case checkDone a' b' of
        Just (a, b, s) -> idone (a, b) s
        Nothing        -> zip x' y'
    step (EOF err) = joinIM $ case err of
      Nothing -> (liftM2.liftM2) (,) (enumEof   x) (enumEof   y)
      Just e  -> (liftM2.liftM2) (,) (enumErr e x) (enumErr e y)

    od a s = return (Just (a, s), idone a s)
    oc k e = return (Nothing    , icont k e)

    checkDone r1 r2 =
      r1 >>= \(a, s1) -> r2 >>= \(b, s2) ->
      return (a, b, shorter s1 s2)

    shorter c1@(Chunk xs) c2@(Chunk ys)
      | LL.length xs < LL.length ys = c1
      | otherwise                   = c2
    shorter e@(EOF _)  _         = e
    shorter _          e@(EOF _) = e
{-# INLINE zip #-}

zip3
  :: (Monad m, Nullable s, LL.ListLike s el)
  => Iteratee s m a -> Iteratee s m b
  -> Iteratee s m c -> Iteratee s m (a, b, c)
zip3 a b c = zip a (zip b c) >>=
  \(r1, (r2, r3)) -> return (r1, r2, r3)
{-# INLINE zip3 #-}

-- | Enumerate over two iteratees in parallel as long as the first iteratee
-- is still consuming input.  The second iteratee will be terminated with EOF
-- when the first iteratee has completed.  An example use is to determine
-- how many elements an iteratee has consumed:
--
-- > snd <$> enumWith (dropWhile (<5)) length
--
-- Compare to @zip@
enumWith
  :: (Monad m, Nullable s, LL.ListLike s el)
  => Iteratee s m a
  -> Iteratee s m b
  -> Iteratee s m (a, b)
enumWith i1 i2 = go i1 i2
  where
    od a s = return (Just (a, s), idone a s)
    oc k e = return (Nothing    , icont k e)

    getUsed xs (Chunk ys) = LL.take (LL.length xs - LL.length ys) xs
    getUsed xs (EOF _)    = xs

    go x y = liftI step
      where
        step (Chunk xs) | nullC xs = liftI step
        step (Chunk xs) = do
          (a', x') <- lift $ (\i -> runIter i od oc) =<< enumPure1Chunk xs x
          case a' of
            Just (a, s) -> do
              b <- lift $ run =<< enumPure1Chunk (getUsed xs s) y
              idone (a, b) s
            Nothing        -> lift (enumPure1Chunk xs y) >>= go x'
        step (EOF err) = joinIM $ case err of
          Nothing -> (liftM2.liftM2) (,) (enumEof   x) (enumEof   y)
          Just e  -> (liftM2.liftM2) (,) (enumErr e x) (enumErr e y)
{-# INLINE enumWith #-}

-- |Enumerate a list of iteratees over a single stream simultaneously
-- and discard the results. This is a different behavior than Prelude's
-- sequence_ which runs iteratees in the list one after the other.
--
-- Compare to @Prelude.sequence_@.
sequence_
  :: (Monad m, LL.ListLike s el, Nullable s)
  => [Iteratee s m a]
  -> Iteratee s m ()
sequence_ = self
  where
    self is = liftI step
      where
        step (Chunk xs) | LL.null xs = liftI step
        step s@(Chunk _) = do
          -- give a chunk to each iteratee
          is'  <- lift $ mapM (enumChunk s) is
          -- filter done iteratees
          is'' <- lift $ catMaybes `liftM` mapM checkIfDone is'
          if Prelude.null is''
            then idone () <=< remainingStream $ is'
            else self is''
        step s@(EOF _) = do
          s' <- remainingStream <=< lift $ mapM (enumChunk s) is
          case s' of
            EOF (Just e) -> throwErr e
            _            -> idone () s'

        checkIfDone i = runIter i
            (\_ _ -> return Nothing)
            (\k e -> return $ Just $ icont k e)

    -- returns the unconsumed part of the stream; "sequence_ is" consumes as
    -- much of the stream as the iteratee in is that consumes the most; e.g.
    -- sequence_ [I.head, I.last] consumes whole stream
    remainingStream
      :: (Monad m, Nullable s, LL.ListLike s el)
      => [Iteratee s m a] -> Iteratee s m (Stream s)
    remainingStream is = lift $
      return . Prelude.foldl1 shorter <=< mapM (\i -> runIter i od oc) $ is
      where
        od _ s = return s
        oc _ e = return $ case e of
          Nothing -> mempty
          _       -> EOF e

    -- return the shorter one of two streams; errors are propagated with the
    -- priority given to the "left"
    shorter c1@(Chunk xs) c2@(Chunk ys)
      | LL.length xs < LL.length ys = c1
      | otherwise                   = c2
    shorter (EOF e1 ) (EOF e2 ) = EOF (e1 `mplus` e2)
    shorter e@(EOF _) _         = e
    shorter _         e@(EOF _) = e

-- ------------------------------------------------------------------------
-- Enumerators

-- |The pure n-chunk enumerator
-- It passes a given stream of elements to the iteratee in @n@-sized chunks.
enumPureNChunk :: (Monad m, LL.ListLike s el) => s -> Int -> Enumerator s m a
enumPureNChunk str n iter
  | LL.null str = return iter
  | n > 0       = enum' str iter
  | otherwise   = error $ "enumPureNChunk called with n==" ++ show n
  where
    enum' str' iter'
      | LL.null str' = return iter'
      | otherwise    = let (s1, s2) = LL.splitAt n str'
                           on_cont k Nothing = enum' s2 . k $ Chunk s1
                           on_cont k e = return $ icont k e
                       in runIter iter' idoneM on_cont
{-# INLINE enumPureNChunk #-}


-- ------------------------------------------------------------------------
-- Monadic functions

-- | Map a monadic function over the elements of the stream and ignore the
-- result.
mapM_
  :: (Monad m, LL.ListLike s el, Nullable s)
  => (el -> m b)
  -> Iteratee s m ()
mapM_ f = liftI step
  where
    step (Chunk xs) | LL.null xs = liftI step
    step (Chunk xs) = lift (LL.mapM_ f xs) >> liftI step
    step s@(EOF _)  = idone () s
{-# INLINE mapM_ #-}

-- |The analogue of @Control.Monad.foldM@
foldM
  :: (Monad m, LL.ListLike s b, Nullable s)
  => (a -> b -> m a)
  -> a
  -> Iteratee s m a
foldM f e = liftI step
  where
    step (Chunk xs) | LL.null xs = liftI step
    step (Chunk xs) = do
        x <- lift $ f e (LL.head xs)
        joinIM $ enumPure1Chunk (LL.tail xs) (foldM f x)
    step (EOF _) = return e
{-# INLINE foldM #-}
