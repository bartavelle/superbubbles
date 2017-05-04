{-# LANGUAGE ScopedTypeVariables #-}
-- | This module finds "super bubbles", as defined in
-- https://arxiv.org/abs/1307.7925
module Graph.Superbubbles (BubbleError(..), findSuperbubble) where

import qualified Data.Set as S
import qualified Data.Map.Strict as M
import Control.Monad
import Control.Monad.Except
import Control.Monad.State.Strict
import Control.Lens

data BubbleError = Loop -- ^ The starting node is in a loop
                 | Tip -- ^ There is a tip dangling under the starting point
                 | NotFound -- ^ No superbubble was found
                 deriving (Show, Eq)

data NodeLabel = Unvisited
               | Seen
               | Visited
               deriving (Show, Eq, Ord)

unvisited :: Ord node => M.Map node NodeLabel -> [node]
unvisited = M.keys . M.filter (== Unvisited)

findSuperbubble
  :: forall node. Ord node
  => (node -> S.Set node) -- ^ Get children of a node
  -> (node -> S.Set node) -- ^ Get parents of a node
  -> node -- ^ Starting node
  -> Either BubbleError node -- ^ Returns an error, or the corresponding exit
findSuperbubble getChilds getParents start = runExcept (evalStateT go (M.singleton start Unvisited))
  where
    go :: StateT (M.Map node NodeLabel) (Except BubbleError) node
    go = do
      queue <- unvisited <$> get
      case queue of
        [] -> throwError NotFound
        (v:_) -> do
           at v ?= Visited
           let vchilds = getChilds v
           when (null vchilds) (throwError Tip)
           forM_ vchilds $ \u -> do
             when (u == start) (throwError Loop)
             at u ?= Seen
             let uparents = getParents u
             pvisited <- M.filterWithKey (\k s -> k `S.member` uparents && s == Visited) <$> get
             when (M.size pvisited == length uparents) (at u ?= Unvisited)
           smap <- get
           case (unvisited smap, M.size (M.filter (== Seen) smap)) of
             ([t], 0) -> if start `S.member` getChilds t
                           then throwError Loop
                           else return t
             _ -> go

