{-# LANGUAGE DeriveGeneric #-}
module Todo where

import Control.Error.Safe (justErr)
import Data.Functor ((<&>))
import Polysemy
import Polysemy.Error
import KVS
import MonotonicSequence
import qualified Data.Map.Strict as M
import Data.Aeson.Types
import GHC.Generics

type Key = Int

newtype TodoError = TodoNotAvailable Int

data Todo =
  Todo
    { _title :: String
    , _completed :: Bool
    }
  deriving (Eq, Show, Generic)

instance ToJSON Todo
instance FromJSON Todo

newTodo :: String -> Todo
newTodo title  = Todo title False

add :: Members '[KVS Key Todo, MonotonicSequence Key] r => Todo -> Sem r Key
add todo = do
  key <- next
  insertKvs key todo
  pure key

list :: Member (KVS Key Todo) r => Sem r (M.Map Key Todo)
list = fmap M.fromList listAllKvs

fetch :: ( Member (KVS Key Todo) r
       , Member (Error TodoError) r
       ) => Key -> Sem r Todo
fetch k =
  getKvs k >>= \case
    Just todo -> pure todo
    Nothing -> throw $ TodoNotAvailable k

toggle :: Members '[KVS Key Todo, Error TodoError] r => Key -> Sem r Todo
toggle k = do
  todoErr <- getKvs k <&> justErr (TodoNotAvailable k)
  todo <- either throw pure todoErr
  let completed = _completed todo
  let modifiedTodo = todo {_completed = not completed}
  insertKvs k modifiedTodo
  pure modifiedTodo
