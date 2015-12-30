module Database.HypherGraph.IO.Node where

import Database.HypherGraph.IO
import Database.HypherGraph.Types

import qualified Filesystem.Path as FS

openNode :: FS.FilePath -> IO (MMArray Node)
openNode = openMMArray

getNode :: (MMArray Node) -> Int -> IO Node
getNode = getMMArrayItem

putNode :: (MMArray Node) -> Int -> Node -> IO ()
putNode = putMMArrayItem

delNode :: (MMArray Node) -> Int -> IO ()
delNode mn i = do
  n <- getNode mn i
  putNode mn i n{nodeInUse=False}
