{-# Language BangPatterns
           , LambdaCase
           , RecordWildCards
           #-}
module Database.HypherGraph.IO
 ( openMMArray
 , closeMMArray
 , putMMArrayItem
 , getMMArrayItem
 , resizeMMArray
 ) where

import Database.HypherGraph.Types

import System.IO.MMap
import Foreign.Ptr
import Foreign.ForeignPtr

import Foreign.Storable

import qualified Filesystem      as FS
import qualified Filesystem.Path as FS
import qualified Filesystem.Path.CurrentOS as FS

import Control.Concurrent.MVar

import Data.Bool

import Control.Applicative

minFreeRecords :: Int
minFreeRecords = 10

getMinSize file rs rc = do
   let ma = max (rs * rc)
   FS.isFile file >>= \case
     False -> return $ ma 0
     True  -> ma . fromIntegral <$> FS.getSize file

openMMArray :: (Storable a) => FS.FilePath -> IO (MMArray a)
openMMArray f = MMArray <$> ( newMVar =<< openMMArray' f minFreeRecords undefined )

resizeMMArray :: (Storable a) =>  Int ->  MMArray a -> IO ()
resizeMMArray i (MMArray m) = (modifyMVar_ m $ resizeMMArray__ i)

putMMArrayItem :: Storable a => (MMArray a) -> Int -> a -> IO ()
putMMArrayItem (MMArray m) i a = modifyMVar_ m $ \g -> do
       if (i < (maxRec g))
        then
          pokeElemOff (storeMM g) i a >> return g
        else  do
          g1 <- resizeMMArray__ i g
          pokeElemOff (storeMM g1) i a
          return g1

getMMArrayItem ::  Storable a => (MMArray a) -> Int -> IO a
getMMArrayItem (MMArray m) i = withMVar m $ \m1 -> peekElemOff (storeMM m1) i


-- MMArray__{..} i | maxRec > i = pokeElemOff storeMM i


openMMArray' :: (Storable a) => FS.FilePath -> Int -> a -> IO (MMArray__ a)
openMMArray' file mfr a = do
    let si = (sizeOf a)
    fs <- getMinSize file si mfr
    MMArray__ <$> pure file
              <*> open file fs
              <*> pure fs
              <*> pure si
              <*> pure ( fs `div` si)


resizeMMArray__ :: (Storable a) =>  Int ->  MMArray__ a -> IO (MMArray__ a)
resizeMMArray__ i (MMArray__ {..}) = do
     munmapFilePtr storeMM fileSize
     openMMArray' fileName (i + 1) undefined

allox = 1024 * 1024

-- openNode :: FS.FilePath -> IO (MMArray a)

-- file = openMMArray' file allox undefined

open :: Storable a => FS.FilePath -> Int -> IO (Ptr a)
open file fs = do
  a <- mmapFilePtr
    (FS.encodeString file)
    ReadWriteEx
    (Just (0,fs))
  --Trace ?
  print a
  let (p,_,_,_) = a
  return  p

closeMMArray m i = munmapFilePtr (storeMM m) i
