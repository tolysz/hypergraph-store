
{-# Language GeneralizedNewtypeDeriving
  , DeriveGeneric
  , DeriveDataTypeable
  , RecordWildCards
  , LambdaCase
  , RankNTypes
  #-}

module Database.HypherGraph.Types where

-- import Control.Monad.Reader
-- import Control.Monad.State


import Data.Word
import Data.Bits
import Foreign.Storable
import Foreign.Ptr
import Data.Bool
import Data.Maybe
--import Data.Proxy
import Data.Tagged


import GHC.Generics
import Data.Typeable
import Control.Applicative
import Control.Monad

import qualified Filesystem.Path as FS

import Control.Concurrent.MVar

data MMArray__ a
  = MMArray__
    { fileName :: FS.FilePath
    , storeMM  :: Ptr a
    , fileSize :: Int
    , recSize  :: Int
    , maxRec   :: Int
    }

newtype MMArray a = MMArray (MVar (MMArray__ a))



justIfBitSet b ptr offset = bool (pure Nothing)  (Just <$> peek ( (castPtr ptr) `plusPtr` offset )) b
-- ^ if correct mask bit is set deserialize to just otherwise to nothing

pnn ptr offset Nothing = return ()
pnn ptr offset (Just v) = poke ((castPtr ptr) `plusPtr` offset ) v
-- ^ on nothing do nothing on something poke it

nc n True v = v `setBit` n
nc _ _    v = v


data Node = Node
 { inUse       :: Bool
 , nextRelId   :: Maybe Word32
 , nextPropId  :: Maybe Word32
 } deriving (Show, Typeable, Generic)

instance Storable Node where
  sizeOf    _ = 9
  alignment _ = 9

--  peekElemOff :: GHC.Ptr.Ptr a -> Int -> IO a
  peek ptr = do
     c <- testBit <$> ((peek (castPtr ptr)) :: IO Word8)
     Node <$> pure         (c 0)
          <*> justIfBitSet (c 1) ptr 1
          <*> justIfBitSet (c 2) ptr 5

  poke ptr (Node {..}) = do
        poke (castPtr ptr) $ nc 0 inUse
                           $ nc 1 (isJust nextRelId)
                           $ nc 2 (isJust nextPropId)
                           $ (0::Word8)
        pnn ptr 1 nextRelId
        pnn ptr 5 nextPropId
{--
  poke ptr (Node {..}) = do
                     poke (castPtr ptr) (
                     poke ((castPtr ptr) `plusPtr` 5 ) nextPropId
--}
--  pokeElemOff :: GHC.Ptr.Ptr a -> Int -> a -> IO ()

--  peekByteOff :: GHC.Ptr.Ptr b -> Int -> IO a
--  pokeByteOff :: GHC.Ptr.Ptr b -> Int -> a -> IO ()
--  peek :: GHC.Ptr.Ptr a -> IO a
--  poke :: GHC.Ptr.Ptr a -> a -> IO ()
        -- Defined in ‘Foreign.Storable’


{--
data HGConfig = HGConfig String deriving Show
data HGState  = HGState  deriving Show
--}


-- open :: String -> IO ( HypherGraph () )
-- open f = undefined
-- return . HGConfig

{-- 
newtype HypherGraph a = HypherGraph
  { runHGM :: ReaderT HGConfig (StateT HGState IO) a
  } deriving (Monad, MonadIO, MonadReader HGConfig,
                MonadState HGState)

runHG file = let
               config = HGConfig file
               state  = HGState
               init   = return ()
             in
               runStateT (runReaderT (runHGM init) config) state
--}



{--
runMyApp :: MyApp a -> Int -> IO (a, AppState)
runMyApp k maxDepth =
    let config = AppConfig maxDepth
        state = AppState 0
    in runStateT (runReaderT (runA k) config) state
--}

-- lets have plenty of redirections so we can play with where exactly we store.


-- we will need to have
-- NodeId 
-- Let's number nodes by location in initial index file.
-- index stores status+(cluster,location)+length
-- storeFile


-- Get node(5) -> fileLookup (offset + 5 * nodesize)

{-
clusters?

nodes
relationships
properties

use - rel - prop

-}