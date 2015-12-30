
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

data MMArrayDyn__
  = MMArrayDyn__
  { fileNameStore :: FS.FilePath
  , storePtr      :: Ptr Word8
  , lookup        :: MMArray__ OffLen
  }

newtype MMArrayDyn = MMArrayDyn (MVar (MMArrayDyn__))

justIfBitSet b ptr offset = bool (pure Nothing)  (Just <$> peek ( (castPtr ptr) `plusPtr` offset )) b
-- ^ if correct mask bit is set deserialize to just otherwise to nothing

pnn ptr offset Nothing = return ()
pnn ptr offset (Just v) = poke ((castPtr ptr) `plusPtr` offset ) v
-- ^ on nothing do nothing on something poke it

nc n True v = v `setBit` n
nc _ _    v = v

data Node = Node
  { nodeInUse   :: Bool
  , nextRelId   :: Maybe Word32
  , nextPropId  :: Maybe Word32
  } deriving (Show, Typeable, Generic)

instance Storable Node where
  sizeOf    _ = 9
  alignment _ = 9

  peek ptr = do
     c <- testBit <$> ((peek (castPtr ptr)) :: IO Word8)
     Node <$> pure         (c 0)
          <*> justIfBitSet (c 1) ptr 1
          <*> justIfBitSet (c 2) ptr 5

  poke ptr (Node {..}) = do
        poke (castPtr ptr) $ nc 0 nodeInUse
                           $ nc 1 (isJust nextRelId)
                           $ nc 2 (isJust nextPropId)
                           $ (0::Word8)
        pnn ptr 1 nextRelId
        pnn ptr 5 nextPropId

data Relationship = Relationship
  { relInUse        :: Bool
  , relFirstNode    :: Maybe Word32
  , relSecondNode   :: Maybe Word32
  , relType         :: Maybe Word32
  , relFirstPrevId  :: Maybe Word32
  , relFirstNextId  :: Maybe Word32
  , relSecondPrevId :: Maybe Word32
  , relSecondNextId :: Maybe Word32
  , relNextPropId   :: Maybe Word32
  } deriving (Show, Typeable, Generic)

instance Storable Relationship where
  sizeOf    _ = 33
  alignment _ = 33

  peek ptr = do
     bb <- ((peek (castPtr ptr)) :: IO Word8)
     let c = testBit bb
     Relationship
          <$> pure (bb /= 0)
          <*> justIfBitSet (c 0) ptr  1
          <*> justIfBitSet (c 1) ptr  5
          <*> justIfBitSet (c 2) ptr  9
          <*> justIfBitSet (c 3) ptr 13
          <*> justIfBitSet (c 4) ptr 17
          <*> justIfBitSet (c 5) ptr 21
          <*> justIfBitSet (c 6) ptr 25
          <*> justIfBitSet (c 7) ptr 29

  poke ptr (Relationship {..}) = do
        poke (castPtr ptr) $ nc 0 (isJust relFirstNode)
                           $ nc 1 (isJust relSecondNode)
                           $ nc 2 (isJust relType)
                           $ nc 3 (isJust relFirstPrevId)
                           $ nc 4 (isJust relFirstNextId)
                           $ nc 5 (isJust relSecondPrevId)
                           $ nc 6 (isJust relSecondNextId)
                           $ nc 7 (isJust relNextPropId)
                           $ (0::Word8)
        pnn ptr  1 relFirstNode
        pnn ptr  5 relSecondNode
        pnn ptr  9 relType
        pnn ptr 13 relFirstPrevId
        pnn ptr 17 relFirstNextId
        pnn ptr 21 relSecondPrevId
        pnn ptr 25 relSecondNextId
        pnn ptr 29 relNextPropId

data RelationshipType = RelationshipType
  { rtInUse    :: Bool
  , rtBlock    :: Maybe Word32
  } deriving (Show, Typeable, Generic)

instance Storable RelationshipType where
  sizeOf    _ = 5
  alignment _ = 5

  peek ptr = do
     c <- testBit <$> ((peek (castPtr ptr)) :: IO Word8)
     RelationshipType <$> pure         (c 0)
                      <*> justIfBitSet (c 1) ptr 1

  poke ptr (RelationshipType {..}) = do
        poke (castPtr ptr) $ nc 0 rtInUse
                           $ nc 1 (isJust rtBlock)
                           $ (0::Word8)
        pnn ptr 1 rtBlock

data Property = Property
  { pOffset ::
  , pLength ::
  } deriving (Show, Typeable, Generic)

data PropertyIndex = PropertyIndex
  {
  } deriving (Show, Typeable, Generic)

data DynamicStore = DynamicStore
  {
  } deriving (Show, Typeable, Generic)

data NeoStore = NeoStore
  {
  } deriving (Show, Typeable, Generic)

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
