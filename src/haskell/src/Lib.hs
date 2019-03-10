{-# LANGUAGE MultiWayIf      #-}
{-# LANGUAGE TemplateHaskell #-}

module Lib () where

import           Control.Lens
import           Net.IPv4
import           Relude

pingTimeoutTicks :: Integer
pingTimeoutTicks = 1

suspiciousTimeoutTicks :: Integer
suspiciousTimeoutTicks = 2

deadTimeoutTicks :: Integer
deadTimeoutTicks = 100

protocolPeriodMs :: Integer
protocolPeriodMs = 1000

-- | # Nodes
type Port = Int16

data NodeAddress = NodeAddress IPv4 Port
  deriving (Eq, Show)

data NodeHealth = Alive | Suspicious | Dead | Left
  deriving (Eq, Ord, Show)

newtype Generation = Generation Int8
  deriving (Eq, Show, Num)

instance Ord Generation where
  (Generation a) <= (Generation b) =
    (0 < a' - b') && (a' - b' < 191) || (a' - b' <= -191)
    where a' = fromIntegral a
          b' = fromIntegral b

newtype ServiceByte = ServiceByte Int8
  deriving (Eq, Show)

newtype TimesMentioned = TimesMentioned Int16
  deriving (Eq, Show)

data NodeState = NodeState { _health         :: NodeHealth
                           , _generation     :: Generation
                           , _serviceByte    :: Maybe ServiceByte
                           , _servicePort    :: Port
                           , _timesMentioned :: TimesMentioned
                           }
  deriving (Eq, Show)

makeLenses ''NodeState

type States = Map NodeAddress NodeState

merge :: NodeState -> NodeState -> NodeState
merge a b = if
  | (_generation a) > (_generation b) -> merge' a b
  | (_health a) > (_health b) -> merge' a b
  | otherwise -> merge' b a
  where
    merge' primary secondary =
          case _serviceByte primary of
            Just sb -> primary
            Nothing -> primary
              & serviceByte .~ secondary ^. serviceByte
              & servicePort .~ secondary ^. servicePort

aliveAgain :: NodeState -> Generation -> NodeState
aliveAgain ns gen = ns & generation .~ max (ns ^. generation) gen + 1

-- | # The Gossiper

data GossipState = GossipState { _address     :: IPv4
                               , _port        :: Port
                               , _servicebyte :: Int8
                               , _serviceport :: Port
                               }

makeLenses ''GossipState
