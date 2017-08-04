{-# language OverloadedStrings #-}
{-# language GeneralizedNewtypeDeriving #-}
{-# language DerivingStrategies #-}
module Lib where

import Control.Monad.Reader (ReaderT(..))
import Control.Monad.State (State(..))
import Data.Bifunctor
import Data.Functor.Identity (Identity)
import Data.Int (Int64)
import Data.Map (Map)
import Data.Text (Text)
import Numeric.Natural (Natural)

data Command name ann
  = Read [Acct name ann] Time
  | Transfer (Acct name ann) (Acct name ann) Fungible
-- the leaves in a transfer must be of the same unit
  | AdminAdjust

newtype Time = Time Natural
  deriving (Enum, Eq, Real, Integral, Num, Ord, Show)

newtype Fungible = Fungible Int64
  deriving (Enum, Eq, Real, Integral, Num, Ord, Show)

-- Note: this notion of account is rather weak. This could be encoded as nested
-- sigmas.
-- or an unboxed tuple sigma telescope :)
--  ie \pi {} -> \sigma {f1 : A , f2 : B f1 , ... }
newtype Acct name ann = Acct  [(name, ann)]
instance Bifunctor Acct where
  bimap f g (Acct ls) = Acct $ fmap (bimap f g) ls

data Ann
  = Bank
  | Account
  | Category
  | FungibleUnit { sort :: Text, identity :: Text }

{-
   right now we're ignore what the premission / auth model is,
   or treating / equating writing the write name prefix AS the authorization

  Idea: any path that ends in  , (name : Account) ,
  the "value" of that "key" must be some public key, possibly signed by
  parent account levels or other domain applicable trust system

  this doesn't address trust model for root nodes like Bank,
  but for child levels *should* be adequate

  also allows for delegation
  -}

joelAcct :: Acct Text Ann
joelAcct = Acct
  [ ("Bank", Bank)
  , ("custody", Category)
  , ("joel", Account)
  , ("equities", Category)
  -- a little redundant right now but we want to keep simply typed
  , ("AAPL", FungibleUnit "Stock" "AAPL")
  ]

carterAcct :: Acct Text Ann
carterAcct = Acct
  [ ("Bank", Bank)
  , ("custody", Category)
  , ("carter", Account)
  ]

exampleRead :: Command Text Ann
exampleRead = Read [joelAcct] 0

-- arguably valid -- just graft eqities/AAPL into Carter's account
-- desugared: do administrative init for that path, then do point transfer
exampleTransfer :: Command Text Ann
exampleTransfer = Transfer carterAcct joelAcct 500

--- not quite right , we want SIGMASSS
type SystemFungibleState = Map (Acct Text Ann) Natural

newtype Path = Path [Text]
data Diff -- TODO

type FungibleM a = ReaderT SystemFungibleState (State SystemFungibleState) a

interpImpl :: [Command Text Ann] -> FungibleM ()
interpImpl = _

-- inner state is initially empty map, then becomes the "current" state
-- for a value once its used
-- -- at the end we can treat the values in the state
interp :: [Command Text Ann] -> SystemFungibleState -> Identity (Map Path Diff)
interp cmds state =
  let runFungible :: FungibleM () -> Identity (Map Path Diff)
      runFungible = _
  in runFungible (interpImpl cmds)
