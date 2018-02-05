-- | The kernel of the wallet implementation
--
-- The goal is to keep this module mostly self-contained, and not use to many
-- Cardano specific types (except those types that appear in the translation
-- of the UTxO DSL).
module Cardano.Wallet.Kernel (
    Wallet -- opaque
  , bracketWalletResources
  , init
  ) where

import Universum

-- | The main wallet abstraction
--
-- We call into this abstraction both from the node as well as from unit tests.
-- The concrete representation is not exported.
data Wallet = Wallet

-- | Allocate wallet resources
--
-- TODO: We'll want some constraints on the monad here, but we need to make
-- sure not to make it /too/ specific so that we can initialize the wallet
-- also from the unit tests.
bracketWalletResources :: (Wallet -> m a) -> m a
bracketWalletResources = ($ Wallet)

-- | Initialize the wallet
--
-- This is separate from allocating the wallet resources, and will only be
-- called when the node is initialized (when run in the node proper).
--
-- TODO: Again, we'll want some constraints on this monad here, but again
-- it shouldn't be too specific.
init :: Monad m => Wallet -> m ()
init _ = return ()
