module Cardano.Wallet.Kernel.BListener (
    onApplyBlocks
  , onRollbackBlocks
  ) where

import Universum

import Pos.Block.Types
import Pos.Util.Chrono
import Pos.DB

import Cardano.Wallet.Kernel

-- | Callback for new blocks
--
-- TODO: This should wrap the functionality in "Cardano.Wallet.Core" to
-- wrap things in Cardano specific types.
onApplyBlocks :: Wallet -> OldestFirst NE Blund -> m SomeBatchOp
onApplyBlocks = error "TODO: Cardano.Wallet.Core.BListener.onApplyBlocks"

-- | Callback for rollbacks
--
-- TODO: This should wrap the functionality in "Cardano.Wallet.Core" to
-- wrap things in Cardano specific types.
onRollbackBlocks :: Wallet -> NewestFirst NE Blund -> m SomeBatchOp
onRollbackBlocks = error "TODO: Cardano.Wallet.Core.BListener.onRollbackBlocks"
