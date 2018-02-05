{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveTraversable          #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures             #-}
{-# LANGUAGE RecordWildCards            #-}
module Main where

import           Universum

import           Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy.Char8 as BL8
import           Data.Maybe (fromJust)
import           Mockable (Production (..), runProduction)
import           Pos.Communication (ActionSpec (..))
import           Pos.DB.DB (initNodeDBs)
import           Pos.Launcher (NodeParams (..), NodeResources (..), bracketNodeResources,
                               loggerBracket, runNode, withConfigurations)
import           Pos.Launcher.Configuration (ConfigurationOptions, HasConfigurations)
import           Pos.Ssc.Types (SscParams)
import           Pos.Txp (txpGlobalSettings)
import           Pos.Update.Configuration (HasUpdateConfiguration)
import           Pos.Util.CompileInfo (HasCompileInfo, retrieveCompileTimeInfo, withCompileInfo)
import           Pos.Util.UserSecret (usVss)
import           Pos.Wallet.Web (bracketWalletWS, bracketWalletWebDB, getSKById, getWalletAddresses,
                                 runWRealMode, syncWalletsWithGState, AddrCIdHashes (..))
import           Pos.Wallet.Web.Mode (WalletWebMode)
import           Pos.Wallet.Web.State (flushWalletStorage)
import           System.Wlog (LoggerName, logInfo)

import qualified Cardano.Wallet.API.V1.Swagger as Swagger
import           Cardano.Wallet.Server.CLI (WalletBackendParams (..), WalletDBOptions (..),
                                            WalletStartupOptions (..), getWalletNodeOptions,
                                            isDebugMode,
                                            ChooseWalletBackend (..), NewWalletBackendParams (..))
import qualified Cardano.Wallet.Server.Plugins as Plugins
import qualified Cardano.Wallet.Kernel      as Kernel
import qualified Cardano.Wallet.Kernel.Mode as Kernel.Mode
import qualified Pos.Client.CLI as CLI


loggerName :: LoggerName
loggerName = "node"

{-
   Most of the code below has been copied & adapted from wallet/node/Main.hs as a path
   of least resistance to make the wallet-new prototype independent (to an extend)
   from breaking changes to the current wallet.
-}

-- | The "workhorse" responsible for starting a Cardano edge node plus a number of extra plugins.
actionWithWallet :: (HasConfigurations, HasCompileInfo)
                 => SscParams
                 -> NodeParams
                 -> WalletBackendParams
                 -> Production ()
actionWithWallet sscParams nodeParams wArgs@WalletBackendParams {..} =
    bracketWalletWebDB (walletDbPath walletDbOptions) (walletRebuildDb walletDbOptions) $ \db ->
        bracketWalletWS $ \conn ->
            bracketNodeResources nodeParams sscParams
                txpGlobalSettings
                initNodeDBs $ \nr@NodeResources {..} -> do
                    ref <- newIORef mempty
                    runWRealMode db conn (AddrCIdHashes ref) nr (mainAction nr)
  where
    mainAction = runNodeWithInit $ do
        when (walletFlushDb walletDbOptions) $ do
            logInfo "Flushing wallet db..."
            flushWalletStorage
            logInfo "Resyncing wallets with blockchain..."
            syncWallets

    runNodeWithInit init nr =
        let (ActionSpec f, outs) = runNode nr plugins
         in (ActionSpec $ \s -> init >> f s, outs)

    syncWallets :: WalletWebMode ()
    syncWallets = do
        sks <- getWalletAddresses >>= mapM getSKById
        syncWalletsWithGState sks

    plugins :: HasConfigurations => Plugins.Plugin WalletWebMode
    plugins = mconcat [ Plugins.conversation wArgs
                      , Plugins.walletBackend wArgs
                      , Plugins.acidCleanupWorker wArgs
                      , Plugins.resubmitterPlugin
                      , Plugins.notifierPlugin
                      ]

actionWithNewWallet :: (HasConfigurations, HasCompileInfo)
                    => SscParams
                    -> NodeParams
                    -> NewWalletBackendParams
                    -> Production ()
actionWithNewWallet sscParams nodeParams NewWalletBackendParams {} =
    bracketNodeResources
        nodeParams
        sscParams
        txpGlobalSettings
        initNodeDBs $ \nr ->
      Kernel.bracketWalletResources $ \wallet ->
        Kernel.Mode.runWalletMode nr wallet (mainAction nr)
  where
    mainAction = runNodeWithInit $ Kernel.init =<< Kernel.Mode.getWallet

    runNodeWithInit init nr =
        let (ActionSpec f, outs) = runNode nr plugins
         in (ActionSpec $ \s -> init >> f s, outs)

    plugins :: HasConfigurations => Plugins.Plugin Kernel.Mode.WalletMode
    plugins = mconcat []


-- | Runs an edge node plus its wallet backend API.
startEdgeNode :: HasCompileInfo
              => WalletStartupOptions
              -> Production ()
startEdgeNode WalletStartupOptions{..} = do
  withConfigurations conf $ do
      (sscParams, nodeParams) <- getParameters
      case wsoWalletBackendParams of
        WalletLegacy legacyParams -> do
          when (isDebugMode $ walletRunMode legacyParams) $
              generateSwaggerDocumentation
          actionWithWallet sscParams nodeParams legacyParams
        WalletNew newParams ->
          actionWithNewWallet sscParams nodeParams newParams
  where
    getParameters :: HasConfigurations => Production (SscParams, NodeParams)
    getParameters = do

      whenJust (CLI.cnaDumpGenesisDataPath wsoNodeArgs) $ CLI.dumpGenesisData True
      currentParams <- CLI.getNodeParams loggerName wsoNodeArgs nodeArgs
      let vssSK = fromJust $ npUserSecret currentParams ^. usVss
      let gtParams = CLI.gtSscParams wsoNodeArgs vssSK (npBehaviorConfig currentParams)

      CLI.printInfoOnStart wsoNodeArgs
      logInfo "Wallet is enabled!"

      return (gtParams, currentParams)

    conf :: ConfigurationOptions
    conf = CLI.configurationOptions $ CLI.commonArgs wsoNodeArgs

    nodeArgs :: CLI.NodeArgs
    nodeArgs = CLI.NodeArgs { CLI.behaviorConfigPath = Nothing }

-- | Generates the updated spec and store it in the appropriate folder.
-- the reason why we don't generate a yaml file is because for swagger-ui is actually
-- much better to start with the JSON input, as the tool is capable of generating
-- better-looking YAMLs.
generateSwaggerDocumentation :: ( MonadIO m
                                , HasCompileInfo
                                , HasUpdateConfiguration
                                ) => m ()
generateSwaggerDocumentation = liftIO $ do
    BL8.writeFile "wallet-new/spec/swagger.json" (encodePretty Swagger.api)
    putText "Swagger API written on disk."

-- | The main entrypoint for the Wallet.
main :: IO ()
main = withCompileInfo $(retrieveCompileTimeInfo) $ do
  cfg <- getWalletNodeOptions
  putText "Wallet is starting..."
  let loggingParams = CLI.loggingParams loggerName (wsoNodeArgs cfg)
  loggerBracket loggingParams . runProduction $ do
    logInfo "[Attention] Software is built with the wallet backend"
    startEdgeNode cfg
