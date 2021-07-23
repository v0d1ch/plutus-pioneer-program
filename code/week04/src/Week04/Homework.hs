{-# LANGUAGE DataKinds         #-}
{-# LANGUAGE DeriveAnyClass    #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications  #-}
{-# LANGUAGE TypeOperators     #-}

module Week04.Homework where

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Default          (def)
import           Data.Functor          (void)
import qualified Data.Map.Strict       as Map
import           Data.Text             (Text)
import           GHC.Generics          (Generic)
import           Ledger
import           Ledger.Ada            as Ada
import           Ledger.Constraints    as Constraints
import           Plutus.Contract       as Contract
import           Plutus.Trace.Emulator as Emulator
import           Wallet.Emulator       (Wallet (..), walletPubKey)

data PayParams = PayParams
    { ppRecipient :: PubKeyHash
    , ppLovelace  :: Integer
    } deriving (Show, Generic, FromJSON, ToJSON)

type PaySchema = Endpoint "pay" PayParams

payContract :: Contract () PaySchema Text ()
payContract = do
    pp <- endpoint @"pay"
    let tx = mustPayToPubKey (ppRecipient pp) $ lovelaceValueOf $ ppLovelace pp
    void $ submitTx tx
    payContract

-- A trace that invokes the pay endpoint of payContract on Wallet 1 twice, each time with Wallet 2 as
-- recipient, but with amounts given by the two arguments. There should be a delay of one slot
-- after each endpoint call.
payTrace :: Integer -> Integer -> EmulatorTrace ()
payTrace x y = do
  let recipient = pubKeyHash $ walletPubKey $ Wallet 2
  let pp =
        PayParams recipient x
  h <- activateContractWallet (Wallet 1) payContract
  callEndpoint @"pay" h pp
  void $ Emulator.waitNSlots 1
  let pp' =
        PayParams recipient y
  callEndpoint @"pay" h pp'
  void $ Emulator.waitNSlots 1


payTest1 :: IO ()
payTest1 = runEmulatorTraceIO $ payTrace 1000000 2000000

payTest2 :: IO ()
payTest2 = do
  let defValue = toValue $ lovelaceOf 10000000000
  let walletsAndValues = Map.fromList [(Wallet 1, defValue), (Wallet 2, defValue)]
  runEmulatorTraceIO'
    def
    (def { _initialChainState = Left walletsAndValues })
    def $ payTrace 1000000000 2000000
