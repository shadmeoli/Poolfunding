-- I made this in relation to how Paylend works(my point of view of how it works)

import Control.Applicative (Applicative (pure))
import Control.Monad (void)
import Data.Default (Default (def))
import Data.Text (Text)
import Ledger (POSIXTime, POSIXTimeRange, PaymentPubKeyHash (unPaymentPubKeyHash), ScriptContext (..), TxInfo (..),
               Validator, getCardanoTxId)
import Ledger qualified
import Ledger.Contexts qualified as V
import Ledger.Interval qualified as Interval
import Ledger.Scripts qualified as Scripts
import Ledger.TimeSlot qualified as TimeSlot
import Ledger.Typed.Scripts qualified as Scripts hiding (validatorHash)
import Ledger.Value (Value)
import Playground.Contract
import Plutus.Contract
import Plutus.Contract.Constraints qualified as Constraints
import Plutus.Contract.Typed.Tx qualified as Typed
import PlutusTx qualified
import PlutusTx.Prelude hiding (Applicative (..), Semigroup (..))
import Prelude (Semigroup (..))
import Prelude qualified as Haskell
import Wallet.Emulator qualified as Emulator

-- Crowdfunding
data Campaign = Campaign

    { campaignDeadline           :: POSIXTime
    , campaignCollectionDeadline :: POSIXTime
    , campaignOwner              :: PaymentPubKeyHash

    --   funds if the campaign is successful.
    } deriving (Generic, ToJSON, FromJSON, ToSchema)

PlutusTx.makeLift ''Campaign

-- conditions
data CampaignAction = Collect | Refund

PlutusTx.unstableMakeIsData ''CampaignAction
PlutusTx.makeLift ''CampaignAction

type CrowdfundingSchema =
        Endpoint "schedule collection" ()
        .\/ Endpoint "contribute" Contribution

newtype Contribution = Contribution
        { contribValue :: Value
        -- contribution
        } deriving stock (Haskell.Eq, Show, Generic)
          deriving anyclass (ToJSON, FromJSON, ToSchema, ToArgument)


--   using the wallet's public key.
mkCampaign :: POSIXTime -> POSIXTime -> Wallet -> Campaign
mkCampaign ddl collectionDdl ownerWallet =
    Campaign
        { campaignDeadline = ddl
        , campaignCollectionDeadline = collectionDdl
        , campaignOwner = Emulator.mockWalletPaymentPubKeyHash ownerWallet
        }


collectionRange :: Campaign -> POSIXTimeRange
collectionRange cmp =
    Interval.interval (campaignDeadline cmp) (campaignCollectionDeadline cmp - 1)

refundRange :: Campaign -> POSIXTimeRange
refundRange cmp =
    Interval.from (campaignCollectionDeadline cmp)

data Crowdfunding
instance Scripts.ValidatorTypes Crowdfunding where
    type instance RedeemerType Crowdfunding = CampaignAction
    type instance DatumType Crowdfunding = PaymentPubKeyHash

typedValidator :: Campaign -> Scripts.TypedValidator Crowdfunding
typedValidator = Scripts.mkTypedValidatorParam @Crowdfunding
    $$(PlutusTx.compile [|| mkValidator ||])
    $$(PlutusTx.compile [|| wrap ||])
    where
        wrap = Scripts.wrapValidator

{-# INLINABLE validRefund #-}
validRefund :: Campaign -> PaymentPubKeyHash -> TxInfo -> Bool
validRefund campaign contributor txinfo =

    (refundRange campaign `Interval.contains` txInfoValidRange txinfo)
    && (txinfo `V.txSignedBy` unPaymentPubKeyHash contributor)

validCollection :: Campaign -> TxInfo -> Bool
validCollection campaign txinfo =

    (collectionRange campaign `Interval.contains` txInfoValidRange txinfo)
    && (txinfo `V.txSignedBy` unPaymentPubKeyHash (campaignOwner campaign))


mkValidator :: Campaign -> PaymentPubKeyHash -> CampaignAction -> ScriptContext -> Bool
mkValidator c con act p = case act of

    Refund  -> validRefund c con (scriptContextTxInfo p)
    Collect -> validCollection c (scriptContextTxInfo p)


-- campaign validation
contributionScript :: Campaign -> Validator
contributionScript = Scripts.validatorScript . typedValidator

-- | The address of a [[Campaign]]
campaignAddress :: Campaign -> Ledger.ValidatorHash
campaignAddress = Scripts.validatorHash . contributionScript

-- | The crowdfunding contract for the 'Campaign'.
crowdfunding :: AsContractError e => Campaign -> Contract () CrowdfundingSchema e ()
crowdfunding c = selectList [contribute c, scheduleCollection c]

-- | A sample campaign
theCampaign :: POSIXTime -> Campaign
theCampaign startTime = Campaign
    { campaignDeadline = startTime + 40000
    , campaignCollectionDeadline = startTime + 60000
    , campaignOwner = Emulator.mockWalletPaymentPubKeyHash (Emulator.knownWallet 1)
    }

-- refunds (no funding)
contribute :: AsContractError e => Campaign -> Promise () CrowdfundingSchema e ()
contribute cmp = endpoint @"contribute" $ \Contribution{contribValue} -> do
    contributor <- ownPaymentPubKeyHash
    let inst = typedValidator cmp
        tx = Constraints.mustPayToTheScript contributor contribValue
                <> Constraints.mustValidateIn (Interval.to (campaignDeadline cmp))
    txid <- fmap getCardanoTxId $ mkTxConstraints (Constraints.typedValidatorLookups inst) tx
        >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx

    utxo <- watchAddressUntilTime (Scripts.validatorAddress inst) (campaignCollectionDeadline cmp)

    
    -- claim refund

    let flt Ledger.TxOutRef{txOutRefId} _ = txid Haskell.== txOutRefId
        tx' = Typed.collectFromScriptFilter flt utxo Refund
                <> Constraints.mustValidateIn (refundRange cmp)
                <> Constraints.mustBeSignedBy contributor
    if Constraints.modifiesUtxoSet tx'
    then do
        logInfo @Text "Claiming refund"
        void $ mkTxConstraints (Constraints.typedValidatorLookups inst
                             <> Constraints.unspentOutputs utxo) tx'
            >>= submitUnbalancedTx . Constraints.adjustUnbalancedTx
    else pure ()


-- Campaign cheak and contribution evaluation
scheduleCollection :: AsContractError e => Campaign -> Promise () CrowdfundingSchema e ()
scheduleCollection cmp =

    endpoint @"schedule collection" $ \() -> do
        let inst = typedValidator cmp

        _ <- awaitTime $ campaignDeadline cmp
        unspentOutputs <- utxosAt (Scripts.validatorAddress inst)

        let tx = Typed.collectFromScript unspentOutputs Collect
                <> Constraints.mustValidateIn (collectionRange cmp)
        void $ submitTxConstraintsSpending inst unspentOutputs tx


endpoints :: AsContractError e => Contract () CrowdfundingSchema e ()
endpoints = crowdfunding (theCampaign $ TimeSlot.scSlotZeroTime def)

mkSchemaDefinitions ''CrowdfundingSchema

$(mkKnownCurrencies [])
