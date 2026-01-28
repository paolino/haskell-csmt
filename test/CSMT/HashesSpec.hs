module CSMT.HashesSpec (spec) where

import CSMT.Hashes
    ( Hash
    , mkHash
    , parseProof
    , renderProof
    )
import CSMT.Interface (Direction (..), Indirect (..))
import CSMT.Proof.Insertion (InclusionProof (..), ProofStep (..))
import Data.ByteString qualified as B
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, Testable (..), elements, forAll, listOf)

genProofs :: Gen (InclusionProof Hash)
genProofs = do
    proofKey <- listOf $ elements [L, R]
    proofValue <- mkHash . B.pack <$> listOf (elements [0 .. 255])
    proofRootHash <- mkHash . B.pack <$> listOf (elements [0 .. 255])
    proofRootJump <- listOf $ elements [L, R]
    proofSteps <- listOf $ do
        stepConsumed <- (+ 1) . length <$> listOf (elements [L, R])
        siblingValue <- mkHash . B.pack <$> listOf (elements [0 .. 255])
        siblingJump <- listOf $ elements [L, R]
        return
            $ ProofStep
                { stepConsumed
                , stepSibling = Indirect{jump = siblingJump, value = siblingValue}
                }
    return
        $ InclusionProof
            { proofKey
            , proofValue
            , proofRootHash
            , proofSteps
            , proofRootJump
            }

spec :: Spec
spec = describe "Hashes" $ do
    it "renders and parses proofs correctly"
        $ property
        $ forAll genProofs
        $ \proof -> do
            let
                rendered = renderProof proof
                parsed = parseProof rendered
            parsed `shouldBe` Just proof
