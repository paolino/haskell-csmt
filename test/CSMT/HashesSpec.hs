module CSMT.HashesSpec (spec) where

import CSMT.Hashes
    ( Hash
    , mkHash
    , parseProof
    , renderProof
    )
import CSMT.Interface (Direction (..), Indirect (..))
import CSMT.Proof.Insertion (Proof (..), ProofStep (..))
import Data.ByteString qualified as B
import Test.Hspec (Spec, describe, it, shouldBe)
import Test.QuickCheck (Gen, Testable (..), elements, forAll, listOf)

genProofs :: Gen (Proof Hash)
genProofs = do
    proofRootJump <- listOf $ elements [L, R]
    proofSteps <- listOf $ do
        dir <- elements [L, R]
        siblingValue <- mkHash . B.pack <$> listOf (elements [0 .. 255])
        stepJump <- listOf $ elements [L, R]
        siblingJump <- listOf $ elements [L, R]
        return
            $ ProofStep
                { stepDirection = dir
                , stepSibling = Indirect{jump = siblingJump, value = siblingValue}
                , stepJump = stepJump
                }
    return $ Proof{proofSteps, proofRootJump}

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
