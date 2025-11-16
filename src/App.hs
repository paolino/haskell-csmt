module App (main) where

import CSMT.Backend.RocksDB
    ( RunRocksDB (RunRocksDB)
    , rocksDBCSMT
    , withRocksDB
    )
import CSMT.Hashes
    ( generateInclusionProof
    , insert
    , root
    , verifyInclusionProof
    )
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteArray.Encoding
    ( Base (Base64)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as B
import Data.ByteString.Char8 qualified as BC
import System.Console.Haskeline
    ( defaultSettings
    , getInputLine
    , runInputT
    )
import System.IO
    ( BufferMode (LineBuffering)
    , hIsTerminalDevice
    , hSetBuffering
    , isEOF
    , stdin
    , stdout
    )

data Command
    = I ByteString ByteString
    | Q ByteString
    | V ByteString ByteString
    | R

parseCommand :: ByteString -> Maybe Command
parseCommand line =
    case B.words line of
        ["i", k, v] -> Just (I k v)
        ["q", k] -> Just (Q k)
        ["v", value] -> Just (V value "")
        ["v", value, proof] -> Just (V value proof)
        ["r"] -> Just R
        _ -> Nothing

printHash :: ByteString -> ByteString -> IO ()
printHash what = B.putStrLn . ((what <> ": ") <>) . convertToBase Base64

readHash :: ByteString -> Maybe ByteString
readHash bs = case convertFromBase Base64 bs of
    Left _ -> Nothing
    Right h -> Just h

core :: Bool -> RunRocksDB -> String -> IO ()
core isPiped (RunRocksDB run) l' = case parseCommand $ BC.pack l' of
    Just (I k v) -> do
        r <- run $ do
            insert rocksDBCSMT k v
            generateInclusionProof rocksDBCSMT k
        case r of
            Just "" -> putStrLn "Empty proof for the first insertion"
            Just proof -> do
                if isPiped then pure () else printHash "proof" proof
            Nothing -> putStrLn "Tree is empty"
    Just (Q k) -> do
        r <- run $ generateInclusionProof rocksDBCSMT k
        case r of
            Just proof -> printHash "proof" proof
            Nothing -> putStrLn "No proof found"
    Just R -> do
        r <- run $ root rocksDBCSMT
        case r of
            Just rootHash -> printHash "root" rootHash
            Nothing -> putStrLn "Tree is empty"
    Just (V value proof) -> do
        case readHash proof of
            Just decoded -> do
                r <- run $ verifyInclusionProof rocksDBCSMT value decoded
                putStrLn $ if r then "Valid proof" else "Invalid proof"
            Nothing -> putStrLn "Invalid proof format"
    Nothing -> putStrLn help

main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    withRocksDB "testdb" $ \run -> do
        isPiped <- checkPipeline
        if isPiped
            then fix $ \loop -> do
                eof <- isEOF
                unless eof $ do
                    line <- B.getLine
                    core isPiped run (BC.unpack line)
                    loop
            else do
                putStrLn help
                runInputT defaultSettings $ fix $ \loop -> do
                    mlline <- getInputLine "\n> "
                    case mlline of
                        Nothing -> return ()
                        Just line -> do
                            lift $ core isPiped run line
                            loop

checkPipeline :: IO Bool
checkPipeline = not <$> hIsTerminalDevice stdin

help :: String
help =
    unlines
        [ "Commands:"
        , "  i <key> <value>   Change key-value pair and print inclusion proof"
        , "  q <key>           Query inclusion proof for key"
        , "  v <value>         Verify inclusion proof for the singleton csmt"
        , "  v <value> <proof> Verify inclusion proof for a value"
        , "  r                 Print root hash of the tree"
        ]
