module CSMT.Frontend.CLI.App (main) where

import CSMT.Backend.RocksDB
    ( RunRocksDB (RunRocksDB)
    , rocksDBCSMT
    , withRocksDB
    )
import CSMT.Hashes
    ( delete
    , generateInclusionProof
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
import OptEnvConf
    ( Parser
    , argument
    , help
    , metavar
    , reader
    , runParser
    , setting
    , str
    )
import Paths_csmt (version)
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
    | D ByteString
    | Q ByteString
    | V ByteString ByteString
    | R

parseCommand :: ByteString -> Maybe Command
parseCommand line =
    case B.words line of
        ["i", k, v] -> Just (I k v)
        ["d", k] -> Just (D k)
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
        run $ insert rocksDBCSMT k v
        unless isPiped
            $ do
                r <- run $ generateInclusionProof rocksDBCSMT k
                case r of
                    Just "" -> putStrLn "Empty proof for the first insertion"
                    Just proof -> do
                        printHash "proof" proof
                    Nothing -> putStrLn "Tree is empty"
    Just (D k) -> do
        run $ delete rocksDBCSMT k
        unless isPiped
            $ putStrLn "Deleted key, exclusion proof generation not implemented"
    -- Deletion is not implemented in this example
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
    Nothing -> putStrLn helpInteractive

newtype Options = Options
    { optDbPath :: FilePath
    }

parseDbPath :: Parser FilePath
parseDbPath =
    setting
        [ argument
        , metavar "DB_PATH"
        , help "Path to RocksDB database"
        , reader str
        ]

optionsParser :: Parser Options
optionsParser =
    Options
        <$> parseDbPath

main :: IO ()
main = do
    Options{optDbPath} <- runParser version "csmt" optionsParser
    hSetBuffering stdout LineBuffering
    hSetBuffering stdin LineBuffering
    withRocksDB optDbPath $ \run -> do
        isPiped <- checkPipeline
        if isPiped
            then fix $ \loop -> do
                eof <- isEOF
                unless eof $ do
                    line <- B.getLine
                    core isPiped run (BC.unpack line)
                    loop
            else do
                putStrLn helpInteractive
                runInputT defaultSettings $ fix $ \loop -> do
                    mlline <- getInputLine "\n> "
                    case mlline of
                        Nothing -> return ()
                        Just line -> do
                            lift $ core isPiped run line
                            loop

checkPipeline :: IO Bool
checkPipeline = not <$> hIsTerminalDevice stdin

helpInteractive :: String
helpInteractive =
    unlines
        [ "Commands:"
        , "  i <key> <value>   Change key-value pair and print inclusion proof"
        , "  d <key>           Delete key and print exclusion proof (soon)"
        , "  q <key>           Query inclusion proof for key"
        , "  v <value>         Verify inclusion proof for the singleton csmt"
        , "  v <value> <proof> Verify inclusion proof for a value"
        , "  r                 Print root hash of the tree"
        ]
