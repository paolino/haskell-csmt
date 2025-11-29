module CSMT.Frontend.CLI.App (main) where

import CSMT.Backend.RocksDB
    ( RunRocksDB (RunRocksDB)
    , withRocksDB
    )
import CSMT.Backend.RocksDB qualified as RocksDB
import CSMT.Hashes
    ( Hash
    , byteStringToKey
    , delete
    , generateInclusionProof
    , insert
    , mkHash
    , queryKV
    , renderHash
    , root
    , verifyInclusionProof
    )
import CSMT.Interface
    ( Backend (queryCSMT)
    , Indirect (..)
    , Key
    )
import CSMT.Interface qualified as I
import Control.Monad (unless)
import Control.Monad.Fix (fix)
import Control.Monad.Trans.Class (MonadTrans (..))
import Data.ByteArray.Encoding
    ( Base (Base64)
    , convertFromBase
    , convertToBase
    )
import Data.ByteString (ByteString)
import Data.ByteString.Char8 qualified as BC
import OptEnvConf
    ( Parser
    , argument
    , env
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
    = -- | Insert key value
      I ByteString ByteString
    | -- | Delete key
      D ByteString
    | -- | Query inclusion proof for key
      Q ByteString
    | -- | Query hash at partial key
      QB (Maybe Key)
    | -- | Verify inclusion proof for value and proof
      V ByteString ByteString
    | -- | Query key-value pair
      W ByteString
    | -- | Query root hash
      R
    | -- | Comment
      C
    | -- | Show key directions
      K ByteString

data Error
    = EmptyProof
    | TreeEmpty
    | InvalidProofFormat
    | InvalidKeyFormat
    | NoProofFound
    | NoNodeFound
    | KeyNotFound
    | DeletedKey
    | AddedKey
    | Valid
    | Invalid
    | UnknownCommand
    | Comment
    deriving (Show)

renderError :: Error -> String
renderError EmptyProof = "Empty proof for the first insertion"
renderError TreeEmpty = "Tree is empty"
renderError InvalidProofFormat = "Invalid proof format"
renderError InvalidKeyFormat = "Invalid key format"
renderError NoProofFound = "No proof found"
renderError NoNodeFound = "No node found at the given key"
renderError KeyNotFound = "Key not found"
renderError DeletedKey = "Deleted key, exclusion proof generation not implemented"
renderError AddedKey = "Added key, inclusion proof generated"
renderError Valid = "Valid proof"
renderError Invalid = "Invalid proof"
renderError UnknownCommand = helpInteractive
renderError Comment = ""

parseCommand :: ByteString -> Maybe Command
parseCommand line =
    case BC.words line of
        ["i", k, v] -> Just (I k v)
        ["d", k] -> Just (D k)
        ["q", k] -> Just (Q k)
        ["p"] -> Just (QB $ Just [])
        ["p", ks] -> Just (QB $ parseLRKey ks)
        ["w", k] -> Just (W k)
        ["v", value] -> Just (V value "")
        ["v", value, proof] -> Just (V value proof)
        ["r"] -> Just R
        ["k", key] -> Just (K key)
        "#" : _comment -> Just C
        _ -> Nothing

parseLRKey :: ByteString -> Maybe Key
parseLRKey = traverse step . BC.unpack
  where
    step 'L' = Just I.L
    step 'R' = Just I.R
    step _ = Nothing

type Prompt = Maybe ByteString

mkPrompt :: Bool -> String -> Prompt
mkPrompt isPiped cmd = if isPiped then Nothing else Just (BC.pack cmd)

printHash :: Prompt -> ByteString -> IO ()
printHash (Just prompt) what = BC.putStrLn . ((prompt <> ": ") <>) . convertToBase Base64 $ what
printHash Nothing what = BC.putStrLn . convertToBase Base64 $ what

readHash :: ByteString -> Maybe ByteString
readHash bs = case convertFromBase Base64 bs of
    Left _ -> Nothing
    Right h -> Just h

rocksDBBackend :: Backend RocksDB.RocksDB ByteString ByteString Hash
rocksDBBackend = RocksDB.rocksDBBackend mkHash

data Output
    = Binary String ByteString
    | Text ByteString
    | Node (Indirect Hash)
    | ErrorMsg Error

core :: Bool -> RunRocksDB -> String -> IO ()
core isPiped (RunRocksDB run) l' = do
    r <- case parseCommand $ BC.pack l' of
        Just (I k v) -> do
            run $ insert rocksDBBackend k v
            pure $ ErrorMsg AddedKey
        Just (D k) -> do
            run $ delete rocksDBBackend k
            pure $ ErrorMsg DeletedKey
        Just (Q k) -> do
            r <- run $ generateInclusionProof rocksDBBackend k
            pure $ case r of
                Just proof -> Binary "proof" proof
                Nothing -> ErrorMsg NoProofFound
        Just (QB mk) -> do
            case mk of
                Nothing -> pure $ ErrorMsg InvalidKeyFormat
                Just k -> do
                    mv <- run $ queryCSMT rocksDBBackend k
                    pure $ case mv of
                        Just v -> Node v
                        Nothing -> ErrorMsg NoNodeFound
        Just R -> do
            r <- run $ root rocksDBBackend
            pure $ case r of
                Just rootHash -> Binary "root" rootHash
                Nothing -> ErrorMsg TreeEmpty
        Just (V value proof) -> do
            case readHash proof of
                Just decoded -> do
                    r <- run $ verifyInclusionProof rocksDBBackend value decoded
                    pure $ ErrorMsg $ if r then Valid else Invalid
                Nothing -> pure $ ErrorMsg InvalidProofFormat
        Just (W k) -> do
            mv <- run $ queryKV rocksDBBackend k
            pure $ case mv of
                Just v -> Text v
                Nothing -> ErrorMsg KeyNotFound
        Just C -> pure $ ErrorMsg Comment
        Just (K k) -> pure $ Text $ BC.pack $ byteStringToKey k >>= show
        Nothing -> pure $ ErrorMsg UnknownCommand
    case r of
        Binary prompt hash -> reportBinary prompt hash
        Text txt -> BC.putStrLn txt
        ErrorMsg e -> reportError' e
        Node node -> do
            BC.putStrLn $ renderKey $ jump node
            reportBinary "value" (renderHash $ value node)
  where
    reportBinary prompt = printHash (mkPrompt isPiped prompt)
    reportError' e
        | isPiped = print e
        | otherwise = putStrLn $ renderError e

renderKey :: Key -> ByteString
renderKey = BC.pack . fmap dirToByte
  where
    dirToByte I.L = 'L'
    dirToByte I.R = 'R'
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
        , env "CSMT_DB_PATH"
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
                    line <- BC.getLine
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
        , "  w <key>           Query value for key in bytestring"
        , "  d <key>           Delete key and print exclusion proof (soon)"
        , "  q <key>           Query inclusion proof for key in bytestring"
        , "  p <key>           Query node at partial key in directionsformat LRLRLL..."
        , "  v <value>         Verify inclusion proof for the singleton csmt"
        , "  v <value> <proof> Verify inclusion proof for a value"
        , "  r                 Print root hash of the tree"
        , "  k <key>           Show key directions"
        , "  # <comment>       Add comment line (no operation)"
        ]
