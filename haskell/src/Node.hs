module Node (startNode) where

-- import Prelude hiding (getLine)

import Control.Monad.Reader
import GHC.IO.Handle (hFlush)
import System.IO (stderr)
import Message (InitRequest, Message)
import Data.Aeson (eitherDecode)
-- import Data.ByteString as BS
import Data.ByteString.Lazy.Char8 as C8
-- import Data.ByteString.Lazy as LBS

data NodeEnv = NodeEnv

newtype Node a = Node {runNode :: ReaderT NodeEnv IO a}
  deriving newtype (Functor, Applicative, Monad, MonadReader NodeEnv, MonadIO)

startNode :: IO ()
startNode = runReaderT (runNode mainLoop) NodeEnv

mainLoop :: Node ()
mainLoop = liftIO $ do
  writeMsg "Staring node"
  forever $ do
    msg <- readMsg
    case parseInit msg of
        Left _err -> writeMsg "Parse err" -- <> LBS.pack  err
        Right (m :: Message  InitRequest) -> writeMsg $ "Parse ok: " <> show m
  where
    readMsg = C8.pack <$> getLine
    writeMsg msg = C8.hPutStrLn stderr (C8.pack msg) >> hFlush stderr

    -- parseInit :: Text -> Either String (Message InitRequest)
    parseInit = eitherDecode
