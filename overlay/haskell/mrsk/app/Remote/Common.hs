{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -Wno-simplifiable-class-constraints #-}

module Remote.Common where

import Cli (CliEffect)
import Control.Exception (Exception)
import Control.Monad.Extra (void, whileM)
import Control.Monad.IO.Class (liftIO)
import Data.Aeson qualified as A
import Data.Aeson.KeyMap (delete, singleton, union, (!?))
import Data.Aeson.KeyMap qualified as A
import Data.Aeson.Types (FromJSON, ToJSON, parseJSON, toJSON, (.:), (.=))
import Data.Aeson.Types qualified as A
import Data.ByteString.Char8 qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Kind (Type)
import Data.Text (Text)
import Data.Text qualified as T
import Effectful (Eff, IOE, inject, (:>))
import Effectful.Concurrent (Concurrent, forkIO)
import Effectful.Concurrent.Async (Async, async, link, race)
import Effectful.Concurrent.Chan (Chan, newChan, readChan)
import Effectful.Concurrent.MVar.Strict (MVar', putMVar')
import Effectful.Monad.Logger (Logger, logDebugN, logErrorN)
import Effectful.Prim (Prim)
import Effectful.Prim.IORef.Strict (newIORef', readIORef')
import Effectful.Process.Typed (Process, getStdin, getStdout)
import Effectful.State.Dynamic (State, evalStateShared, modify, state)
import GHC.Generics (Generic)
import Lens.Micro.Platform (at, use)
import RequireCallStack (RequireCallStack)
import System.IO (BufferMode (LineBuffering), Handle, hSetBuffering)
import System.IO qualified as IO

type Message' = Type -> Type

data SomeResponse = SomeResponse {id' :: Int, response :: A.Value}
  deriving stock (Generic)
  deriving anyclass (A.FromJSON, A.ToJSON)
data SomeMessage m = forall a. (ToJSON (m a)) => SomeMessage (m a)
data Message m = Message {id' :: Int, message :: SomeMessage m}

instance (FromJSON (SomeMessage m)) => FromJSON (Message m) where
  parseJSON = A.withObject "Message" \obj ->
    ( do
        message :: A.KeyMap A.Value <- obj .: "message"
        type_ :: Text <- obj .: "type"

        A.parseJSON (A.Object $ message `union` singleton "type" (A.String type_))
    )
      >>= \inner ->
        Message
          <$> obj .: "id"
          <*> pure inner

instance ToJSON (Message m) where
  toJSON Message{message, id'} =
    case toJSON message of
      A.Object inner ->
        A.object
          [ "id" .= id'
          , "type" .= inner !? "type"
          , "message" .= delete "type" inner
          ]
      A.String type_ ->
        A.object
          [ "id" .= id'
          , "type" .= type_
          , "message" .= A.object []
          ]
      value -> error ("unreachable: " <> show value)

instance ToJSON (SomeMessage m) where
  toJSON (SomeMessage msg) = toJSON msg

data RemoteException
  = ConnectionFailed {user :: Text, address :: Text, output :: BSL.ByteString}
  | CouldNotLocateMrskBinary {user :: Text, address :: Text}
  deriving (Show)
instance Exception RemoteException

data
  Protocol
    (inMessage :: Type -> Type)
    (outMessage :: Type -> Type)
  = Protocol {hIn :: Handle, hOut :: Handle}

newProtocol
  :: (IOE :> es, RequireCallStack)
  => Handle -> Handle -> Eff es (Protocol inMessage outMessage)
newProtocol hOut hIn = do
  liftIO $ hSetBuffering hIn LineBuffering
  liftIO $ hSetBuffering hOut LineBuffering
  pure $ Protocol{hIn, hOut}

class MShow a where
  mshow :: a -> String

protoRead
  :: (FromJSON (Message inMessage), IOE :> es, Logger :> es, RequireCallStack)
  => Protocol inMessage outMessage -> Eff es (Either (Message inMessage) SomeResponse)
protoRead Protocol{hIn} = do
  line <- liftIO (BS.hGetLine hIn)

  A.throwDecodeStrict line

protoRespond
  :: (A.ToJSON a, IOE :> es, Logger :> es, RequireCallStack) => Protocol inMessage outMessage -> Int -> a -> Eff es ()
protoRespond Protocol{hOut} id' response = do
  liftIO . BSL.hPut hOut . (<> "\n") $ json
 where
  json = A.encode . (Right @() @SomeResponse) $ SomeResponse{id', response = A.toJSON response}

protoWrite
  :: forall inMessage outMessage es
   . (IOE :> es, Logger :> es, RequireCallStack, ToJSON (Message outMessage))
  => Protocol inMessage outMessage -> Message outMessage -> Eff es ()
protoWrite Protocol{hOut} outData = do
  liftIO . BSL.hPut hOut . (<> "\n") $ json
 where
  json = A.encode $ (Left @(Message outMessage) @()) outData

protoWriteIO
  :: forall inMessage outMessage
   . (RequireCallStack, ToJSON (Message outMessage))
  => Protocol inMessage outMessage -> Message outMessage -> IO ()
protoWriteIO Protocol{hOut} outData = do
  BSL.hPut hOut . (<> "\n") $ json
 where
  json = A.encode $ (Left @(Message outMessage) @()) outData

acquireId :: (State Int :> es) => Eff es Int
acquireId = state \s -> (s, s + 1)

data Local (msg :: Message') where
  Local'Message
    :: (FromJSON a, ToJSON (msg a))
    => { result :: MVar' a
       , message :: msg a
       }
    -> Local msg

data RequestMap = forall a. RequestMap
  { response :: MVar' a
  , parseJSON' :: A.Value -> A.Parser a
  }

background
  :: forall (inMessage :: Type -> Type) (outMessage :: Type -> Type) es
   . ( A.FromJSON (SomeMessage inMessage)
     , Concurrent :> es
     , IOE :> es
     , Logger :> es
     , Prim :> es
     , RequireCallStack
     )
  => IO.Handle
  -> IO.Handle
  -> (Chan (Local outMessage) -> forall a. inMessage a -> Eff es A.Value)
  -> Eff es (Chan (Local outMessage), Async ())
background hIn hOut callback =
  evalStateShared (HM.empty :: HashMap Int RequestMap) . evalStateShared (0 :: Int) $ do
    localChannel <- newChan

    protocol :: Protocol inMessage outMessage <- newProtocol hIn hOut

    continue <- newIORef' True

    a <-
      async
        . whileM
        $ readIORef' continue
          <* ( race (readChan localChannel) (protoRead protocol) >>= \case
                 Left Local'Message{result, message = message :: msg a} -> do
                   id' <- acquireId

                   modify (HM.insert id' $ RequestMap{response = result, parseJSON' = parseJSON @a})

                   protoWrite protocol Message{id', message = SomeMessage message}
                 Right (Left (Message{id', message = SomeMessage message})) ->
                   void . forkIO $ inject (callback localChannel message) >>= protoRespond protocol id'
                 Right (Right (SomeResponse{id', response = netResponse})) -> do
                   use @(HashMap Int RequestMap) (at id') >>= \case
                     Just (RequestMap{response, parseJSON'}) ->
                       case A.parseEither parseJSON' netResponse of
                         Right responseValue -> putMVar' response responseValue
                         Left err ->
                           logErrorN $
                             "failed to decode response { id = " <> T.show id' <> ", error = " <> T.show err <> " }"
                     Nothing ->
                       logErrorN $ "response with invalid id " <> T.show id'
             )
    link a

    pure (localChannel, a)
