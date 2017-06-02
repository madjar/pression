{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TypeFamilies #-}
module Main where

import Pression.Library

import Graphics.QML
import Data.Text (Text)
import qualified Data.Text as T
import Data.IORef
import Data.Typeable
import Data.Proxy

import Control.Monad.Trans.Reader
import Control.Monad.Trans.Writer
import Control.Monad.IO.Class

import Paths_pression

-------------------

-- | A monad in which you defined class members (methods and properties)
newtype MkClass a = MkClass
    { extractMkClass :: WriterT [Member ()] IO a
    } deriving (Functor,Applicative,Monad,MonadIO)

-- | The body of a method : ObjRef () -> IO a
newtype MethodBody a = MethodBody
    { extractMethodBody :: ReaderT (ObjRef ()) IO a
    } deriving (Functor,Applicative,Monad,MonadIO)

-- | Auxilliary type that is used within MkClass to represent properties
data Property a = Property (IORef a) (SignalKey (IO ()))

-- TODO not hardcode for Member ()
mkClass :: MkClass a -> IO [Member ()]
mkClass = execWriterT . extractMkClass

runBody = runReaderT . extractMethodBody

-- | Create a new property with a given name and default value. This also takes
-- function to translate the property to something that Qt can read
property :: (Marshal tr, CanReturnTo tr ~ Yes) => String -> a -> (a -> tr) -> MkClass (Property a)
property name value tr = do
  ref <- liftIO (newIORef value)
  sig <- liftIO newSignalKey
  MkClass . tell $ [defPropertySigRO' name sig (\_ -> tr <$> readIORef ref)]
  return (Property ref sig)

-- | Defined a method
define :: (Marshal a, CanReturnTo a ~ Yes) => String -> MethodBody a -> MkClass ()
define name body = MkClass . tell $ [defMethod' name (runBody body)]

-- | Inside a method, get the value of a property
get :: Property a -> MethodBody a
get (Property ref _)= MethodBody . ReaderT $ \_ -> readIORef ref

-- | Inside a method, changes the value of a property. This fires the attached signal.
set :: Property a -> a -> MethodBody ()
set (Property ref sig) value =
    MethodBody . ReaderT $
    \obj -> do
        writeIORef ref value
        fireSignal sig obj

-- | Directly returns an usable context object from a MkClass
mkObject :: MkClass () -> IO (ObjRef ())
mkObject mk = do members <- mkClass mk
                 clazz <- newClass members
                 newObject clazz ()


------------

main :: IO ()
main = do
    -- game <- newIORef =<< randomGame
    -- skey <- newSignalKey :: IO (SignalKey (IO ()))
    -- clazz <-
    --     newClass
    --         [ defMethod'
    --               "changeGame"
    --               (\obj -> do
    --                    writeIORef game =<< randomGame
    --                    fireSignal skey obj)
    --         , defMethod'
    --               "launch"
    --               (\_ ->
    --                     (runGame =<< readIORef game))
    --         , defPropertySigRO'
    --               "game"
    --               skey
    --               (\_ ->
    --                     T.pack . show <$> readIORef game)]
    --ctx <- newObject clazz ()

    newGame <- liftIO randomGame
    ctx <- mkObject $ do
      game <- property "game" newGame (T.pack . show)
      define "changeGame" (set game =<< liftIO randomGame)
      define "launch" (liftIO . runGame =<< get game)

    doc <- getDataFileName "main.qml"
    runEngineLoop
        defaultEngineConfig
        { initialDocument = fileDocument doc
        , contextObject = Just $ anyObjRef ctx
        }
