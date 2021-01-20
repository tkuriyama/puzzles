{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Internal as BL (defaultChunkSize)
import           Data.List (insertBy)
import           Data.Function (on)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (UTCTime, NominalDiffTime)
import           Data.Time.LocalTime
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format
import           System.Environment (getArgs)
import           System.IO (withBinaryFile, IOMode(ReadMode), Handle)

--------------------------------------------------------------------------------

data QuoteMsg = QuoteMsg
  { packetTime :: UTCTime
  , acceptTime :: TimeOfDay
  , isinCode :: B.ByteString
  , bidQuotes :: [Quote]
  , askQuotes :: [Quote]
  }

data Quote =
  Quote QuotePx QuoteQty

type QuotePx = Int
type QuoteQty = Int

type QuoteBuffer = [QuoteMsg]

--------------------------------------------------------------------------------

main :: IO ()
main = getArgs >>= \case
    (fp:[]) -> go fp processFromHandle
    (fp:"-r":[]) -> go fp processFromHandleReorder
    ("-r":fp:[]) -> go fp processFromHandleReorder
    _ -> putStrLn "Args expected: filename and optional reorder flag -r"
    where go fp = withBinaryFile fp ReadMode

--------------------------------------------------------------------------------

processFromHandle :: Handle -> IO ()
processFromHandle h = feed (runGetIncremental (skip 24)) h >>= \case
  Left _ -> putStrLn "Global header parsing failed"
  Right ((), bs) -> void $ loop bs
  where
    loop :: B.ByteString -> IO ()
    loop bs =
      feedWithInitialChunk (runGetIncremental getQuoteMsg) h bs >>= \case
      Left _ ->
        putStrLn "processing terminated"
      Right (mQuoteMsg, bs') -> case mQuoteMsg of
        Nothing -> loop bs'
        Just quoteMsg -> (TIO.putStrLn $ showQuoteMsg quoteMsg) >> loop bs'

processFromHandleReorder :: Handle -> IO ()
processFromHandleReorder h = feed (runGetIncremental (skip 24)) h >>= \case
  Left _ -> putStrLn "Global header parsing failed"
  Right ((), bs) -> void $ loop bs []
  where
    loop :: B.ByteString -> QuoteBuffer -> IO QuoteBuffer
    loop bs buffer =
      feedWithInitialChunk (runGetIncremental getQuoteMsg) h bs >>= \case
      Left _ -> do
        TIO.putStr . T.unlines . map showQuoteMsg $ buffer
        putStrLn "processing terminated"
        pure []
      Right (mQuoteMsg, bs') -> case mQuoteMsg of
        Nothing -> loop bs' buffer
        Just quoteMsg -> updateBuffer quoteMsg buffer >>= loop bs'

updateBuffer :: QuoteMsg -> QuoteBuffer -> IO QuoteBuffer
updateBuffer q buffer = do
  let ft = daysAndTimeOfDayToTime 0
      flushTime = ft (acceptTime q) - window
      (xs, ys) = span (\x -> ft (acceptTime x) <= flushTime) buffer
      buffer' = insertBy (compare `on` acceptTime) q ys
  case xs of
    [] -> pure buffer'
    flush -> do TIO.putStr . T.unlines . map showQuoteMsg $ flush
                pure buffer'

-- acceptTime window after which messages are flushed for processing
window :: NominalDiffTime
window = daysAndTimeOfDayToTime 0 $ TimeOfDay 0 0 3

--------------------------------------------------------------------------------

feedWithInitialChunk :: Decoder a -> Handle -> B.ByteString ->
                        IO (Either (ByteOffset, String) (a, B.ByteString))
feedWithInitialChunk d h bs = case d of

  (Done bs' _ a) -> pure $ Right (a, bs `mappend` bs') -- verify
  (Fail _ pos str) -> pure $ Left (pos, str)
  (Partial k) -> case B.length bs of
    0 -> feed (k Nothing) h
    _ -> feed (k (Just bs)) h

feed :: Decoder a -> Handle ->
        IO (Either (ByteOffset, String) (a, B.ByteString))
feed (Done bs _ a) _ = pure $ Right (a, bs)
feed (Fail _ pos str) _ = pure $ Left (pos, str)
feed (Partial k) h = do
  chunk <- B.hGet h BL.defaultChunkSize
  case B.length chunk of
    0 -> feed (k Nothing) h
    _ -> feed (k (Just chunk)) h

--------------------------------------------------------------------------------

-- filter by value on dstPort and first five bytes of data
getQuoteMsg :: Get (Maybe QuoteMsg)
getQuoteMsg = do
  (pktTime, len) <- getPacketHeader
  _ <- skip 36
  dstPort <- getInt16be
  if dstPort == 15515 || dstPort == 15516
    then do _ <- skip 4
            ids <- getByteString 5
            if ids /= B.pack [66, 54, 48, 51, 52]
              then do _ <- skip (len - 47)
                      pure Nothing
              else do (accTime, isin, bids, asks) <- getPacketData
                      pure $ Just $ QuoteMsg pktTime accTime isin bids asks
    else do _ <- skip (len - 38)
            pure $ Nothing

getPacketHeader :: Get (UTCTime, Int)
getPacketHeader = do
  s <- getInt32le
  us <- getInt32le
  _ <- skip 4
  len <- getInt32le
  let pktTime = posixSecondsToUTCTime $
                fromIntegral s + (fromIntegral us / 1000000)
  pure (pktTime, fromIntegral len)

getPacketData :: Get (TimeOfDay, B.ByteString, [Quote], [Quote])
getPacketData = do
  isin <- getByteString 12
  _ <- skip 12
  bids <- replicateM 5 getQuote
  _ <- skip 7
  asks <- replicateM 5 getQuote
  _ <- skip 50
  accTime <- getAccTime
  _ <- skip 1
  pure (accTime, isin, bids, asks)

getAccTime :: Get TimeOfDay
getAccTime = do
  [h, m, s] <- sequence [getIntBytes 2, getIntBytes 2, getIntBytes 4]
  pure $ TimeOfDay h m (fromIntegral s / 100)

getQuote :: Get Quote
getQuote = Quote <$> getIntBytes 5 <*> getIntBytes 7

getIntBytes :: Int -> Get Int
getIntBytes n = read . BC.unpack <$> getByteString n

--------------------------------------------------------------------------------

showQuoteMsg :: QuoteMsg -> T.Text
showQuoteMsg q =
  pktTime <> sp <> accTime <> sp <> isin <> sp <> bids <> sp <> asks
  where
    sp = " "
    pktTime = showUTCTime $ packetTime q
    accTime = T.pack . show $ acceptTime q
    isin = T.pack . BC.unpack $ isinCode q
    bids = showQuotes $ reverse $ bidQuotes q
    asks = showQuotes $ askQuotes q

-- Show UTC time with microsecond granularity
showUTCTime :: UTCTime -> T.Text
showUTCTime t =
  T.take 15 $ T.pack $ formatTime defaultTimeLocale "%H:%M:%S.%q" t

showQuotes :: [Quote] -> T.Text
showQuotes = foldl f ""
  where f acc (Quote px qty) = acc <> packs qty <> "@" <> packs px <> " "

packs :: Show a => a -> T.Text
packs = T.pack . show
