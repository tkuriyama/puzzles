{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as BL
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format
import           System.Environment (getArgs)

--------------------------------------------------------------------------------

data QuoteMsg = QuoteMsg
  { packetTime :: UTCTime
  , acceptTime :: TimeOfDay
  , isinCode :: B.ByteString
  , bidQuotes ::  [Quote]
  , askQuotes :: [Quote]
  }

data Quote =
  Quote QuotePx QuoteQty

type QuotePx = Int
type QuoteQty = Int

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case parseArgs args of
    Just (fp, runParse) -> runParse fp >>= printQuoteMsgs
    Nothing -> putStrLn "Args expected: filename and optional reoder flag -r"

parseArgs :: [String] -> Maybe (FilePath, FilePath -> IO [QuoteMsg])
parseArgs  args = case args of
  (fp:[]) -> Just (fp, parseQuoteMsgs)
  (fp:"-r":[]) -> Just (fp, fmap reorder . parseQuoteMsgs)
  ("-r":fp:[]) -> Just (fp, fmap reorder . parseQuoteMsgs)
  _ -> Nothing

--------------------------------------------------------------------------------

parseQuoteMsgs :: FilePath -> IO [QuoteMsg]
parseQuoteMsgs fp = do
  input <- BL.readFile fp
  pure $ runGet getPcap input

getPcap :: Get [QuoteMsg]
getPcap = skip 24 *> getQuoteMsgs

getQuoteMsgs :: Get [QuoteMsg]
getQuoteMsgs = do
  empty <- isEmpty
  if empty
    then pure []
    else do mQuote <- getQuoteMsg
            quotes <- getQuoteMsgs
            case mQuote of
              Just quote -> pure $ quote:quotes
              Nothing -> pure quotes

getQuoteMsg :: Get (Maybe QuoteMsg)
getQuoteMsg = do
  (pktTime, len) <- getPacketHeader
  if len < 47
    then do _ <- skip len
            pure $ Nothing
    else do _ <- skip 42
            ids <- getByteString 5
            if ids /= B.pack [66, 54, 48, 51, 52]
              then do _ <- skip (len - 47)
                      pure Nothing
              else do (accTime, isin, bids, asks) <- getPacketData
                      pure $ Just $ QuoteMsg pktTime accTime isin bids asks

getPacketHeader :: Get (UTCTime, Int)
getPacketHeader = do
  pktTime <- getPktTime
  _ <- skip 4
  len <- getInt32le
  pure (pktTime, fromIntegral len)

getPktTime :: Get UTCTime
getPktTime = do
  secs <- getInt32le
  usecs <- getInt32le
  let pktTime = posixSecondsToUTCTime $
                fromIntegral secs + (fromIntegral usecs / 1000000)
  pure pktTime

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
  [h, m, s] <- sequence [getInt 2, getInt 2, getInt 4]
  pure $ TimeOfDay h m (fromIntegral s / 100)

getQuote :: Get Quote
getQuote = Quote <$> getInt 5 <*> getInt 7


getInt :: Int -> Get Int
getInt n = do
  i <- getByteString n
  pure $ read . BC.unpack $ i

--------------------------------------------------------------------------------

reorder :: [QuoteMsg] -> [QuoteMsg]
reorder = id

--------------------------------------------------------------------------------

printQuoteMsgs :: [QuoteMsg] -> IO ()
printQuoteMsgs = TIO.putStrLn . T.unlines . map showQuoteMsg

showQuoteMsg :: QuoteMsg -> T.Text
showQuoteMsg q =
  pktTime <> sp <> accTime <> sp <> isin <> sp <> bids <> sp <> asks
  where
    sp = " "
    pktTime = showUTCTime $ packetTime q
    accTime = packs $ acceptTime q
    isin = packs $ isinCode q
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
