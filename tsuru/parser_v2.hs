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
import           System.IO (withBinaryFile, IOMode(ReadMode))

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

type DecodeResult =
  Either (ByteOffset, String) [QuoteMsg]

--------------------------------------------------------------------------------

main :: IO ()
main = do
  args <- getArgs
  case args of
    (fp:[]) -> decodeOrFail fp >>= printQuoteMsgs
    (fp:"-r":[]) -> decodeOrFail fp >>= printReorderQuoteMsgs
    ("-r":fp:[]) -> decodeOrFail fp >>= printReorderQuoteMsgs
    _ -> putStrLn "Args expected: filename and optional reorder flag -r"

--------------------------------------------------------------------------------

-- Skip 24 bytes of global header, then run decoder incrementally
decodeOrFail :: FilePath -> IO DecodeResult
decodeOrFail f =
  withBinaryFile f ReadMode $ \h -> do
    feed (runGetIncremental (skip 24 *> getQuoteMsgs)) h
  where
    feed (Done _ _ x) _ = return (Right x)
    feed (Fail _ pos str) _ = return (Left (pos, str))
    feed (Partial k) h = do
      chunk <- B.hGet h BL.defaultChunkSize
      case B.length chunk of
        0 -> feed (k Nothing) h
        _ -> feed (k (Just chunk)) h

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

printQuoteMsgs :: DecodeResult -> IO ()
printQuoteMsgs result = case result of
  Left (offset, err) -> putStrLn $ err ++ " (offset " ++ show offset ++ ")"
  Right qs -> TIO.putStr . T.unlines . map showQuoteMsg $ qs

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

--------------------------------------------------------------------------------

printReorderQuoteMsgs :: DecodeResult -> IO ()
printReorderQuoteMsgs result = case result of
  Left (offset, err) -> putStrLn $ err ++ " (offset " ++ show offset ++ ")"
  Right [] -> putStrLn "No messages parsed"
  Right (q:qs) -> printReorder qs [q] []

printReorder :: [QuoteMsg] -> [QuoteMsg] -> [QuoteMsg] -> IO ()
printReorder qs buffer flush = do
  case flush of
    [] -> case qs of
            [] -> do TIO.putStr . T.unlines . map showQuoteMsg $ buffer
                     pure ()
            _ -> do let (qs', buffer', flush') = processMsg qs buffer
                    printReorder qs' buffer' flush'
    _ -> do TIO.putStr . T.unlines . map showQuoteMsg $ flush
            printReorder qs buffer []

window :: NominalDiffTime
window = daysAndTimeOfDayToTime 0 $ TimeOfDay 0 0 3

processMsg :: [QuoteMsg] -> [QuoteMsg]-> ([QuoteMsg], [QuoteMsg], [QuoteMsg])
processMsg quotes buffer = case quotes of
  (q:qs) -> (qs, buffer', flush)
    where
      (flush, buffer') =
        let ft = daysAndTimeOfDayToTime 0
            flushTime = ft (acceptTime q) - window
            (xs, ys) = span (\x -> ft (acceptTime x) <= flushTime) buffer
        in (xs, insertBy (compare `on` acceptTime) q ys)
  [] -> ([], [], buffer)
