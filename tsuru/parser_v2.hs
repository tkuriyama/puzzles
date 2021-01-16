{-# LANGUAGE OverloadedStrings #-}

import           Control.Monad
import           Data.Binary.Get
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Internal as BL (defaultChunkSize)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           Data.Time.Clock (UTCTime)
import           Data.Time.LocalTime
import           Data.Time.Clock.POSIX (posixSecondsToUTCTime)
import           Data.Time.Format
import           System.Environment (getArgs)
import           System.IO ( withBinaryFile, IOMode(ReadMode))

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
  Right qs -> TIO.putStrLn . T.unlines . map showQuoteMsg $ qs

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

data Deque a = Deque [a] [a]

toList :: Deque a -> [a]
toList (Deque l r) = l ++ reverse r

-- split Deque by performing takeWhile from the left
splitWhile :: (a -> Bool) -> Deque a -> ([a], Deque a)
splitWhile p (Deque l r) = case l of
  [] -> let (xs, ys) = splitList p r
        in (xs, Deque [] ys)
  _ -> let (xs, ys) = splitList p l
       in case ys of
            [] -> let (xs', ys') = splitList p (reverse r)
                  in (xs ++ xs', Deque [] ys')
            _ -> (xs, Deque ys r)

splitList :: (a -> Bool) -> [a] -> ([a], [a])
splitList p xs = (takeWhile p xs, dropWhile p xs)

-- insert element into Deque from the right, stopping when predicate is false
insertRight :: (a -> Bool) -> a -> Deque a -> Deque a
insertRight p a (Deque l r) = case foldl fL (False, []) r of
  (True, r') -> Deque l (reverse r')
  (False, _) -> Deque (snd $ foldr fR (False, []) l) r
  where
    fL (done, acc) x = if p x then (True, x:p:acc) else (done, x:acc)
    fR x (done, acc) = if (not p) x then (True, p:x:acc) else (done, x:acc)


--------------------------------------------------------------------------------

printReorderQuoteMsgs :: DecodeResult -> IO ()
printReorderQuoteMsgs result = case result of
  Left (offset, err) -> putStrLn $ err ++ " (offset " ++ show offset ++ ")"
  Right [] -> putStrLn "No messages parsed"
  Right (q:qs) -> printReorder qs (Deque [] [q]) []

printReorder :: [QuoteMsg] -> Deque QuoteMsg -> [QuoteMsg] -> IO ()
printReorder qs buffer flush = do
  case flush of
    [] -> case qs of
            [] -> pure ()
            _ -> do let (qs', buffer', flush') = processMsg qs buffer []
                    printReorder qs' buffer' flush'
    _ -> do TIO.putStr . T.unlines . map showQuoteMsg $ flush
            printReorder qs buffer []

processMsg :: [QuoteMsg] -> Deque QuoteMsg -> [QuoteMsg] ->
              ([QuoteMsg], Deque QuoteMsg, [QuoteMsg])
processMsg qs buffer _ = case qs of
  [] -> ([], buffer, toList buffer)
  (q:qs) -> (qs, buffer', flush)
    where
      flushTime =
        let t = acceptTime q
            (h, m, s) = (todHour t, todMin t, todSec t)
        in TimeOfDay h m (s - 3)
      (flush, buffer') =
        let (xs, deque) = splitWhile (\m -> acceptTime m <= flushTime) buffer
        in (xs, insertRight (\m -> acceptTime m < acceptTime q) q deque)
