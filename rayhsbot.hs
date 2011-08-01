import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception (bracket)
import Data.Hash.CRC32.Posix
import Data.List (isPrefixOf)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time
import Data.Word (Word8)
import Network
import Numeric (showFFloat)
import System.Exit (exitSuccess)
import System.IO
import System.Locale
import Text.Printf
import Text.XML.HXT.Core hiding (utf8)
import Text.XML.HXT.Curl

server   = "irc.freenode.net"
port     = PortNumber 6667
mynick   = "rayhsbot"
mychan   = "#ubuntu-cn"
myowner  = "MaskRay"

data Bot = Bot { socket :: Handle }
type Net = ReaderT Bot IO

writeRaw :: String -> Net ()
writeRaw msg = do
  h <- asks socket
  lift $ hPutStrLn h (msg ++ "\r")
           
write :: String -> String -> Net ()
write s t = do
  h <- asks socket
  lift $ hPrintf h "%s %s\r\n" s t
  lift $ printf "> %s %s\r\n" s t

privmsg :: String -> String -> Net ()
privmsg chan msg = write "PRIVMSG" (chan ++ " :" ++ msg)

action :: String -> String -> Net ()
action chan msg = write "PRIVMSG" (chan ++ " :\SOHACTION " ++ msg ++ "\SOH")
  
main = bracket connect disconnect loop
  where
    disconnect = hClose . socket
    loop bot = catch (runReaderT run bot) (const $ return ())
  
connect :: IO Bot
connect = do
  h <- connectTo server port
  hSetBuffering h NoBuffering
  hSetEncoding h utf8
  return $ Bot h

run :: Net ()
run = do
  write "NICK" mynick
  write "USER" (mynick++" 0 * :tutorial bot")
  write "JOIN" mychan
  bot <- ask
  lift $ forkIO $ getFeed bot
  listen

listen :: Net ()
listen = forever $ do
  h <- asks socket
  s <- init `fmap` liftIO (hGetLine h)
  lift $ putStrLn s
  if "PING :" `isPrefixOf` s
    then pong s
    else eval (parse s)
  where
    pong x = write "PONG" (':':drop 6 x)
    parse x = (takeWhile (/='!') $ tail x,
               words $ takeWhile (/=':') $ tail $ dropWhile (/=' ') $ tail x,
               drop 1 $ dropWhile (/= ':') $ tail x)

eval :: (String, [String], String) -> Net ()
eval (nick, act, msg) = case act of
  ["PRIVMSG", chan] | "jrrp" `isPrefixOf` msg
                      -> do
                        (year,month,day) <- lift getDate
                        let s = show year++show month++show day++nick
                            value = fromIntegral (crc32 s) :: Word8
                        action chan $ printf "%s今日的人品指数：[>>>>>>>>>>>>>>>>>>>>>>>>]%.3f%%" nick (fromIntegral value/256*100::Double)
                    | nick == myowner && chan == mynick ->
                        writeRaw msg
                    | nick == myowner && ".quit" `isPrefixOf` msg -> 
                      write "QUIT" ":Bye" >> lift exitSuccess
  _ -> return ()

getDate :: IO (Integer, Int, Int) -- :: (year,month,day)
getDate = getCurrentTime >>= return . toGregorian . utctDay

processRss filename =
    readDocument [withValidate no, withCurl []] filename >>>
    getChildren >>>
    isElem >>> hasName "rss" >>>
    getChildren >>>
    isElem >>> hasName "channel" >>>
    getChildren >>>
    isElem >>> hasName "item" >>>
    getChildren >>>
    isElem >>> hasName "title" <+> hasName "link" <+> hasName "pubDate" >>>
    getChildren >>> getText

getTime :: String -> UTCTime
getTime = readTime defaultTimeLocale "%a, %d %b %Y %T %Z"

getFeed :: Bot -> IO ()
getFeed bot = 
  evalStateT (forever $ do
                 result <- lift $ runX $ processRss "http://www.linuxsir.org/bbs/external.php?type=RSS2"
--                 result <- lift $ runX $ processRss "/tmp/gentoo.xml"
                 lastCheck <- get
                 let timestamp = getTime $ result !! 2
                 if length result >= 3 && timestamp > lastCheck
                   then put timestamp >> liftIO (runReaderT (action mychan $ printf "论坛新帖：%s，链接：%s\n" (result!!0) (result!!1)) bot)
                   else return ()
                 lift $ threadDelay 300000000
             ) $ UTCTime {utctDay = ModifiedJulianDay 0, utctDayTime = 0}