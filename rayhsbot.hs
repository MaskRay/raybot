import Control.Concurrent
import Control.Monad.Reader
import Control.Monad.State
import Control.Exception (bracket)
import Data.Hash.CRC32.Posix
import Data.List (isPrefixOf)
import Data.Time
import Data.Word (Word8)
import Network

import Plugin.Pl.Common
import Plugin.Pl.Optimize
import Plugin.Pl.Parser
import Plugin.Pl.Transform
import Lambdabot.Pointful (pointful)

import System.Exit (exitSuccess)
import System.IO
import System.Locale (defaultTimeLocale)
import Text.Printf
import Text.XML.HXT.Core hiding (utf8)
import Text.XML.HXT.Curl

server   = "irc.freenode.net"
--server   = "localhost"
port     = PortNumber 6667
mynick   = "raybot"
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
  hSetBuffering h LineBuffering
  hSetEncoding h utf8
  return $ Bot h

run :: Net ()
run = do
  write "NICK" mynick
  write "USER" (mynick++" 0 * :MaskRay's bot")
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
  ["PRIVMSG", chan] | "jrrp" `isPrefixOf` msg -> do
    julianDay <- lift getDate
    let s = show julianDay++nick
        value = fromIntegral (crc32 s) :: Word8
        score = fromIntegral value/256*100::Double
        barLen = round (score/5)
    action chan $ printf "%s今日的人品指数：[%s%s] %.3f%% (Lv %d/%d)" nick (replicate barLen '>') (replicate (20-barLen) '.') score barLen (20::Int)
                    | ".pl " `isPrefixOf` msg ->
                        privmsg (if "#" `isPrefixOf` chan then chan else nick) $ pl (drop 4 msg) False
                    | ".pf " `isPrefixOf` msg ->
                        privmsg (if "#" `isPrefixOf` chan then chan else nick) $ pointful (drop 4 msg)
                    | nick == myowner && chan == mynick ->
                        writeRaw msg
                    | nick == myowner && ".quit" `isPrefixOf` msg ->
                      write "QUIT" ":Bye" >> lift exitSuccess
  ["INVITE", _] -> write "JOIN" msg
  _ -> return ()
  where
    getDate = getCurrentTime >>= return . toModifiedJulianDay . utctDay :: IO Integer

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

getFeed :: Bot -> IO ()
getFeed bot = 
  evalStateT (forever $ do
                 result <- lift $ runX $ processRss "http://www.linuxsir.org/bbs/external.php?type=RSS2"
                 lastCheck <- get
                 let timestamp = getTime $ result !! 2
                 if length result >= 3 && timestamp > lastCheck
                   then put timestamp >> liftIO (runReaderT (action mychan $ printf "论坛新帖：%s %s\n" (result!!0) (result!!1)) bot)
                   else return ()
                 lift $ threadDelay 300000000
             ) $ UTCTime {utctDay = ModifiedJulianDay 0, utctDayTime = 0}
  where
    getTime str = readTime defaultTimeLocale "%a, %d %b %Y %T %Z" str :: UTCTime
    

pl :: String -> Bool -> String
pl input verbose = case parsePF input of
  Right d -> show $ last $ mapTopLevel' optimize $ mapTopLevel transform d
  Left err -> err
