import Control.Monad.Reader
import Control.Exception (bracket)
import Data.Hash.CRC32.Posix
import Data.List (isPrefixOf)
import Data.Time.Clock
import Data.Time.Calendar
import Data.Word (Word8)
import Network
import Numeric (showFFloat)
import System.Exit (exitSuccess)
import System.IO
import Text.Printf
 
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
