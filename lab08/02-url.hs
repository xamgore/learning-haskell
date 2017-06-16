import Parser
import SimpleParsers
import ParseNumbers (natural)
import Control.Applicative hiding (many, optional)
import Control.Monad

{-
   Определите тип данных, представляющий адрес URL следующего вида:

     <схема>://(<логин>(:<пароль>)@)<хост>(:<порт>)(/<URL‐путь>)(?(<параметры>))(#(<якорь>))

   Реализуйте соответствующий парсер, считая, что обязательными компонентами
   URL являются только схема с символами "://" и имя хоста, все остальные
   компоненты могут отсутствовать.
-}


data URL      = URL Scheme Login Pass Server Port Path Params Anchor deriving Show
data Scheme   = FTP | HTTP | HTTPS | Unk String deriving Show
type Login    = String
type Pass     = String
type Server   = String
type Port     = Int
type Path     = String
type Params   = String
type Anchor   = String

scheme = (string "https" >> return HTTPS) <|>
         (string "http"  >> return HTTP)  <|>
         (string "ftp"   >> return FTP)   <|>
         Unk `liftM` lowers >>= tap (string "://")

server = many1 (sat (`notElem` ":/?#"))
port   = char ':' >> natural
path   = many1 $ sat (`notElem` "?#")
params = char '?' >> many1 (notChar '#')
anchor = char '#' >> many1 (sat $ const True)
login  = find '@' $ many1 (sat (`notElem` ":@")) >>= skip '@'
passwd = find '@' $ char ':' >> many1 (notChar '@') >>= skip '@'

find ch parse = lookForward ch >>= \ok -> if ok then parse else return ""
skip ch       = tap (optional ' ' $ char ch)


url = URL <$>
      scheme <*>
      login  <*>
      passwd <*>
      server <*>
      optional 0 port <*>
      optional "" path <*>
      optional "" params <*>
      optional "" anchor
