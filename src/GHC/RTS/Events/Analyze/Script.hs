{-# OPTIONS_GHC -w -W #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE CPP #-}
module GHC.RTS.Events.Analyze.Script (
    -- * Types
    Script
  , Title
  , EventFilter(..)
  , EventSort(..)
  , Command(..)
    -- * Script execution
  , matchesFilter
    -- * Parsing and unparsing
  , pScript
  , unparseScript
    -- * Quasi-quoting support
  , scriptQQ
  ) where

import Control.Applicative ((<$>), (<*>), (*>), (<*), pure)
import Data.List (intercalate)
#if !MIN_VERSION_template_haskell(2,10,0)
import Data.Word (Word32)
#endif
import Language.Haskell.TH.Lift (deriveLiftMany)
import Language.Haskell.TH.Quote
import Language.Haskell.TH.Syntax
import Text.Parsec
import Text.Parsec.Language (haskellDef)
import qualified Text.Parsec.Token as P

import GHC.RTS.Events.Analyze.Types

{-------------------------------------------------------------------------------
  Script definition
-------------------------------------------------------------------------------}

-- | A script is used to drive the construction of reports
type Script = [Command]

-- | Title of a section of an event
type Title = String

-- | Event filters
data EventFilter =
    -- | A single event
    --
    -- Examples
    -- > GC     -- the GC event
    -- > "foo"  -- user event "foo"
    -- > 5      -- thread ID 5
    Is EventId

    -- | Any user event
    --
    -- Example
    -- > user
  | IsUser

    -- | Any thread event
    --
    -- Example
    -- > thread
  | IsThread

    -- | Logical or
    --
    -- Example
    -- > [GC, "foo", 5]
  | Any [EventFilter]
  deriving Show

-- | Sorting
data EventSort =
    -- | Sort by event name
    --
    -- Example
    -- > thread by name
    SortByName

    -- | Sort by total
    --
    -- Example
    -- > user by name
  | SortByTotal
    -- | Sort by start time
    --
    -- Example
    -- > user by start
  | SortByStart
  deriving Show

-- | Commands
data Command =
    -- | Start a new section
    --
    -- Example
    -- > section "User events"
    Section Title

    -- | A single event
    --
    -- Example
    -- > "foo"  -- user event "foo"
  | One EventId     (Maybe Title)

    -- | Show all the matching events
    --
    -- Examples
    -- > user by total  -- all user events, sorted
    -- > [4, 2, 3]      -- thread events 4, 2 and 3, in that order
  | All EventFilter (Maybe EventSort)

    -- | Sum over the specified events
    --
    -- Example
    -- > sum user
  | Sum EventFilter (Maybe Title)
  deriving Show

{-------------------------------------------------------------------------------
  Script execution
-------------------------------------------------------------------------------}

matchesFilter :: EventFilter -> EventId -> Bool
matchesFilter (Is eid') eid = eid' == eid
matchesFilter IsUser    eid = isUserEvent eid
matchesFilter IsThread  eid = isThreadEvent eid
matchesFilter (Any fs)  eid = or (map (`matchesFilter` eid) fs)

{-------------------------------------------------------------------------------
  Lexical analysis
-------------------------------------------------------------------------------}

lexer :: P.TokenParser ()
lexer = P.makeTokenParser haskellDef {
            P.reservedNames = [
                "section"
              , "GC"
              , "user"
              , "thread"
              , "as"
              , "by"
              , "total"
              , "name"
              , "all"
              ]
           }

reserved      = P.reserved      lexer
stringLiteral = P.stringLiteral lexer
natural       = P.natural       lexer
squares       = P.squares       lexer
commaSep1     = P.commaSep1     lexer
whiteSpace    = P.whiteSpace    lexer

{-------------------------------------------------------------------------------
  Syntax analysis
-------------------------------------------------------------------------------}

type Parser a = Parsec String () a

pEventId :: Parser EventId
pEventId =  (EventUser     <$> stringLiteral <*> pure 0 <?> "user event")
        <|> (EventThread   <$> pThreadId     <?> "thread event")
        <|> (const EventGC <$> reserved "GC")
  where
    pThreadId = fromIntegral <$> natural

pEventFilter :: Parser EventFilter
pEventFilter =  (Is             <$> pEventId)
            <|> (const IsUser   <$> reserved "user")
            <|> (const IsThread <$> reserved "thread")
            <|> (Any            <$> (squares $ commaSep1 pEventFilter))

pCommand :: Parser Command
pCommand = (Section <$> (reserved "section" *> stringLiteral))
       <|> (One     <$> pEventId                         <*> pTitle)
       <|> (Sum     <$> (reserved "sum" *> pEventFilter) <*> pTitle)
       <|> (All     <$> (reserved "all" *> pEventFilter) <*> pEventSort)

pEventSort :: Parser (Maybe EventSort)
pEventSort = optionMaybe $ reserved "by" *> (
                     (const SortByTotal <$> reserved "total")
                 <|> (const SortByName  <$> reserved "name")
                 <|> (const SortByStart <$> reserved "start")
               )

pTitle :: Parser (Maybe Title)
pTitle = optionMaybe (reserved "as" *> stringLiteral)

pScript :: Parser Script
pScript = whiteSpace *> many1 pCommand <* eof

{-------------------------------------------------------------------------------
  Quasi-quoting
-------------------------------------------------------------------------------}

$(deriveLiftMany [''EventId, ''EventFilter, ''EventSort, ''Command])

#if !MIN_VERSION_template_haskell(2,10,0)
instance Lift Word32 where
  lift = let conv :: Word32 -> Int ; conv = fromEnum in lift . conv
#endif

scriptQQ :: QuasiQuoter
scriptQQ = QuasiQuoter {
    quoteExp  = \e -> parseScriptString "<<source>>" e >>= lift
  , quotePat  = \_ -> fail "Cannot use script as a pattern"
  , quoteType = \_ -> fail "Cannot use script as a type"
  , quoteDec  = \_ -> fail "Cannot use script as a declaration"
  }

parseScriptString :: Monad m => String -> String -> m Script
parseScriptString source input =
  case runParser pScript () source input of
    Left  err    -> fail (show err)
    Right script -> return script

{-------------------------------------------------------------------------------
  Unparsing
-------------------------------------------------------------------------------}

unparseScript :: Script -> [String]
unparseScript = concatMap unparseCommand

unparseCommand :: Command -> [String]
unparseCommand (Section title) = ["", title]
unparseCommand (One eid title) = [unparseEventId eid ++ " " ++ unparseTitle title]
unparseCommand (All f sort)    = ["all " ++ unparseFilter f ++ " " ++ unparseSort sort]
unparseCommand (Sum f title)   = ["sum " ++ unparseFilter f ++ " " ++ unparseTitle title]

unparseEventId :: EventId -> String
unparseEventId EventGC           = "GC"
unparseEventId (EventUser e _)   = e
unparseEventId (EventThread tid) = show tid

unparseTitle :: Maybe Title -> String
unparseTitle Nothing  = ""
unparseTitle (Just t) = "as " ++ t

unparseSort :: Maybe EventSort -> String
unparseSort Nothing            = ""
unparseSort (Just SortByName)  = "by name"
unparseSort (Just SortByTotal) = "by total"
unparseSort (Just SortByStart) = "by start"

unparseFilter :: EventFilter -> String
unparseFilter (Is eid) = unparseEventId eid
unparseFilter IsUser   = "user"
unparseFilter IsThread = "thread"
unparseFilter (Any fs) = "[" ++ intercalate "," (map unparseFilter fs) ++ "]"
