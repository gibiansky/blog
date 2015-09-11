module Posts (
Post, Year, Month, Day, Category, PostType (..), source, title, date,
  categories, contents, filetype, url, dateString, dayString, postYear, rssDate,
  postMonth, postDay, postsDir, loadPosts) where

import Config

import Control.Applicative ((<$>))
import Data.List           (sortBy)
import Data.Monoid         ((<>))
import Data.String.Utils   (endswith, startswith)
import Data.Time.Calendar  (fromGregorian)
import Data.Time.Format    (formatTime, defaultTimeLocale)
import System.Directory    (getDirectoryContents)

import Text.Parsec
import Text.Parsec.Perm
import Text.Parsec.String

data Post = Post {
  source :: FilePath,
  title :: String,
  date :: (Year, Month, Day),
  categories :: [Category],
  contents :: String,
  filetype :: PostType
  } deriving Eq

type Year = Int
type Month = Int
type Day = Int
type Category = String

data PostType = Markdown | Latex | IPythonNotebook | Asciidoc deriving (Show, Eq)

-- Metadata present in post file.
data PostMeta = PostMeta FilePath String (Year, Month, Day) [Category]

url :: Post -> String
url post = concat [blogRoot, "/blog/", head $ categories post, "/", source post]

dateFmt :: String -> Post -> String
dateFmt fmt post = 
  let (year, month, day) = date post
      postDate = fromGregorian (fromIntegral year) month day in
    formatTime defaultTimeLocale fmt postDate

dateString :: Post -> String
dateString = dateFmt "%A, %B %e, %Y"

rssDate :: Post -> String
rssDate = dateFmt "%a, %e %b %Y 00:00:00"

dayString :: Post -> String
dayString = dateFmt "%b %e"

postYear :: Post -> Year
postYear post = yr
  where (yr, _, _) = date post

postMonth :: Post -> Month
postMonth post = mn
  where (_, mn, _) = date post

postDay :: Post -> Day
postDay post = dy
  where (_, _, dy) = date post

-- | Read and parse the list of posts.
-- | Returns a list of sorted posts, recent first.
loadPosts :: IO [Post]
loadPosts = do
  parsed <- parseFromFile (many1 postParser) postsFile 
  case parsed of
    Left err -> error $ show err
    Right parsedPosts -> 
      let posts = mapM readPost parsedPosts in
        sortBy compareByDate <$> posts

readPost :: PostMeta -> IO Post
readPost (PostMeta srcdir postname postdate postcats) = do
  postfile <- getPostFile $ postsDir ++ srcdir
  postdata <- readFile $ postsDir ++ srcdir ++ "/" ++ postfile
  return Post {
      source = srcdir,
      title = postname,
      date = postdate,
      categories = postcats,
      contents = postdata,
      filetype = getPostType postfile
    }

getPostType :: String -> PostType
getPostType filename
  | endswith ".md" filename = Markdown
  | endswith ".markdown" filename = Markdown
  | endswith ".tex" filename = Latex
  | endswith ".ipynb" filename = IPythonNotebook
  | endswith ".adoc" filename = Asciidoc


-- | Compare two posts by their dates.
compareByDate :: Post -> Post -> Ordering
compareByDate p1 p2 =
  let (y1, m1, d1) = date p1
      (y2, m2, d2) = date p2 in
    compare y2 y1 <> compare m2 m1 <> compare d2 d1

getPostFile :: FilePath -> IO String
getPostFile srcdir = do
  files <- getDirectoryContents srcdir
  let potentialPosts = filter (startswith "post.") files
  case length potentialPosts of
    0 -> error $ "Could not find post file in " ++ srcdir
    1 -> return $ head potentialPosts
    _ -> error $ "Too many post files in " ++ srcdir

-- | Parse a single post.
-- | A post has the format:
-- |   post {
-- |     [field value;]..
-- |   }
-- | Fields are currently 'date', 'title', and 'categories'.
postParser :: Parser PostMeta
postParser = do
  -- Allow the 'post' and braces tokens to have arbirary whitespace around them.
  whitespaced $ string "post"
  whitespaced $ char '{'
  post <- postData
  whitespaced $ char '}'
  return post

-- | Parse the actual data fields of a post.
-- | These can occur in any order, but must all be present.
postData :: Parser PostMeta
postData = permute $ PostMeta <$$> directoryParser <||> titleParser <||> dateParser <||> categoryParser

-- | Parse a field in the format:
-- |   title "Text";
-- | Note that quote escaping is not supported.
titleParser :: Parser String
titleParser = field "title" $ do
  char '"'
  manyTill anyChar $ char '"'

-- | Parse a date in the form %d-%d-%d (year, month, day).
dateParser :: Parser (Year, Month, Day)
dateParser = field "date" $ do
  let int = read <$> many1 digit
  [year, month, day] <- sepBy int $ char '-'
  return (year, month, day)

-- | Parse a list of categories, separated by commas.
categoryParser ::  Parser [Category]
categoryParser = field "categories" $ sepBy category comma
  where comma = whitespaced $ char ','
        category = many $ letter <|> digit <|> char '-'

directoryParser :: Parser FilePath
directoryParser = field "source" $ many1 anyChar

-- | Parse a generic field in the format 'field: value;'
field :: String -> Parser a -> Parser a
field name parser = whitespaced $ do
  -- First, get the string that is the field value
  string name
  char ' '
  fieldValue <- manyTill anyChar $ char ';'

  -- Then, parse the actual field value using the field parser
  loc <- sourceName <$> getPosition
  case parse parser loc fieldValue of
    Left err -> error $ show err
    Right result -> return result

-- | Parse anything surrounded by whitespace on both sides.
whitespaced :: Parser a -> Parser a
whitespaced parser = do
  whitespace
  result <- parser
  whitespace
  return result

-- | Parse whitespace characters.
whitespace :: Parser String
whitespace = many $ oneOf " \n\t"
