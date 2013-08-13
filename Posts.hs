module Posts (
  Post,
  Year, Month, Day, Category, PostType (..),
  source, title, date, categories, contents, filetype,
  loadPosts) where

import Control.Applicative ((<$>))

import System.Directory
import Data.String.Utils

import Text.Parsec
import Text.Parsec.String
import Text.Parsec.Perm

data Post = Post {
  source :: FilePath,
  title :: String,
  date :: (Year, Month, Day),
  categories :: [Category],
  contents :: String,
  filetype :: PostType
  }

type Year = Int
type Month = Int
type Day = Int
type Category = String

data PostType = Markdown deriving Show

-- Metadata present in post file.
data PostMeta = PostMeta FilePath String (Year, Month, Day) [Category]

-- | Path to the post directory.
postsDir :: FilePath
postsDir = "posts/"

-- | Path to the file describing all blog posts. 
postsFile :: FilePath
postsFile = postsDir ++ "postlist"

-- | Read and parse the list of posts.
loadPosts :: IO [Post]
loadPosts = do
  parsed <- parseFromFile (many1 postParser) postsFile 
  case parsed of
    Left err -> error $ show err
    Right parsedPosts -> mapM readPost parsedPosts

readPost :: PostMeta -> IO Post
readPost (PostMeta srcdir postname postdate postcats) = do
  postfile <- getPostFile srcdir
  postdata <- readFile $ srcdir ++ "/" ++ postfile
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
directoryParser = field "source" $ do
  dirname <- many1 anyChar
  return $ postsDir ++ dirname


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
