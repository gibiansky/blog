{-# LANGUAGE OverloadedStrings #-}
import Hakyll hiding (Markdown)

import Posts

import Data.Monoid ((<>), mconcat)
import Control.Monad (liftM)
import Data.List (elemIndex, sortBy, intercalate)
import Data.Maybe (fromJust, maybeToList)
import Control.Applicative ((<$>))
import Data.Function (on)

import System.Locale (defaultTimeLocale)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (formatTime)

compileWithFilter :: String -> [String] -> Item String -> Compiler (Item String)
compileWithFilter cmd args = withItemBody (unixFilter cmd args)

sassCompiler ::  Compiler (Item String)
sassCompiler = do
  str <- getResourceString
  liftM (fmap compressCss) $ compileWithFilter "sass" ["--stdin", "--scss", "--compass"] str

markdownPost :: Item String -> Compiler (Item String)
markdownPost = compileWithFilter "pandoc" ["-f", "markdown", "-t", "html", "--mathjax"]

dateCompare :: (Year, Month, Day) -> (Year, Month, Day) -> Ordering
dateCompare (y1, m1, d1) (y2, m2, d2) = compare y1 y2 <> compare m1 m2 <> compare d1 d2

blogRoot :: String
blogRoot = "file:///Users/agibiansky/Code/fun/blog/_site"

postUrl :: Post -> String
postUrl post = blogRoot ++ "/" ++ source post

globalContext ::  Context String
globalContext = constField "google-analytics-id" "UA-40714888-1" <> 
                constField "root" blogRoot <>
                defaultContext

postCompiler :: [Post] -> Post -> Compiler (Item String)
postCompiler posts post = do
  -- Decide which of the compilers to use.
  let fileProcessor = case filetype post of
                        Markdown -> markdownPost     
                    --  "tex" -> latexPost
                    --  "html" -> htmlPost
                    --  "ipynb" -> ipythonNotebookPost
  
  let sortedPosts = sortBy (dateCompare `on` date) posts
      postIndex = fromJust $ elemIndex post posts
      previousPost =  if postIndex == 0 then Nothing else Just $ sortedPosts !! (postIndex - 1)
      nextPost = if postIndex == length posts - 1 then Nothing else Just $ sortedPosts !! (postIndex - 1)
      previousUrl = postUrl <$> previousPost
      nextUrl = postUrl <$> nextPost
      categoryList = intercalate ", " $ categories post
      fields = [constField "title" (title post),
                constField "date" (showDate $ date post),
                constField "categories" categoryList] ++
               maybeToList (constField "previousUrl" <$> previousUrl) ++
               maybeToList (constField "nextUrl" <$> nextUrl)

  let postContext = mconcat fields <> globalContext

  makeItem (contents post) >>=
    fileProcessor >>=
    loadAndApplyTemplate "templates/post.html" postContext >>=
    loadAndApplyTemplate "templates/default.html" postContext
 
  
showDate :: (Year, Month, Day) -> String
showDate (year, month, day) = 
  let postDate = fromGregorian (fromIntegral year) month day in
    formatTime defaultTimeLocale "%Y-%m-%d" postDate

makePost :: [Post] -> Post -> Rules ()
makePost posts post =
  let outpage = fromFilePath $ concat [head $ categories post, "/", source post, "/index.html"] in
    create [outpage] $ do
      route idRoute
      compile $ postCompiler posts post

--------------------------------------------------------------------------------
main :: IO ()
main = do
  posts <- loadPosts
  hakyll $ blog posts

blog :: [Post] -> Rules ()
blog posts = do
  -- Images in the images directory are just copied over directly.
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler

  -- *.sass need to first be converted to CSS, then compressed.
  match "css/screen.scss" $ do
    -- Change extension to *.css
    route $ setExtension "css" 
    compile sassCompiler

  -- Compile all templates.
  match "templates/*" $ compile templateCompiler

  mapM_ (makePost posts) posts

---- Normal CSS files can be compressed.
--match "css/**.css" $ do
--  route   idRoute
--  compile compressCssCompiler

--match "pages/*.md" $ do
--  route   $ setExtension "html"
--  compile $ pandocCompiler
--    >>= loadAndApplyTemplate "templates/default.html" defaultContext
--    >>= relativizeUrls

--match "posts/*" $ do
--  route $ setExtension "html"
--  compile $ pandocCompiler
--    >>= loadAndApplyTemplate "templates/post.html"    postCtx
--    >>= loadAndApplyTemplate "templates/default.html" postCtx
--    >>= relativizeUrls

--create ["archive.html"] $ do
--  route idRoute
--  compile $ do
--    posts <- recentFirst =<< loadAll "posts/*"
--    let archiveCtx =
--            listField "posts" postCtx (return posts) `mappend`
--            constField "title" "Archives"            `mappend`
--            defaultContext

--    makeItem ""
--      >>= loadAndApplyTemplate "templates/archive.html" archiveCtx
--      >>= loadAndApplyTemplate "templates/default.html" archiveCtx
--      >>= relativizeUrls


--match "index.html" $ do
--  route idRoute
--  compile $ do
--    posts <- recentFirst =<< loadAll "posts/*"
--    let indexCtx =
--            listField "posts" postCtx (return posts) `mappend`
--            constField "title" "Home"                `mappend`
--            defaultContext

--    getResourceBody
--      >>= applyAsTemplate indexCtx
--      >>= loadAndApplyTemplate "templates/default.html" indexCtx
--      >>= relativizeUrls

