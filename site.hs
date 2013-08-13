{-# LANGUAGE OverloadedStrings #-}
import Hakyll hiding (Markdown)

import Posts

import Data.Monoid (mconcat)
import Control.Monad (liftM)

import System.Locale (defaultTimeLocale)
import Data.Time.Calendar (fromGregorian)
import Data.Time.Format (formatTime)

compileWithFilter :: String -> [String] -> Item String -> Compiler (Item String)
compileWithFilter cmd args = withItemBody (unixFilter cmd args)

sassCompiler ::  Compiler (Item String)
sassCompiler = do
  str <- getResourceString
  liftM (fmap compressCss) $ compileWithFilter "sass" ["--stdin", "--scss"] str

markdownPost :: Item String -> Compiler (Item String)
markdownPost = compileWithFilter "pandoc" ["-f", "markdown", "-t", "html"]

postCompiler :: Post -> Compiler (Item String)
postCompiler post = do
  -- Decide which of the compilers to use.
  let fileProcessor = case filetype post of
                        Markdown -> markdownPost     
                    --  "tex" -> latexPost
                    --  "html" -> htmlPost
                    --  "ipynb" -> ipythonNotebookPost
  
  let postContext = mconcat [
          constField "title" $ title post,
          constField "date" $ showDate $ date post,
          defaultContext
          ]

  makeItem (contents post) >>= fileProcessor >>= loadAndApplyTemplate "templates/post.html" postContext
  
  
showDate :: (Year, Month, Day) -> String
showDate (year, month, day) = 
  let postDate = fromGregorian (fromIntegral year) month day in
    formatTime defaultTimeLocale "%Y-%m-%d" postDate

makePost :: Post -> Rules ()
makePost post =
  let outpage = fromFilePath $ source post ++ "/index.html" in
    create [outpage] $ do
      route idRoute
      compile $ postCompiler post

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

  -- Normal CSS files can be compressed.
  match "css/**.css" $ do
    route   idRoute
    compile compressCssCompiler

  -- *.sass need to first be converted to CSS, then compressed.
  match "css/**.scss" $ do
    -- Change extension to *.css
    route $ setExtension "css" 
    compile sassCompiler

  -- Compile all templates.
  match "templates/*" $ compile templateCompiler

  match "pages/*.md" $ do
    route   $ setExtension "html"
    compile $ pandocCompiler
      >>= loadAndApplyTemplate "templates/default.html" defaultContext
      >>= relativizeUrls

  mapM_ makePost posts

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

