{-# LANGUAGE OverloadedStrings #-}
module Compilers where

import Hakyll hiding (Markdown)

import Contexts
import Posts

import Control.Monad  (liftM)
import Data.Char      (toUpper)
import Data.List      (nub)
import Data.Monoid    ((<>))
import Text.Printf    (printf)

-- | Compile using a Unix filter.
compileWithFilter :: String -> [String] -> Item String -> Compiler (Item String)
compileWithFilter cmd args = withItemBody (unixFilter cmd args)

-- | Compile a Compass project.
-- | Compress the CSS after compilation.
compassCompiler ::  Compiler (Item String)
compassCompiler = do
  str <- getResourceString
  liftM (fmap compressCss) $ compileWithFilter "sass" ["--stdin", "--scss", "--compass"] str

-- | Compile a markdown file.
markdownPost :: Item String -> Compiler (Item String)
markdownPost = compileWithFilter command arguments
  where 
    command = "pandoc"
    arguments = ["-f", "markdown", "-t", "html5", "--mathjax", "--highlight-style=pygments"]

-- | Compile a Latex file.
latexPost :: Item String -> Compiler (Item String)
latexPost = compileWithFilter command arguments
  where 
    command = "pandoc"
    arguments = ["-f", "latex", "-t", "html5", "--mathjax", "--highlight-style=pygments"]

-- Compile an IPython notebook using a custom script.
ipynbPost :: Item String -> Compiler (Item String)
ipynbPost = compileWithFilter command arguments
  where command = "notebook-convert"
        arguments = []

-- | Get the compiler for a type of content.
compilerFor :: PostType -> Item String -> Compiler (Item String)
compilerFor Markdown = markdownPost
compilerFor Latex = latexPost
compilerFor IPythonNotebook = ipynbPost

-- | Compile a post of any type.
postCompiler :: [Post] -> Post -> Compiler (Item String)
postCompiler posts post = do
  let context = fullPostContext posts post

  makeItem (contents post) >>=
    compilerFor (filetype post) >>=
    loadAndApplyTemplate "templates/post.html" context >>=
    loadAndApplyTemplate "templates/default.html" context

-- | Images in the images directory are just copied over directly.
copyImages ::  Rules ()
copyImages = do
  match "images/**" $ do
    route   idRoute
    compile copyFileCompiler
  match "favicon.ico" $ do
    route   idRoute
    compile copyFileCompiler

-- | Copy over all downloadable files.
copyDownloads ::  Rules ()
copyDownloads = match "downloads/**" $ do
  route   idRoute
  compile copyFileCompiler

-- | Copy over all javascript files.
copyScripts ::  Rules ()
copyScripts = match "script/**/*.js" $ do
  route $ gsubRoute "script" $ const "javascripts"
  compile copyFileCompiler

-- | Generate the CSS from the Compass project.
generateStyles ::  Rules ()
generateStyles = do
  match "css/screen.scss" $ do
    -- Change extension to *.css
    route $ setExtension "css" 
    compile compassCompiler

  -- Also copy over any other CSS files.
  match "css/*.css" $ do
    -- Change extension to *.css
    route idRoute
    compile copyFileCompiler

-- | Load all templates in the template directory.
loadTemplates ::  Rules ()
loadTemplates = match "templates/*" $ compile templateCompiler


-- | Generate a page for each unique post category with a list of those posts.
generateCategoryPages ::  [Post] -> Rules ()
generateCategoryPages posts = mapM_ (generateCategoryPage posts) uniqueCategories
  where
    -- Extract all the unique categories that exist and generate a page for each.
    uniqueCategories = nub $ concatMap categories posts
  
generateHomepage ::  [Post] -> Rules ()
generateHomepage posts = do
    -- Move all images from the most recent post directory to the image folder.
    match (fromGlob (imageSource ++  "/*")) $ version "index" $ do
      route $ gsubRoute imageSource $ const "images"
      compile copyFileCompiler

    -- Create the actual homepage, using just the last post page.
    create ["index.html"] $ do
      route idRoute
      compile (load (fromFilePath htmlSource) >>= makeItem . itemBody :: Compiler (Item String))

  where 
    -- Get the most recent posts' image directory and generated HTML.
    lastPost = head posts
    imageSource = printf "%s%s/images" postsDir $ source lastPost
    htmlSource = printf "blog/%s/%s/index.html" (head $ categories lastPost) $ source lastPost

-- | Generate pages for all markdown files in pages.
generatePages ::  [Post] -> Rules ()
generatePages posts = match "pages/*" $ do
  route $ setExtension "html"
  compile $ pandocCompiler
    >>= loadAndApplyTemplate "templates/page.html" (postListContext "recentPosts" posts  <> globalContext)
    >>= loadAndApplyTemplate "templates/default.html" globalContext
    >>= relativizeUrls

generateArchive ::  [Post] -> Rules ()
generateArchive posts = create ["archive.html"] $ do
  route idRoute
  compile $ do
    let context = postListContext "posts" posts <>
                  postListContext "recentPosts" posts <>
                  constField "title" "Blog Archive" <>
                  globalContext
    makeItem ""
      >>= loadAndApplyTemplate "templates/archive.html" context
      >>= loadAndApplyTemplate "templates/default.html" context
      >>= relativizeUrls

generateCategoryPage ::  [Post] -> Category -> Rules ()
generateCategoryPage posts category = create [fromFilePath location] $ do
    route idRoute
    compile $ do
      let context = postListContext "posts" postsInCategory <>
                    constField "categoryName" categoryName <>
                    postListContext "recentPosts" posts <>
                    globalContext

      makeItem ""
        >>= loadAndApplyTemplate "templates/category.html" context
        >>= loadAndApplyTemplate "templates/default.html" context
        >>= relativizeUrls
  where
    location = concat ["blog/categories/", category, "/index.html"]
    -- Get all posts in this category.
    postsInCategory = filter ((category `elem`) . categories) posts

    -- Convert a category name to a category heading, with capitalized words and spaces.
    wordsInCategory = splitAll "-" category
    categoryName = unwords $ map capitalize wordsInCategory
      where capitalize (x:xs) = toUpper x : xs

-- | Generate all the post pages.
generatePosts ::  [Post] -> Rules ()
generatePosts posts = mapM_ (generatePost posts) posts

-- | Generate a single posts page.
generatePost :: [Post] -> Post -> Rules ()
generatePost posts post =
  let outDirectory = concat ["blog/", head $ categories post, "/", source post, "/"]
      inImgDirectory = postsDir ++ source post ++ "/images"
      inDataDirectory = postsDir ++ source post ++ "/data"
      inFilesDirectory = postsDir ++ source post ++ "/files"
      outImgDirectory = outDirectory ++ "images"
      outDataDirectory = outDirectory ++ "data"
      outFilesDirectory = outDirectory ++ "files"
      outHtml = outDirectory ++ "index.html" in
    do
      -- Copy over files in /images
      match (fromGlob $ inImgDirectory ++ "/**") $ version "post" $ do
        route  $ gsubRoute inImgDirectory (const outImgDirectory)
        compile copyFileCompiler

      -- Copy over files in /data
      match (fromGlob $ inDataDirectory ++ "/**") $ version "post" $ do
        route  $ gsubRoute inDataDirectory (const outDataDirectory)
        compile copyFileCompiler

      -- Copy over files in /files
      match (fromGlob $ inFilesDirectory ++ "/**") $ version "post" $ do
        route  $ gsubRoute inFilesDirectory (const outFilesDirectory)
        compile copyFileCompiler

      -- Create actual post
      create [fromFilePath outHtml] $ do
        route idRoute
        compile $ postCompiler posts post
