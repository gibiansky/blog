module Contexts where

import Hakyll

import Config
import Posts

import Control.Applicative  ((<$>))
import Data.List            (elemIndex, intercalate, find)
import Data.Maybe           (fromJust, catMaybes)
import Data.Monoid          ((<>), mconcat)
import Text.Printf          (printf)

-- | Constants from the Config module.
constantsContext :: Context a
constantsContext =
  constField "google-analytics-id" googleAnalyticsId <> 
  constField "root" blogRoot

-- | The default context to use for my blog.
globalContext ::  Context String
globalContext = constantsContext <> defaultContext

-- | A list context for all the provided posts.
postListContext :: String -> [Post] -> Context String
postListContext name posts = listField name (minimalPostContext posts) (mapM makeItem posts)

-- | A minimal post context with info only about this post.
minimalPostContext :: [Post] -> Context Post
minimalPostContext posts = 
    postField "categoryLinks"  makeCategoryLinks <>
    postField "date"           dateString <>
    postField "monthAndDay"    dayString <>
    postField "title"          title <>
    postField "url"            url <>
    postField "yearHeading"    yearHeading <>
    constantsContext
  where -- Convenience function for post fields.
        postField fieldName func = field fieldName $ return . func . itemBody

        -- Generate HTML for a link to a category page.
        linkToCategory category = printf "<a href=\"%s/blog/categories/%s\">%s</a>" blogRoot category category
        -- Generate a links to a list of categories.
        makeCategoryLinks post = intercalate ", " $ map linkToCategory $ categories post

        -- Generate a year heading for this post, if its the first post of the year.
        yearHeading post = if isFirstInYear post then printf "<h2>%s</h2>" (show $ postYear post) else ""
        -- Return whether this is the first post of the year.
        isFirstInYear post = (fromJust . find (sameYear post)) posts == post
        -- Check whether two posts are in the same yaer.
        sameYear p1 p2 = postYear p1 == postYear p2

-- | A complete post context, with info about previous and next posts as well as recent posts. 
fullPostContext :: [Post] -> Post -> Context String
fullPostContext posts post = 
  let -- Find where in the post list this post is.
      postIndex = fromJust $ post `elemIndex` posts
  
      -- If this is the first post, no previous one exists.
      previousPost =  if post == last posts then Nothing else Just $ posts !! (postIndex + 1)

      -- If this is the most recent post, no next one exists.
      nextPost =      if post == head posts then Nothing else Just $ posts !! (postIndex - 1)

      -- Operating inside Maybe, get the previous and next post URLs and titles.
      previousUrl = url <$> previousPost
      nextUrl =     url <$> nextPost

      previousTitle = title <$> previousPost
      nextTitle =     title <$> nextPost

      postTypeField = constField (show $ filetype post) ""

      -- Extract all fields that exist from the Maybes.
      extraFields =
        catMaybes [
          constField "previousUrl"   <$> previousUrl,
          constField "nextUrl"       <$> nextUrl,
          constField "previousTitle" <$> previousTitle,
          constField "nextTitle"     <$> nextTitle
        ] in

    -- Include minimal post context
    applyContext post (minimalPostContext posts) <>

    -- Include recent posts
    postListContext "recentPosts" posts <>

    -- Post type field so templates can specialize
    postTypeField <>
    
    -- And all new fields introduced above.
    mconcat extraFields <>

    -- And conclude with the normal blog context.
    globalContext

  where
    -- Utility function for converting Context a to Context String.
    applyContext :: a -> Context a -> Context String
    applyContext val (Context uncontext) = Context newUncontext
      where newUncontext name s _ = makeItem val >>= uncontext name s
