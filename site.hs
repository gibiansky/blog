import Hakyll

import Compilers
import Config
import Posts

--------------------------------------------------------------------------------

main :: IO ()
main = do
  -- Read the post descriptions and contents
  posts <- loadPosts
  hakyllWith configuration $ blog posts

--------------------------------------------------------------------------------

-- My Hakyll configuration. 
configuration :: Configuration
configuration = defaultConfiguration {
    -- Where to store backend files and temporary files (out of sight!).
    storeDirectory = ".store",
    tmpDirectory = ".store/tmp",

    -- Where to read input files from.
    providerDirectory = ".",

    -- Where to put output files.
    destinationDirectory = "gen",

    -- How to deploy to server.
    deployCommand = concat ["rsync gen/* root@", serverAddress, ":/var/www/ --progress --delete --recursive"]
  }

-- Configuration for an RSS feed.
feedConfiguration :: FeedConfiguration
feedConfiguration = FeedConfiguration {
    feedTitle = "Andrew Gibiansky's Technical Blog",
    feedDescription = "Machine learning, data analysis, mathematics and modeling, physics and simulation, Haskell, and more.",
    feedAuthorName = "Andrew Gibiansky",
    feedAuthorEmail = "andrew.gibiansky@gmail.com",
    feedRoot = "http://www.gibiansky.com"
  }

blog :: [Post] -> Rules ()
blog posts = do
  -- General set up, not specific to our posts.
  loadTemplates
  copyImages
  copyScripts
  copyDownloads
  generateStyles

  -- Generate everything dependent on the list of posts.
  generatePosts posts
  generateCategoryPages posts
  generateHomepage posts
  generatePages posts
  generateArchive posts
  generateRssFeed feedConfiguration posts
