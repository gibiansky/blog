module Config where

-- | Path to the post directory.
postsDir :: FilePath
postsDir = "posts/"

-- | Path to the file describing all blog posts. 
postsFile :: FilePath
postsFile = postsDir ++ "postlist"

blogRoot :: String
blogRoot = "file:///home/silver/code/blog/gen"

googleAnalyticsId :: String
googleAnalyticsId = "UA-40714888-1" 
