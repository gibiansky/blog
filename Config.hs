module Config where

-- | Path to the post directory.
postsDir :: FilePath
postsDir = "posts/"

-- | Path to the file describing all blog posts. 
postsFile :: FilePath
postsFile = postsDir ++ "postlist"

blogRoot :: String
blogRoot = "http://" ++ serverAddress

googleAnalyticsId :: String
googleAnalyticsId = "UA-40714888-1" 

serverAddress :: String
serverAddress = "andrew.gibiansky.com"
