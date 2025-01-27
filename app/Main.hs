{-# LANGUAGE OverloadedStrings #-}

module Main where

import BlogDSL
import Data.Time (UTCTime(..), fromGregorian)
import qualified Data.Text.Lazy.IO as TL
import Data.Text (Text, pack)
import System.Directory (createDirectoryIfMissing, copyFile, doesDirectoryExist, listDirectory)
import System.FilePath ((</>), takeExtension)
import CSS (renderCSS)
import Lucid
import System.IO (IOMode(..), openFile, hSetEncoding, utf8, hClose)
import qualified Data.Text.Lazy as TL
import Control.Monad (forM_, when)

-- | Write file with UTF-8 encoding
writeFileUtf8 :: FilePath -> TL.Text -> IO ()
writeFileUtf8 path content = do
  handle <- openFile path WriteMode
  hSetEncoding handle utf8
  TL.hPutStr handle content
  hClose handle

-- blog posts
samplePosts :: [BlogPost]
samplePosts =
    [ BlogPost 1 "Building a Static Site Generator in Haskell" (UTCTime (fromGregorian 2024 1 15) 0)
        [ HeaderContent "My Journey with Haskell"
        , TextContent "I embarked on this project to deepen my understanding of functional programming and Haskell. Building a static site generator from scratch using EDSLs has been an enlightening experience."
        , HeaderContent "What I Learned"
        , TextContent "Through this project, I gained hands-on experience with:"
        , TextContent "• Type-safe DSLs in Haskell\n• Functional programming patterns\n• Static site generation\n• CSS generation using Clay"
        , CodeBlock "haskell" "-- Example of our Blog DSL\ndata BlogElement = \n    TextContent Text\n  | HeaderContent Text\n  | Image { path :: Text, alt :: Text }"
        ]
    , BlogPost 2 "Functional Programming Principles" (UTCTime (fromGregorian 2024 1 20) 0)
        [ HeaderContent "Why Functional Programming Matters"
        , TextContent "Functional programming offers a different way of thinking about code. Instead of telling the computer how to do something, we focus on what we want to compute."
        , HeaderContent "Key Concepts"
        , TextContent "• Pure functions\n• Immutability\n• Higher-order functions\n• Type safety"
        ]
    ]

-- generate about page
generateAbout :: Html ()
generateAbout = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "About Me - Mouad Tiahi"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "css/style.css"]
  body_ $ do
    div_ [class_ "container"] $ do
      nav_ [class_ "nav"] $ do
        a_ [href_ "index.html"] "Home"
        a_ [href_ "about.html"] "About"
      main_ [] $ do
        div_ [class_ "about-section"] $ do
          h1_ "About Me"
          img_ [class_ "profile-img", src_ "../images/headshot.png", alt_ "Mouad Tiahi"]
          p_ "Computer Science & Physics Major with a Minor in Mathematics, Prev Cloud Intern @ Amazon and Software Engineering intern" 
          p_ "I specialize in:"
          ul_ $ do
            li_ "Functional Programming (Haskell, OCaml)"
            li_ "Web Development"
            li_ "System Design"
          div_ [class_ "social-links"] $ do
            a_ [href_ "https://github.com/MuuSeoTia", target_ "_blank"] "GitHub |"
            a_ [href_ "https://linkedin.com/in/your-profile", target_ "_blank"] " LinkedIn |"
            a_ [href_ "mailto:tiahimouad22@gmail.com"] " Email |"
      footer_ [class_ "footer"] $ do
        p_ [] "© 2024 Generated in Haskell"

-- index page
generateIndex :: [BlogPost] -> Html ()
generateIndex posts = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "Mouad Tiahi - Software Engineer"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "css/style.css"]
  body_ $ do
    div_ [class_ "container"] $ do
      nav_ [class_ "nav"] $ do
        a_ [href_ "index.html"] "Home"
        a_ [href_ "about.html"] "About"
      main_ [] $ do
        div_ [class_ "hero-section"] $ do
          img_ [class_ "profile-img", src_ "../images/headshot.png", alt_ "Mouad Tiahi"]
          h1_ "Mouad Tiahi"
          p_ [class_ "subtitle"] "Machine Learning & High Performance Researcher @ NUCAR | 3x Hackathon Winner | Aspiring Quantum Computing Researcher"
          div_ [class_ "social-links"] $ do
            a_ [href_ "https://github.com/MuuSeoTia", target_ "_blank", class_ "social-link"] "GitHub"
            a_ [href_ "https://www.linkedin.com/in/mouad-tiahi-0b361524b/", target_ "_blank", class_ "social-link"] " LinkedIn"
            a_ [href_ "mailto:tiahimouad22@gmail.com", class_ "social-link"] " Email"
        
        div_ [class_ "intro-section"] $ do
          h2_ "Welcome to My Website"
          p_ "This is where I share my thoughts and ideas about machine learning, programming, hackathons, quantum computing, and more."
        
        h2_ "Latest Posts"
        div_ [class_ "post-list"] $ do
          mapM_ (\post -> do
            article_ [class_ "post-preview"] $ do
              h3_ $ a_ [href_ $ "posts/" <> pack (show $ postId post) <> ".html"] $ toHtml $ title post
              p_ [class_ "post-date"] $ toHtml $ show $ date post
            ) posts
      footer_ [class_ "footer"] $ do
        p_ [] "Generated in Haskell"

-- | Copy all images from source to dist
copyImages :: IO ()
copyImages = do
  let sourceDir = "images"
  let targetDir = "dist/images"
  exists <- doesDirectoryExist sourceDir
  when exists $ do
    files <- listDirectory sourceDir
    let imageFiles = filter (\f -> takeExtension f `elem` [".png", ".jpg", ".jpeg", ".gif"]) files
    forM_ imageFiles $ \file -> do
      copyFile (sourceDir </> file) (targetDir </> file)

-- | Generate static files
generateSite :: IO ()
generateSite = do
  -- Create directories
  createDirectoryIfMissing True "dist"
  createDirectoryIfMissing True "dist/posts"
  createDirectoryIfMissing True "dist/css"
  createDirectoryIfMissing True "dist/js"
  createDirectoryIfMissing True "dist/images"

  -- Copy images
  copyImages

  -- Generate posts
  mapM_ (\post -> do
    let postPath = "dist/posts/" <> show (postId post) <> ".html"
    writeFileUtf8 postPath $ renderText $ renderBlogPost post
    ) samplePosts

  -- Generate index page
  writeFileUtf8 "dist/index.html" $ renderText $ generateIndex samplePosts
  
  -- Generate about page
  writeFileUtf8 "dist/about.html" $ renderText generateAbout

  -- Generate CSS
  writeFileUtf8 "dist/css/style.css" renderCSS

  putStrLn "Site generated successfully in dist/ directory!"

main :: IO ()
main = generateSite
