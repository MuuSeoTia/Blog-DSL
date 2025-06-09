{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

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

-- write file with UTF-8 encoding
writeFileUtf8 :: FilePath -> TL.Text -> IO ()
writeFileUtf8 path content = do
  handle <- openFile path WriteMode
  hSetEncoding handle utf8
  TL.hPutStr handle content
  hClose handle

-- blog posts
samplePosts :: [BlogPost]
samplePosts =
    [ BlogPost 1 "Why Did You Code Your Personal Website in Haskell?" (UTCTime (fromGregorian 2025 1 27) 19)
        [ HeaderContent "How it started"
        , TextContent "One day I was in the basement of Hayden Hall on campus and I was approached by a suspicious hooded figure who smelled like matcha powder. \
        \He came up to me ominously offering me a book of divine power: \"Learn Haskell by building a blog generator\" it was intriguing to say the least but I took it in stride \
        \and challenged myself to learn Haskell over winter break."
        , HeaderContent "The Process"
        , TextContent "At first the extremely functional nature of Haskell took me aback, as somebody entrenched in low level and often times very verbose \
        \programming languages such as C++, it changed my perception of programming languages entirely."
        , TextContent "A massive difference I found right away was how types worked in Haskell and how useful Type Classes are specifically with multiple input polymorphism. \
        \Another notable difference was the explicit nature of said inputs compared to a language like C++ where type conversions are largely implicit.\
        \I'm currently still getting used to implementing more Functors into my code and utilizing abstractions rather than mutation for not just Haskell but other languages I code in."
        , HeaderContent "What I Learned"
        , TextContent "Through this project, I gained hands-on experience with:"
        , TextContent "• How to build DSLs in Haskell\n• Functional programming patterns\n• Static site generation\n• CSS generation\n• Haskell syntax and logic"
        , CodeBlock "haskell" "-- Example of my Blog DSL\ndata BlogElement = \n    TextContent Text\n  | HeaderContent Text\n  | Image { path :: Text, alt :: Text }"
        , HeaderContent "What's next + Reveal of the Mysterious Figure"
        , TextContent "My next steps as of this date is to add a Markdown parser for better formatting, adding a few more styling tools to my CSS EDSL \
        \ and generating a Projects page, maybe even add a Tetris block game to keep people's attention. Also the mysterious figure was my good friend Lucas, \
        \ the link to his website will be somewhere on your screen. Please make sure to endorse him in \"Rizz\" when you have the opportunity."
        ]
    , BlogPost 2 "Q&A 1" (UTCTime (fromGregorian 2025 1 27) 21)
        [ HeaderContent "Q: Is AI taking CS jobs?"
        , TextContent "A: For individuals who have poor code structure, lack of diversity of applicable skills and DS majors yes, otherwise you're fine."
        , HeaderContent "Q: Is Quantum Computing Even Real?"
        , TextContent "A: Ask yourself: What does it mean for something to be real do you need to see it? Or simply believe in it? Is it something that is proven? \
        \ If you're looking for an opinion from me then the answer is yes. (I know nothing about philosophy)"
        , HeaderContent "Q: How to get \"cracked\"?"
        , TextContent "A: Don't. "
        , HeaderContent "Q: TypeScript?"
        , TextContent "A: A bad day writing code in C++ is better than a good day writing code in TypeScript -David Stigant (Surely)"
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
        a_ [href_ "projects.html"] "Projects"
      main_ [] $ do
        div_ [class_ "about-section"] $ do
          h1_ "About Me"
          img_ [class_ "profile-img", src_ "images/headshot.png", alt_ "Mouad Tiahi"]
          p_ "Computer Science & Physics Major with a Minor in Mathematics, Prev Cloud Intern @ Amazon and Software Engineering Intern @ Dell Technologies" 
          p_ "I specialize in:"
          ul_ $ do
            li_ "Machine Learning & Deep Learning (Regression, Classification, Computer Vision, NLP, Reinforcement Learning)"
            li_ "High Performance Computing (CUDA, Metal, Clustering, Slurm, Dynamic Binary Instrumentation)"
            li_ "Cloud Computing (Google Cloud Platform, AWS, Microsoft Azure, Terraform, Hashicorp Lang)"
          div_ [class_ "social-links"] $ do
            a_ [href_ "https://github.com/MuuSeoTia", target_ "_blank"] "GitHub |"
            a_ [href_ "www.linkedin.com/in/mouad-tiahi", target_ "_blank"] " LinkedIn |"
            a_ [href_ "mailto:tiahimouad22@gmail.com"] " Email |"
      footer_ [class_ "footer"] $ do
        p_ [] "Generated in Haskell"

-- generate projects page
generateProjects :: Html()
generateProjects = doctypehtml_ $ do
  head_ $ do 
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "Projects"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "css/style.css"]
  body_ $ do
    div_ [class_ "container"] $ do
      nav_ [class_ "nav"] $ do
        a_ [href_ "index.html"] "Home"
        a_ [href_ "about.html"] "About"
        a_ [href_ "projects.html"] "Projects"
      main_ [class_ "projects-main"] $ do
        div_ [class_ "project-section"] $ do
          h1_ [class_ "section-title"] "My Projects"
          img_ [class_ "profile-img", src_ "images/conference.png", alt_ "MIT URTC Conference 2024"]
          
          -- project 1
          div_ [class_ "project-card"] $ do
            h2_ [class_ "project-title"] "Sustainable AF"
            p_ [class_ "project-description"] "A web application that allows users to find sustainable products and services in their area with a solar heat map and a carbon footprint calculator."
            div_ [class_ "project-links"] $ do
              a_ [href_ "https://www.youtube.com/watch?v=-a0d_5INf8Q&t=1s&ab_channel=FahadFaruqi", class_ "demo-link", target_ "_blank"] $ do
                span_ [class_ "icon"] "▶ "
                "Watch Demo"
              a_ [href_ "https://github.com/MuuSeoTia/carbon-advisor", class_ "github-link", target_ "_blank"] $ do
                span_ [class_ "icon"] "⌘ "
                "GitHub Repo"
            div_ [class_ "project-tech"] $ do
              p_ [class_ "tech-title"] "Technologies:"
              div_ [class_ "tech-list"] $ do
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot gcp"] "●"
                  span_ "Google Cloud Platform"
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot react"] "●"
                  span_ "React.js"
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot typescript"] "●"
                  span_ "TypeScript"
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot python"] "●"
                  span_ "Python"
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot ml"] "●"
                  span_ "Machine Learning"
          
          -- project 2
          div_ [class_ "project-card"] $ do
            h2_ [class_ "project-title"] "Maritime"
            p_ [class_ "project-description"] "Mobile application which allows users to track live microplastic concentrations in the ocean and allows for waste identification."
            div_ [class_ "project-links"] $ do
              a_ [href_ "https://youtu.be/Dj94vM3d93Q?si=ZK42k89Ask6-Or8u", class_ "demo-link", target_ "_blank"] $ do
                span_ [class_ "icon"] "▶ "
                "Watch Demo"
              a_ [href_ "https://github.com/Zapaway/maritime", class_ "github-link", target_ "_blank"] $ do
                span_ [class_ "icon"] "⌘ "
                "GitHub Repo"
            div_ [class_ "project-tech"] $ do
              p_ [class_ "tech-title"] "Technologies:"
              div_ [class_ "tech-list"] $ do
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot python"] "●"
                  span_ "Python"
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot flask"] "●"
                  span_ "Flask"
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot expo"] "●"
                  span_ "Expo"
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot typescript"] "●"
                  span_ "TypeScript"
                div_ [class_ "tech-item"] $ do
                  span_ [class_ "tech-dot cnn"] "●"
                  span_ "CNN"

              -- project 3
              div_ [class_ "project-card"] $ do
                h2_[class_ "project-title"] "Compiler"
          
          -- current projects section
          div_ [class_ "current-projects"] $ do
            h2_ [class_ "section-title"] "Current Research Projects"
            div_ [class_ "research-list"] $ do
              div_ [class_ "research-item"] $ do
                span_ [class_ "research-dot"] "◆"
                span_ "Writing a Compiler to 3D Objects"
              div_ [class_ "research-item"] $ do
                span_ [class_ "research-dot"] "◆"
                span_ "NvBit Compiler for NVIDIA CUDA"
              div_ [class_ "research-item"] $ do
                span_ [class_ "research-dot"] "◆"
                span_ "Enhanced Workload Handling via Integrating LLM into Slurm"
      
      footer_ [class_ "footer"] $ do
        p_ [] "Generated in Haskell"

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
        a_ [href_ "projects.html"] "Projects"
      main_ [] $ do
        div_ [class_ "hero-section"] $ do
          img_ [class_ "profile-img", src_ "images/headshot.png", alt_ "Mouad Tiahi"]
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

-- generate static files
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

  -- Generate projects page
  writeFileUtf8 "dist/projects.html" $ renderText generateProjects

  putStrLn "Site generated successfully in dist/ directory!"

main :: IO ()
main = generateSite
