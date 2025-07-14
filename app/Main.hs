{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module Main where

import BlogDSL
import Pages
import Data.Time (UTCTime(..), fromGregorian, Day)
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

-- Sample experience data
sampleExperiences :: [Experience]
sampleExperiences = 
  [ Experience 1 "Amazon" "Cloud Infrastructure Intern" "Seattle, WA" 
      (fromGregorian 2024 6 1) (Just $ fromGregorian 2024 8 30)
      [ "Developed scalable cloud infrastructure solutions using AWS services"
      , "Optimized EC2 instance performance, resulting in 25% cost reduction"
      , "Collaborated with senior engineers on distributed systems architecture"
      ]
      ["AWS", "Python", "Terraform", "Docker", "Kubernetes"]
      [ "Improved deployment pipeline efficiency by 40%"
      , "Led implementation of automated monitoring system"
      , "Received intern excellence award for outstanding performance"
      ]
      Nothing
  , Experience 2 "Dell Technologies" "Software Engineering Intern" "Austin, TX"
      (fromGregorian 2023 6 1) (Just $ fromGregorian 2023 8 30)
      [ "Built full-stack web applications using modern frameworks"
      , "Implemented RESTful APIs with comprehensive documentation"
      , "Participated in agile development processes and code reviews"
      ]
      ["React", "Node.js", "TypeScript", "MongoDB", "Express"]
      [ "Delivered 3 production-ready features ahead of schedule"
      , "Reduced API response time by 35% through optimization"
      , "Mentored 2 junior interns on best practices"
      ]
      Nothing
  , Experience 3 "NUCAR Lab" "Machine Learning Researcher" "Boston, MA"
      (fromGregorian 2024 9 1) Nothing
      [ "Conducting research on high-performance computing and ML optimization"
      , "Developing CUDA kernels for accelerated deep learning workloads"
      , "Publishing research papers on distributed training algorithms"
      ]
      ["CUDA", "PyTorch", "C++", "Python", "Slurm", "MPI"]
      [ "Achieved 3x speedup in neural network training"
      , "Published 2 papers in top-tier conferences"
      , "Leading team of 4 graduate research assistants"
      ]
      Nothing
  ]

-- Sample education data  
sampleEducation :: [Education]
sampleEducation =
  [ Education 1 "Northeastern University" "Bachelor of Science" "Computer Science & Physics"
      (fromGregorian 2026 5 15)
      (Just 3.8)
      ["Dean's List", "Magna Cum Laude", "Outstanding Student in CS"]
      [ "Algorithms & Data Structures", "Machine Learning", "Computer Systems"
      , "Quantum Computing", "Distributed Systems", "Computer Graphics"
      ]
      Nothing
  ]

-- Sample skills data
sampleSkills :: [Skill]  
sampleSkills =
  [ Skill "Python" Expert Programming (Just 5)
  , Skill "Haskell" Advanced Programming (Just 2)
  , Skill "C++" Advanced Programming (Just 4)
  , Skill "JavaScript/TypeScript" Advanced Programming (Just 3)
  , Skill "CUDA" Intermediate Programming (Just 2)
  , Skill "React" Advanced Framework (Just 3)
  , Skill "PyTorch" Expert MachineLearning (Just 3)
  , Skill "TensorFlow" Advanced MachineLearning (Just 2)
  , Skill "AWS" Advanced Cloud (Just 2)
  , Skill "Docker" Advanced Tool (Just 2)
  , Skill "Kubernetes" Intermediate Tool (Just 1)
  ]

-- Sample certificates
sampleCertificates :: [Certificate]
sampleCertificates =
  [ Certificate "AWS Certified Cloud Practitioner" "Amazon Web Services"
      (fromGregorian 2024 3 15) (Just $ fromGregorian 2027 3 15)
      (Just "AWS-CCP-123456") 
      (Just "https://aws.amazon.com/verification")
  , Certificate "NVIDIA Deep Learning Institute Certificate" "NVIDIA"
      (fromGregorian 2024 7 20) Nothing
      Nothing
      (Just "https://nvidia.com/dli/certificate")
  ]

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
  
  -- Generate about page (without social links)
  writeFileUtf8 "dist/about.html" $ renderText generateAbout

  -- Generate projects page
  writeFileUtf8 "dist/projects.html" $ renderText generateProjects

  -- Generate experiences page using the DSL
  writeFileUtf8 "dist/experience.html" $ renderText $ 
    generateExperiences sampleExperiences sampleEducation sampleSkills sampleCertificates

  -- Generate CSS with all enhanced styles
  writeFileUtf8 "dist/css/style.css" renderCSS

  putStrLn "Enhanced website generated successfully in dist/ directory!"


main :: IO ()
main = generateSite
