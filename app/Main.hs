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

-- experience block
sampleExperiences :: [Experience]
sampleExperiences = 
  [ Experience 1 "Amazon" "Cloud Infrastructure Intern" "Boston, MA" 
      (fromGregorian 2023 9 4) (Just $ fromGregorian 2023 12 20)
      [ "Spearheaded AWS operations and connectivity for a system managing thousands of database points, optimizing data flow\n\ 
      \and improving latency by 15% within connections across distributed environments."
      , "Collaborated on the design and optimization of database architecture, enabling faster query execution and supporting the\n\ 
        \development of secure, scalable API endpoints to facilitate seamless data access"
      , "Integrated vector databases, AWS Amplify, and AWS Direct Connect to enable real-time data synchronization, reduce data\n\
        \retrieval times by 10%, and enhance cross-platform connectivity for cloud-native applications."
      ]
      ["AWS", "Python", "Docker", "Kubernetes"]
      [ "Improved implementation of automated monitoring system"
      , "Developed hands-on expertise in Kubernetes by assisting with container orchestration tasks,\n\
       \ streamlining deployment pipelines, and automating infrastructure provisioning"
      ]
      Nothing
  , Experience 2 "Dell Technologies" "Software Engineering Mentee" "Hopkinton, MA"
      (fromGregorian 2024 1 8) (Just $ fromGregorian 2024 4 17)
      [ "Optimized cloud infrastructure by leveraging Dell Technologies APEX Private Cloud, enabling 15% faster deployment \n\
      \times for virtualized environments and improving system scalability"
      , "Built custom API integrations and Python scripts to streamline cloud resource provisioning, resulting in a 10% increase in \n\
        \ operational efficiency"
      ]
      ["Python", "Dell APEX Private Cloud", "Terraform"]
      [ "Contributed to the automation Dell Cloud Platform data pipleline"
      ]
      Nothing
  , Experience 3 "NUCAR Lab w/ David Kaeli" "Machine Learning & High Performance Computing Researcher" "Boston, MA"
      (fromGregorian 2024 5 28) Nothing
      [ "Conducting research on high-performance computing and ML optimization"
      , "Developing CUDA kernels for accelerated deep learning workloads"
      , "Publishing research papers on distributed RAG retrieval"
      , "Currently researching Sparsity & optimizing sparse matrix operations."
      ]
      ["CUDA", "PyTorch", "C++", "Python", "Slurm"]
      [ "Built multiple RAG full-stack applications for multiple organizations including PROTECT, NIH, NIEHS..."
      , "Published findings to MIT IEEE"
      , "Currently working on NvBit Compiler"
      ]
      Nothing
      , Experience 4 "IDEA: Northeastern's Interdisciplinary Entrepreneurship Venture Accelerator" "Chief Operating Officer" "Boston, MA"
      (fromGregorian 2025 1 6) Nothing
       [ "Leading team of 30+ students across Analytics, Venture and Operations to run VA operations."
       , "Orchestrating website, mobile application and event organization platform for the organization."
       , "Managing data for 2800+ lifetime student ventures inclduing Slate, Amino, Mavrk who've raised  $900M+"
       ]
       ["Python", "Typescript", "Leadership", "Salesforce"]
    
       [
        "Led the complete construction of IDEA software pipeline"
      , "Contributed to revamping venture accelerator curriculum and strategy"
       ]
      -- []
  

-- Sample education data  
sampleEducation :: [Education]
sampleEducation =
  [ Education 1 "Northeastern University" "Bachelor of Science" "Computer Science & Physics"
      (fromGregorian 2027 5 15)
      ["Putnam Club", "Inter-Disciplinary Entrepreneurship Accelerator (IDEA) Director of Analytics 2024/2025 | Chief Operations Officer (COO) 2025/2026"
       , "rev.school"]
      ["Dean's List"]
      [ "Algorithms (Graduate)", "Intensive Mathematical Reasoning", "Object Oriented Design", "Computer Systems"
      , "Programming Languages" "Advanced Quantum Mechanics", "Advanced Linear Algebra", "Logic & Computation"
      , "Quantum Computing & Hardware Platforms" ]
      Nothing
  ]

-- Sample skills data
sampleSkills :: [Skill]  
sampleSkills =
  [ Skill "Python" Expert Programming (Just 7)
  , Skill "C++" Expert Programming (Just 6)
  , Skill "C" Expert Programming (Just 5)
  , Skill "PyTorch" Expert MachineLearning (Just 4)
  , Skill "CUDA" Advanced Programming (Just 2)
  , Skill "TensorFlow" Advanced MachineLearning (Just 4)
  , Skill "AWS" Advanced Cloud (Just 3)
  , Skill "Google Cloud Platform" Advanced Cloud (Just 2)
  , Skill "JavaScript/TypeScript" Advanced Programming (Just 5)
  , Skill "React" Advanced Framework (Just 4)
  , Skill "Haskell" Intermediate Programming (Just 1)
  , Skill "Rust" Intermediate Programming (Just 2)
  , Skill "Docker" Intermediate Tool (Just 2)
  , Skill "Kubernetes" Intermediate Tool (Just 2)
  , Skill "HIP/ROCm" Beginner Programming (Just 0)
  , Skill "OCaml" Beginner Programming (Just 0)
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
    generateExperiences sampleExperiences sampleEducation sampleSkills 

  -- Generate CSS with all enhanced styles
  writeFileUtf8 "dist/css/style.css" renderCSS

  putStrLn "Enhanced website generated successfully in dist/ directory!"


main :: IO ()
main = generateSite
