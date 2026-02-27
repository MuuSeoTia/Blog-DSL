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

-- experience data
sampleExperiences :: [Experience]
sampleExperiences =
  [ Experience 1 "Amazon" "Cloud Infrastructure Intern" "Boston, MA"
      (fromGregorian 2023 9 4) (Just $ fromGregorian 2023 12 20)
      [ "Optimized data flow across a distributed system managing thousands of database endpoints, reducing connection latency by 15% through targeted AWS Direct Connect configuration and vector database integration"
      , "Designed and implemented secure, scalable API endpoints backed by optimized database architecture, enabling 10% faster query execution across cloud-native applications"
      , "Built a real-time data synchronization pipeline using AWS Amplify and vector databases, improving cross-platform data retrieval times and enabling seamless multi-region data access"
      ]
      ["AWS", "Python", "Docker", "Kubernetes"]
      [ "Automated monitoring and alerting infrastructure for distributed database systems"
      , "Developed container orchestration workflows that streamlined deployment pipelines and reduced manual provisioning overhead"
      ]
      Nothing
  , Experience 2 "Dell Technologies" "Software Engineering Mentee" "Hopkinton, MA"
      (fromGregorian 2024 1 8) (Just $ fromGregorian 2024 4 17)
      [ "Leveraged Dell APEX Private Cloud to optimize virtualized environment deployments, achieving 15% faster provisioning times and improved infrastructure scalability"
      , "Built custom API integrations and Python automation scripts for cloud resource management, driving a 10% increase in operational efficiency across the platform"
      ]
      ["Python", "Dell APEX Private Cloud", "Terraform"]
      [ "Contributed to the automation of Dell's cloud platform data pipeline, reducing manual intervention in resource allocation"
      ]
      Nothing
  , Experience 3 "NUCAR Lab — Prof. David Kaeli" "ML & HPC Researcher" "Boston, MA"
      (fromGregorian 2024 5 28) Nothing
      [ "Conducting research on high-performance computing optimization, focusing on sparse matrix operations and GPU-accelerated deep learning workloads using CUDA"
      , "Developing custom CUDA kernels for accelerated neural network training and inference, with a focus on exploiting sparsity patterns in NVIDIA architectures"
      , "Published distributed RAG retrieval research at MIT IEEE, demonstrating improved retrieval accuracy for multi-source knowledge bases"
      , "Building compiler-level instrumentation tools using NvBit for dynamic binary analysis of GPU workloads"
      ]
      ["CUDA", "PyTorch", "C++", "Python", "Slurm", "NvBit"]
      [ "Built production RAG systems for multiple organizations including PROTECT, NIH, and NIEHS"
      , "Published findings at MIT IEEE URTC 2024"
      , "Actively developing NvBit-based compiler for GPU workload profiling"
      ]
      Nothing
  , Experience 4 "IDEA Venture Accelerator" "Chief Operating Officer" "Boston, MA"
      (fromGregorian 2025 1 6) Nothing
      [ "Leading a cross-functional team of 30+ students across Analytics, Venture, and Operations to manage accelerator programs and organizational infrastructure"
      , "Architecting and maintaining the organization's software ecosystem including website, mobile application, and event management platform"
      , "Managing data systems tracking 2,800+ lifetime student ventures, including companies like Slate, Amino, and Mavrk that have collectively raised over $900M"
      ]
      ["Python", "TypeScript", "Salesforce", "Leadership"]
      [ "Led the end-to-end construction of IDEA's software pipeline from design through deployment"
      , "Contributed to revamping the venture accelerator curriculum and operational strategy"
      ]
      Nothing
  ]

-- education data
sampleEducation :: [Education]
sampleEducation =
  [ Education 1 "Northeastern University" "Bachelor of Science" "Computer Science & Physics"
      (fromGregorian 2027 5 15)
      [ "Putnam Club"
      , "IDEA — Director of Analytics 2024/2025, Chief Operating Officer 2025/2026"
      , "rev.school"
      ]
      ["Dean's List"]
      [ "Algorithms (Graduate)", "Intensive Mathematical Reasoning", "Object Oriented Design"
      , "Computer Systems", "Programming Languages", "Advanced Quantum Mechanics"
      , "Advanced Linear Algebra", "Logic & Computation", "Quantum Computing & Hardware Platforms"
      ]
      Nothing
  ]

-- projects data
sampleProjects :: [Project]
sampleProjects =
  [ Project 1 "Sustainable AF"
      "Full-stack web application for finding sustainable products and services with an interactive solar heat map and carbon footprint calculator."
      ["Google Cloud Platform", "React.js", "TypeScript", "Python", "Machine Learning"]
      (Just "https://github.com/MuuSeoTia/carbon-advisor")
      (Just "https://www.youtube.com/watch?v=-a0d_5INf8Q&t=1s&ab_channel=FahadFaruqi")
      Nothing
      Nothing
  , Project 2 "Maritime"
      "Mobile application for tracking live microplastic concentrations in the ocean with CNN-powered waste identification from camera input."
      ["Python", "Flask", "Expo", "TypeScript", "CNN"]
      (Just "https://github.com/Zapaway/maritime")
      (Just "https://youtu.be/Dj94vM3d93Q?si=ZK42k89Ask6-Or8u")
      Nothing
      Nothing
  , Project 3 "OracleNet"
      "Global knowledge graph that takes hypothetical news headlines and visualizes all plausible cascading impacts on the economy, industries, supply chains, and geopolitics."
      ["Python", "Flask", "Graph Neural Network", "TypeScript"]
      (Just "https://github.com/ChittebbayiPenugonda/MacroNet")
      (Just "https://youtu.be/87r524Pl718?si=Im06aQ0B5mpAKgJe")
      Nothing
      Nothing
  , Project 4 "NSightful"
      "Real-time 3D visualization tool for GPU utilization monitoring, built with Rust and Tauri for native performance."
      ["JavaScript", "Rust", "HTML", "CSS", "Tauri"]
      (Just "https://github.com/MuuSeoTia/NSightful")
      Nothing
      Nothing
      Nothing
  , Project 5 "Griddy"
      "Smart grid power management and optimization system for real-time energy distribution and load balancing."
      ["TypeScript", "Python", "C", "CSS", "WebSocket"]
      (Just "https://github.com/Tetraslam/hackmit25")
      (Just "https://plume.hackmit.org/project/kmips-kavlf-auvtc-ymtmn")
      Nothing
      (Just "HackMIT 2025 Winner")
  ]

-- skills data
sampleSkills :: [Skill]
sampleSkills =
  [ Skill "Python" Expert Programming (Just 7)
  , Skill "C++" Expert Programming (Just 6)
  , Skill "C" Expert Programming (Just 5)
  , Skill "JavaScript/TypeScript" Advanced Programming (Just 5)
  , Skill "Haskell" Intermediate Programming (Just 1)
  , Skill "Rust" Intermediate Programming (Just 2)
  , Skill "OCaml" Beginner Programming Nothing
  , Skill "CUDA" Advanced Programming (Just 2)
  , Skill "HIP/ROCm" Beginner Programming Nothing
  , Skill "PyTorch" Expert MachineLearning (Just 4)
  , Skill "TensorFlow" Advanced MachineLearning (Just 4)
  , Skill "AWS" Advanced Cloud (Just 3)
  , Skill "Google Cloud Platform" Advanced Cloud (Just 2)
  , Skill "React" Advanced Framework (Just 4)
  , Skill "Docker" Intermediate Tool (Just 2)
  , Skill "Kubernetes" Intermediate Tool (Just 2)
  ]

-- blog posts
samplePosts :: [BlogPost]
samplePosts =
  [ BlogPost 1 "Why Did You Code Your Personal Website in Haskell?"
      (UTCTime (fromGregorian 2025 1 27) 19)
      "A mysterious figure in a hoodie handed me a Haskell book. Here's what happened when I decided to build my entire personal site with it."
      ["haskell", "functional programming", "web"]
      [ HeaderContent "How it started"
      , TextContent "One day I was in the basement of Hayden Hall on campus and I was approached by a suspicious hooded figure who smelled like matcha powder. \
        \He came up to me ominously offering me a book of divine power: \"Learn Haskell by building a blog generator.\" It was intriguing to say the least but I took it in stride \
        \and challenged myself to learn Haskell over winter break."
      , HeaderContent "The Process"
      , TextContent "At first the extremely functional nature of Haskell took me aback. As somebody entrenched in low-level and often very verbose \
        \programming languages like C++, it changed my perception of programming languages entirely."
      , TextContent "A massive difference I found right away was how types worked in Haskell and how useful Type Classes are, specifically with multiple input polymorphism. \
        \Another notable difference was the explicit nature of said inputs compared to a language like C++ where type conversions are largely implicit. \
        \I'm currently still getting used to implementing more Functors into my code and utilizing abstractions rather than mutation — not just in Haskell but across all languages I write."
      , HeaderContent "What I Learned"
      , TextContent "Through this project, I gained hands-on experience with:"
      , BulletList
          [ "Building embedded DSLs in Haskell"
          , "Functional programming patterns and type-driven design"
          , "Static site generation from pure data types"
          , "CSS generation using the Clay EDSL"
          , "Lucid for type-safe HTML generation"
          ]
      , CodeBlock "haskell" "-- The core of my Blog DSL\ndata BlogElement = \n    TextContent Text\n  | HeaderContent Text\n  | Image { path :: Text, alt :: Text, caption :: Maybe Text }\n  | CodeBlock { language :: Text, codeContent :: Text }\n  | BulletList [Text]"
      , HeaderContent "What's next + The Mysterious Figure"
      , RichText
          [ Plain "My next steps are adding a Markdown parser for better formatting, expanding the CSS EDSL, and building out the Projects page. Also — the mysterious figure was my good friend "
          , Link "Lucas" "https://lucasdamianjohnson.com"
          , Plain ". Please make sure to endorse him in \"Rizz\" when you have the opportunity."
          ]
      ]
  , BlogPost 2 "Q&A 1"
      (UTCTime (fromGregorian 2025 1 27) 21)
      "Answers to questions about AI taking CS jobs, whether quantum computing is real, and how to get cracked."
      ["career", "quantum", "opinions"]
      [ HeaderContent "Q: Is AI taking CS jobs?"
      , TextContent "For individuals who have poor code structure, lack diversity of applicable skills, and are pure DS majors — yes. Otherwise you're fine."
      , HeaderContent "Q: Is Quantum Computing even real?"
      , TextContent "Ask yourself: What does it mean for something to be real? Do you need to see it? Or simply believe in it? Is it something that is proven? \
        \If you're looking for an opinion from me then the answer is yes. (I know nothing about philosophy)"
      , HeaderContent "Q: How to get \"cracked\"?"
      , TextContent "Don't."
      , HeaderContent "Q: TypeScript?"
      , TextContent "A bad day writing code in C++ is better than a good day writing code in TypeScript — David Stigant (Surely)"
      ]
  , BlogPost 3 "Five Hackathons, Five Wins: What I Learned"
      (UTCTime (fromGregorian 2025 3 15) 14)
      "From building sustainability tools at HackHarvard to winning HackMIT — reflections on what makes a hackathon project win."
      ["hackathons", "engineering", "lessons"]
      [ HeaderContent "The Track Record"
      , TextContent "Over the past two years I've competed in five major hackathons and managed to win all five. \
        \Not because I'm some genius — but because I've learned what matters in these compressed 24-48 hour sprints. \
        \Here's the breakdown of each and what I took away."
      , HeaderContent "HackHarvard — Sustainable AF"
      , RichText
          [ Plain "Our first big win. We built a "
          , Link "web app" "https://github.com/MuuSeoTia/carbon-advisor"
          , Plain " that helps users find sustainable products and services in their area, \
            \complete with a solar heat map and carbon footprint calculator. The key insight was that judges care about real-world impact. \
            \We didn't just demo a tech stack — we showed how it would change user behavior. Google Cloud Platform, React, TypeScript, Python, \
            \and ML models for the sustainability scoring."
          ]
      , HeaderContent "Maritime — Tracking Ocean Microplastics"
      , RichText
          [ Plain "A "
          , Link "mobile app" "https://github.com/Zapaway/maritime"
          , Plain " for tracking live microplastic concentrations in the ocean. We trained a CNN to identify waste types from camera input. \
            \The lesson here was scope. We scoped it tight — one thing done really well — and the demo was polished. \
            \Built with Flask, Expo, TypeScript, and a custom CNN model."
          ]
      , HeaderContent "OracleNet — Global Knowledge Graph"
      , RichText
          [ Plain "This one was ambitious. We built a "
          , Link "global knowledge graph" "https://github.com/ChittebbayiPenugonda/MacroNet"
          , Plain " that takes hypothetical news headlines and shows all plausible cascading impacts \
            \on the economy, industries, supply chains, and geopolitics. Graph Neural Networks were the backbone. \
            \What won us this one was the vision — the judges could see where this could go."
          ]
      , HeaderContent "HackMIT 2025 — Griddy"
      , RichText
          [ Link "Griddy" "https://github.com/Tetraslam/hackmit25"
          , Plain " was a smart grid power management system. Real-time energy distribution, load balancing, \
            \optimization algorithms running over WebSockets. We won this with pure execution — the demo worked flawlessly, \
            \the system handled live load, and we explained the engineering decisions clearly. \
            \TypeScript, Python, C, and WebSocket for real-time communication."
          ]
      , HeaderContent "The Pitchathon — Beanpot Trophy"
      , Image "../images/pitchathon.jpg" "Holding the Beanpot trophy at Pitchathon 2025" (Just "Winning the Beanpot at Northeastern's Pitchathon 2025")
      , TextContent "This was different from a traditional hackathon — more pitch-focused. \
        \But the core lessons applied: know your audience, demonstrate impact, and be authentic about what you built and why."
      , HeaderContent "What Actually Wins Hackathons"
      , TextContent "After five wins, the pattern is clear:"
      , BulletList
          [ "Scope ruthlessly. Build one thing that works perfectly rather than three things that kind of work."
          , "The demo is everything. If it doesn't work live, it doesn't matter how good the code is."
          , "Tell a story. Judges remember narratives, not feature lists."
          , "Pick a real problem. Toy projects don't win — show genuine impact."
          , "Team chemistry matters. My best projects came from teams where everyone knew their role and trusted each other."
          ]
      , TextContent "I'm grateful for every team I've worked with. These wins belong to all of us."
      ]
  ]

-- copy images
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

-- generate static site
generateSite :: IO ()
generateSite = do
  createDirectoryIfMissing True "dist"
  createDirectoryIfMissing True "dist/posts"
  createDirectoryIfMissing True "dist/css"
  createDirectoryIfMissing True "dist/images"

  copyImages

  mapM_ (\post -> do
    let postPath = "dist/posts/" <> show (postId post) <> ".html"
    writeFileUtf8 postPath $ renderText $ renderBlogPost post
    ) samplePosts

  writeFileUtf8 "dist/index.html" $ renderText $ generateIndex samplePosts
  writeFileUtf8 "dist/about.html" $ renderText generateAbout
  writeFileUtf8 "dist/projects.html" $ renderText $ generateProjectsFromData sampleProjects
  writeFileUtf8 "dist/experience.html" $ renderText $
    generateExperiences sampleExperiences sampleEducation sampleSkills

  writeFileUtf8 "dist/css/style.css" renderCSS

  putStrLn "Site generated in dist/"

main :: IO ()
main = generateSite
