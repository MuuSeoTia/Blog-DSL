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
      Nothing
  ]

-- skills data
sampleSkills :: [Skill]
sampleSkills =
  -- programming
  [ Skill "Python" Expert Programming (Just 7)
  , Skill "C++" Expert Programming (Just 6)
  , Skill "C" Expert Programming (Just 5)
  , Skill "CUDA" Advanced Programming (Just 2)
  , Skill "JavaScript/TypeScript" Advanced Programming (Just 5)
  , Skill "Haskell" Intermediate Programming (Just 1)
  , Skill "Rust" Intermediate Programming (Just 2)
  , Skill "OCaml" Beginner Programming Nothing
  , Skill "HIP/ROCm" Beginner Programming Nothing
  -- ml & rl
  , Skill "PyTorch" Expert MachineLearning (Just 4)
  , Skill "JAX" Advanced MachineLearning (Just 1)
  , Skill "TensorFlow" Advanced MachineLearning (Just 4)
  , Skill "Tunix" Intermediate MachineLearning Nothing
  , Skill "TRL" Intermediate MachineLearning Nothing
  , Skill "OpenRLHF" Learning MachineLearning Nothing
  , Skill "DeepSpeed" Learning MachineLearning Nothing
  -- hpc & systems
  , Skill "Slurm" Advanced HPC (Just 2)
  , Skill "MPI" Intermediate HPC (Just 2)
  , Skill "OpenMP" Intermediate HPC (Just 2)
  , Skill "Pallas" Learning HPC Nothing
  , Skill "NCCL" Learning HPC Nothing
  , Skill "NvBit" Intermediate HPC (Just 1)
  -- cloud & infra
  , Skill "AWS" Advanced Cloud (Just 3)
  , Skill "Google Cloud Platform" Advanced Cloud (Just 2)
  -- frameworks & tools
  , Skill "React" Advanced Framework (Just 4)
  , Skill "MaxText" Learning Framework Nothing
  , Skill "Unix/Linux" Expert Tool (Just 7)
  , Skill "Git" Advanced Tool (Just 5)
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
      "I've won every hackathon I've entered. Here's the honest breakdown of why — and the framework I accidentally built along the way."
      ["hackathons", "engineering", "lessons"]
      [ HeaderContent "How it started"
      , TextContent "I didn't go into my first hackathon planning to win. I went because a friend dragged me there \
        \and I figured free pizza and a weekend of coding sounded better than whatever else I had going on. \
        \We ended up winning. Then it happened again. And again. Five times total now."
      , TextContent "People keep asking me what the trick is. There is no trick. But there is a pattern, \
        \and after five of these things I think I've finally figured out what it actually looks like."
      , HeaderContent "The Framework (that I didn't mean to build)"
      , TextContent "Every winning project I've been part of nailed four things. I drew this out on a napkin at 3am \
        \during HackMIT and it's been stuck in my head ever since:"
      , CodeBlock "" "          +-------------------+\n          |   TECHNICAL DEPTH |\n          | (Can you build it) |\n          +--------+----------+\n                   |\n    +--------------+---------------+\n    |              |               |\n    v              v               v\n+--------+   +-----------+   +--------+\n| SCOPE  |   |   PITCH   |   | DEMO   |\n| (Less  |   | (Tell the |   | (Make  |\n|  is    |   |  story)   |   |  it    |\n|  more) |   +-----------+   |  work) |\n+--------+                   +--------+\n                   |\n          +--------+----------+\n          |    X-FACTOR       |\n          | (Why should they  |\n          |  care RIGHT NOW)  |\n          +-------------------+"
      , TextContent "Let me break each of these down with actual examples."
      , HeaderContent "Technical Depth — Can you actually build it?"
      , TextContent "This is the foundation. If your team can't execute, nothing else matters. \
        \But here's the thing — technical depth doesn't mean complexity. It means competence. \
        \Judges can tell the difference between a team that chose the right tool for the job \
        \and a team that threw every buzzword at the wall."
      , RichText
          [ Plain "When we built "
          , Link "Sustainable AF" "https://github.com/MuuSeoTia/carbon-advisor"
          , Plain " at HackHarvard, we used GCP, React, TypeScript, Python, and ML models. \
            \That sounds like a lot but every piece had a clear reason to exist. The ML model powered the sustainability scoring. \
            \GCP handled the solar heat map data. React made the frontend interactive. Nothing was there for show."
          ]
      , HeaderContent "Scope — Less is more (seriously)"
      , RichText
          [ Plain "This is the one everyone gets wrong. You have 24-48 hours. You are not building a startup. \
            \You are building a proof of concept that works flawlessly for a 3-minute demo. "
          , Link "Maritime" "https://github.com/Zapaway/maritime"
          , Plain " won because we did one thing — track ocean microplastics and identify waste with a CNN — \
            \and we did it really well. The app loaded fast, the model was accurate, the UX was clean. \
            \We could have added ten more features but we didn't."
          ]
      , TextContent "I've seen teams with better ideas lose because they tried to build too much and their demo crashed. \
        \Scope ruthlessly. Kill your darlings. Ship the thing that works."
      , HeaderContent "Pitch — Tell the story"
      , RichText
          [ Plain "Judges don't remember feature lists. They remember stories. When we presented "
          , Link "OracleNet" "https://github.com/ChittebbayiPenugonda/MacroNet"
          , Plain " — a global knowledge graph that shows cascading impacts of hypothetical events — \
            \we didn't lead with \"we used Graph Neural Networks.\" We led with \"what happens to the global economy \
            \if a major shipping route gets blocked tomorrow?\" Then we showed it. The GNN was the how, not the what."
          ]
      , TextContent "Structure your pitch like a story: problem, why it matters, what you built, how it works, what's next. \
        \In that order. Every time."
      , HeaderContent "Demo — Make it work. Period."
      , RichText
          [ Link "Griddy" "https://github.com/Tetraslam/hackmit25"
          , Plain " at HackMIT was our cleanest execution. Smart grid power management with real-time load balancing \
            \over WebSockets. When we demoed, the system handled live load without a hiccup. \
            \The judges could see the data flowing. They could interact with it. \
            \Nothing was mocked, nothing was hardcoded."
          ]
      , TextContent "If your demo doesn't work live, none of the above matters. \
        \Spend the last two hours before judging just testing. Then test again. Then one more time."
      , HeaderContent "The X-Factor — Why should they care RIGHT NOW?"
      , TextContent "This is the secret fifth element. Every winning project we built addressed something timely. \
        \Sustainability when climate legislation was in the news. Ocean microplastics when there was a viral documentary about it. \
        \Smart grid optimization when energy costs were spiking. It's not manipulation — it's relevance."
      , TextContent "If your project solves a problem that's currently in the cultural conversation, \
        \judges connect with it on a gut level before you even explain the technical details."
      , HeaderContent "One more thing"
      , TextContent "None of this works without the right team. My best projects came from teams where everyone knew their role \
        \and trusted each other. No ego, no politics, just building. \
        \I'm grateful for every person I've stayed up until 4am with over stale Red Bull and questionable WiFi. \
        \These wins belong to all of us."
      , TextContent "If you're reading this before a hackathon — good luck. Scope small, demo hard, tell a story."
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
