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
  [ Experience 1 "OsmosisAI (YC W25)" "GPU Infrastructure Engineer" "Remote"
      (fromGregorian 2026 2 1) Nothing
      [ "Developing custom Triton kernels for fused linear cross entropy, optimizing rollout throughput for large-scale LLM post-training pipelines"
      , "Engineering GPU parallelism infrastructure for efficient distributed training and inference across multi-node clusters"
      ]
      ["Triton", "CUDA", "Python", "PyTorch"]
      []
      Nothing
      (Just "https://osmosis.ai")
  , Experience 2 "IDEA Venture Accelerator" "Chief Operating Officer" "Boston, MA"
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
      Nothing
  , Experience 4 "Dell Technologies" "Software Engineering Mentee" "Hopkinton, MA"
      (fromGregorian 2024 1 8) (Just $ fromGregorian 2024 4 17)
      [ "Leveraged Dell APEX Private Cloud to optimize virtualized environment deployments, achieving 15% faster provisioning times and improved infrastructure scalability"
      , "Built custom API integrations and Python automation scripts for cloud resource management, driving a 10% increase in operational efficiency across the platform"
      ]
      ["Python", "Dell APEX Private Cloud", "Terraform"]
      [ "Contributed to the automation of Dell's cloud platform data pipeline, reducing manual intervention in resource allocation"
      ]
      Nothing
      (Just "https://www.delltechnologies.com")
  , Experience 5 "Amazon" "Cloud Infrastructure Intern" "Boston, MA"
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
      (Just "https://www.amazon.com")
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
  , Skill "Triton" Intermediate Programming Nothing
  , Skill "Java" Advanced Programming (Just 4)
  , Skill "JavaScript/TypeScript" Advanced Programming (Just 5)
  , Skill "Haskell" Intermediate Programming (Just 1)
  , Skill "Rust" Intermediate Programming (Just 2)
  , Skill "OCaml" Beginner Programming Nothing
  , Skill "HIP/ROCm" Beginner Programming Nothing
  -- ml & rl
  , Skill "PyTorch" Expert MachineLearning (Just 4)
  , Skill "JAX" Advanced MachineLearning (Just 1)
  , Skill "TensorFlow" Advanced MachineLearning (Just 4)
  , Skill "TRL" Intermediate MachineLearning Nothing
  , Skill "Tunix" Learning MachineLearning Nothing
  , Skill "OpenRLHF" Learning MachineLearning Nothing
  , Skill "DeepSpeed" Learning MachineLearning Nothing
  -- hpc & systems
  , Skill "Slurm" Advanced HPC (Just 2)
  , Skill "MPI" Intermediate HPC (Just 2)
  , Skill "OpenMP" Intermediate HPC (Just 2)
  , Skill "NvBit" Intermediate HPC (Just 1)
  , Skill "Pallas" Learning HPC Nothing
  , Skill "NCCL" Learning HPC Nothing
  , Skill "Tokamax" Learning HPC Nothing
  , Skill "XLA" Learning HPC Nothing
  -- cloud & infra
  , Skill "AWS" Advanced Cloud (Just 3)
  , Skill "Google Cloud Platform" Advanced Cloud (Just 2)
  -- frameworks & tools
  , Skill "React" Advanced Framework (Just 4)
  , Skill "MaxText" Learning Framework Nothing
  , Skill "Unix/Linux" Expert Tool (Just 7)
  , Skill "Git" Expert Tool (Just 5)
  , Skill "Docker" Advanced Tool (Just 2)
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
  , BlogPost 3 "On Hackathons"
      (UTCTime (fromGregorian 2025 10 11) 17)
      "From competitive programming prize money to MLH Top 50. Here's the hackathon meta I've figured out over five wins and dozens of placements."
      ["hackathons", "engineering", "lessons"]
      [ HeaderContent "How it began"
      , TextContent "I began to develop an interest in hackathons and competitive programming starting in late high school. \
        \At the time, my family didn't really have enough money to get by without me constantly working part time, \
        \so I would compete in algorithm-based competitions for both college applications and a form of income \
        \since I quickly began gaining traction and winning multiple state and regional level events."
      , TextContent "As with everything I usually do, I gradually grew tired of solving repetitive algorithms \
        \and distinct assigned tasks and wanted something more freeform and creative. I started attending 4-8 hour \
        \\"Programming Project\" competitions which completely invigorated me. Everyone around me seemed so ambitious \
        \and passionate about the projects they'd built, everything from a Chrome dinosaur game replica with a wallaby \
        \as a replacement, to an AP Chem interactive virtual lab. From then on I knew I wanted to compete at more \
        \hackathons and see what people would come up with."
      , HeaderContent "Succeeding in hackathons?"
      , RichText
          [ Plain "After winning five international level hackathons spanning thousands of competitors and placing in sponsor prizes \
            \and runner-up categories at dozens of others, I became the "
          , Link "first Northeastern student ranked in the MLH Top 50" "https://www.khoury.northeastern.edu/mouad-tiahi-started-coding-on-a-decades-old-computer-now-hes-a-2025-top-50-hacker/"
          , Plain ". I consider myself to be one of the most successful hackers still competing, and that is an honor \
            \I attribute mainly to the friends who joined me throughout my hackathon journey. I genuinely could not \
            \have accomplished anything without their support."
          ]
      , TextContent "As a result of this, many always ask me for assistance in hackathons, ideation, and judging. \
        \For the rest of this blog I'll be giving insight into the hackathon \"meta\" and how to win large-scale \
        \hackathons with 1000+ hackers. I'll cover this in order of most to least important: the idea, \
        \pitching and judging, project structure, and lastly team dynamics."
      , HeaderContent "Ideation"
      , TextContent "Many tech influencers and LinkedIn officers will likely tell you something along the lines of:"
      , CodeBlock "cpp" "#include <iostream>\n\nint main() {\n    std::cout << \"Solve a real world problem!\" << std::endl;\n    std::cout << \"Incorporate a billion AI Agents!\" << std::endl;\n    std::cout << \"Pump out dashboards!\" << std::endl;\n    std::cout << \"IMPORT <free_sponsor_api_key>\" << std::endl;\n}"
      , TextContent "Unfortunately, they are correct. This is typically the most effective tactic to win any prize \
        \at a smaller scale hackathon."
      , TextContent "However, if you actually want to win 1st place in track or general at the most competitive \
        \hackathons in the world, or if you want to build something with actual long-term impact and scale, \
        \this won't cut it."
      , TextContent "So how do we start? Throughout my hackathon experience, my teams and I have come up with ideas \
        \from as long as 4 months before, up to 14 hours into a hackathon. As a result, I've found the best way \
        \to get an idea is through the intersection of three main components: Problem, Solution, and Technicality, \
        \plus an additional component I call the X-Factor."
      , CodeBlock "" "      PROBLEM              SOLUTION\n    (Why does it         (What does the\n     matter?)             product do?)\n          \\                   /\n           \\                 /\n            +------+-------+\n            | WINNING IDEA |\n            +------+-------+\n           /                 \\\n          /                   \\\n    TECHNICALITY           X-FACTOR\n   (How do you           (Why right now?\n    execute it?)          What's the hook?)"
      , HeaderContent "The Problem"
      , TextContent "Defining the problem is mainly about deciding whether or not your idea is actually something \
        \worth solving. Judges at top hackathons have seen hundreds of projects. They can smell a forced problem \
        \statement from across the room. The problem needs to be real. Not \"wouldn't it be cool if\" but \
        \\"this genuinely affects people and here's why.\""
      , RichText
          [ Plain "When we built "
          , Link "Sustainable AF" "https://github.com/MuuSeoTia/carbon-advisor"
          , Plain " at HackHarvard, the problem was clear: consumers want to make sustainable choices but have no \
            \practical way to evaluate the environmental impact of their purchases. That's a real pain point. \
            \No stretching required."
          ]
      , HeaderContent "The Solution"
      , TextContent "Your solution needs to be specific and scoped. This is where most teams lose. They define a vague \
        \problem, then propose an equally vague solution. \"We're going to fix climate change\" is not a solution. \
        \\"We built an interactive solar heat map that shows you exactly how much energy your roof can generate\" is."
      , RichText
          [ Plain "With "
          , Link "Maritime" "https://github.com/Zapaway/maritime"
          , Plain " the solution was dead simple: point your camera at ocean waste, get an instant CNN classification, \
            \and see live microplastic concentration data for your location. One sentence. That's what you're aiming for."
          ]
      , HeaderContent "Technicality"
      , TextContent "This is where your engineering chops matter. The technicality isn't about using the most buzzwords. \
        \It's about choosing the right stack for the problem and executing it well. Judges can tell the difference \
        \between a team that understood their architecture and a team that copy-pasted a tutorial."
      , RichText
          [ Plain "For "
          , Link "OracleNet" "https://github.com/ChittebbayiPenugonda/MacroNet"
          , Plain " we used Graph Neural Networks to model cascading economic impacts. That sounds fancy, but it was \
            \the natural choice. You're modeling a network of dependencies, so a graph-based approach is the obvious \
            \architecture. Every technical decision should have a reason."
          ]
      , HeaderContent "The X-Factor"
      , TextContent "This is the secret sauce. The X-Factor is timing and relevance. Why should judges care about \
        \this problem right now? Sustainability when climate legislation is in the news. Ocean microplastics \
        \when a documentary goes viral. Smart grid optimization when energy costs are spiking. It's not manipulation. \
        \It's relevance. If your project solves a problem that's currently in the cultural conversation, \
        \judges connect with it on a gut level before you even open your mouth."
      , HeaderContent "Pitching & Judging"
      , TextContent "Once you have a winning idea, you need to sell it. I've seen technically superior projects lose \
        \to teams with half the engineering but twice the storytelling. Here's the structure that has never failed me:"
      , BulletList
          [ "Lead with the problem. Make them feel it."
          , "Show the solution. Demo first, explain second."
          , "Explain the technicality. Briefly, confidently."
          , "End with the X-Factor. Why this matters right now."
          ]
      , RichText
          [ Plain "Judges don't remember feature lists. They remember stories. When we presented "
          , Link "OracleNet" "https://github.com/ChittebbayiPenugonda/MacroNet"
          , Plain ", we didn't lead with \"we used Graph Neural Networks.\" We led with \"what happens to the global economy \
            \if a major shipping route gets blocked tomorrow?\" Then we showed it. The GNN was the how, not the what."
          ]
      , TextContent "Also, and I cannot stress this enough, if your demo doesn't work live, none of the above matters. \
        \Spend the last two hours before judging just testing. Then test again. Then one more time."
      , HeaderContent "Project Structure"
      , TextContent "Planning your project structure is about scope management. You have 24-48 hours. You are not \
        \building a startup. You are building a proof of concept that works flawlessly for a 3-minute demo."
      , RichText
          [ Plain "Here's what I do: in the first hour, whiteboard the entire system. Every API call, every component, \
            \every data flow. Assign ownership. Then cut 40% of it. Whatever you think you can build in 24 hours, \
            \you can build about 60% of it. Scope ruthlessly. Kill your darlings. Ship the thing that works. "
          , Link "Griddy" "https://github.com/Tetraslam/hackmit25"
          , Plain " at "
          , Link "HackMIT" "https://www.khoury.northeastern.edu/khoury-undergrads-win-three-categories-at-prestigious-mit-hackathon/"
          , Plain " was our cleanest execution because we planned it this way from the start."
          ]
      , HeaderContent "Your Team"
      , TextContent "None of this works without the right team. My best projects came from teams where everyone knew \
        \their role and trusted each other. No ego, no politics, just building."
      , TextContent "The most important thing isn't individual skill. It's complementary skill sets and mutual trust. \
        \You need someone who can build the backend, someone who can make the frontend not look terrible, and \
        \someone who can pitch. If one person can do two of those, you're golden."
      , TextContent "I'm grateful for every person I've stayed up until 4am with over stale Red Bull and questionable WiFi. \
        \If you're reading this before a hackathon, good luck. Scope small, demo hard, tell a story."
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
