{-# LANGUAGE OverloadedStrings #-}

module Pages where

import BlogDSL
import Data.Time (UTCTime(..), fromGregorian, Day)
import Data.Text (Text, pack)
import Lucid
import qualified Data.Text.Lazy as TL

-- Generate experiences page using the DSL
generateExperiences :: [Experience] -> [Education] -> [Skill]  -> Html ()
generateExperiences experiences education skills = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ "Experience - Mouad Tiahi"
    link_ [rel_ "stylesheet", type_ "text/css", href_ "css/style.css"]
  body_ $ do
    div_ [class_ "container"] $ do
      nav_ [class_ "nav"] $ do
        a_ [href_ "index.html"] "Home"
        a_ [href_ "about.html"] "About"
        a_ [href_ "projects.html"] "Projects"
        a_ [href_ "experience.html"] "Experience"
      main_ [] $ do
        div_ [class_ "experience-section"] $ do
          -- Professional Experience Section
          div_ [class_ "section-header"] $ do
            h1_ [class_ "section-title"] "Professional Experience"
            p_ [class_ "section-subtitle"] "My journey through internships and research positions"
          
          div_ [class_ "experiences-container"] $
            mapM_ (toBlogHtml) experiences
          
          -- Education Section
          div_ [class_ "section-header"] $ do
            h2_ [class_ "section-title"] "Education"
            p_ [class_ "section-subtitle"] "Academic foundation and achievements"
          
          div_ [class_ "education-container"] $
            mapM_ (toBlogHtml) education
          
          -- Skills Section  
          div_ [class_ "section-header"] $ do
            h2_ [class_ "section-title"] "Technical Skills"
            p_ [class_ "section-subtitle"] "Technologies and tools I work with"
          
          div_ [class_ "skills-section"] $ do
            div_ [class_ "skills-grid"] $
              mapM_ (toBlogHtml) skills
          
          -- Certifications Section
          --div_ [class_ "section-header"] $ do
          -- h2_ [class_ "section-title"] "Certifications"
          --  p_ [class_ "section-subtitle"] "Professional certifications and credentials"
          
          -- div_ [class_ "certificates-container"] $
           -- mapM_ (toBlogHtml) certificates
            
      footer_ [class_ "footer"] $ do
        p_ [] "Generated in Haskell"

-- Generate about page 
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
        a_ [href_ "experience.html"] "Experience"
      main_ [] $ do
        div_ [class_ "about-section"] $ do
          h1_ "About Me"
          img_ [class_ "profile-img", src_ "images/beanpot.jpg", alt_ "Mouad Tiahi"]
          p_ "Computer Science & Physics Major with a Minor in Mathematics, Prev Cloud Intern @ Amazon and Software Engineering Intern @ Dell Technologies" 
          p_ "I specialize in:"
          ul_ $ do
            li_ "Machine Learning & Deep Learning (Regression, Classification, Computer Vision, NLP, Reinforcement Learning)"
            li_ "High Performance Computing (CUDA, Metal, Clustering, Slurm, Dynamic Binary Instrumentation)"
            li_ "Cloud Computing (Google Cloud Platform, AWS, Microsoft Azure, Terraform, Hashicorp Lang)"
      footer_ [class_ "footer"] $ do
        p_ [] "Generated in Haskell"

-- Generate projects page
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
        a_ [href_ "experience.html"] "Experience"
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

          -- project 3 placeholder  
          div_ [class_ "project-card"] $ do
            h2_[class_ "project-title"] "Compiler Project"
            p_ [class_ "project-description"] "Currently developing a compiler that translates code into 3D objects for visualization purposes."
          
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

-- Generate index page
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
        a_ [href_ "experience.html"] "Experience"
      main_ [] $ do
        div_ [class_ "hero-section"] $ do
          img_ [class_ "profile-img", src_ "images/photoshootneut.jpg", alt_ "Mouad Tiahi"]
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