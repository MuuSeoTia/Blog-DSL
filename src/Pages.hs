{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Pages where

import BlogDSL
import Data.Time (UTCTime(..), fromGregorian, Day)
import Data.Text (Text, pack)
import qualified Data.Text as T
import Lucid
import qualified Data.Text.Lazy as TL
import Control.Monad (unless)

-- page shell
pageShell :: Text -> Text -> Html () -> Html ()
pageShell pageTitle cssPath bodyContent = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml pageTitle)
    link_ [rel_ "stylesheet", type_ "text/css", href_ cssPath]
    link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
    link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Crimson+Pro:wght@400;500;600&family=Space+Grotesk:wght@400;500;600;700&display=swap"]
  body_ $ do
    div_ [class_ "site"] $ do
      a_ [class_ "skip-link", href_ "#main-content"] "Skip to content"
      siteNav cssPath
      main_ [class_ "content", id_ "main-content", role_ "main"] bodyContent
      siteFooter

-- nav
siteNav :: Text -> Html ()
siteNav cssPath = nav_ [class_ "nav", role_ "navigation", term "aria-label" "Main navigation"] $ do
  a_ [href_ (rel "index.html"), class_ "nav-name"] $ do
    span_ [class_ "full-name"] "Mouad Tiahi"
    span_ [class_ "short-name"] "Mouad"
  div_ [class_ "nav-links"] $ do
    a_ [href_ (rel "index.html")] "~/"
    a_ [href_ (rel "about.html")] "About"
    a_ [href_ (rel "projects.html")] "Projects"
    a_ [href_ (rel "experience.html")] "Experience"
  where
    rel p = if cssPath == "css/style.css" then p else "../" <> p

siteFooter :: Html ()
siteFooter = footer_ [class_ "footer", role_ "contentinfo"] $
  p_ [] $ do
    "Built with "
    a_ [href_ "https://github.com/MuuSeoTia/Blog-DSL", target_ "_blank"] "my Haskell DSL"

-- index page
generateIndex :: [BlogPost] -> Html ()
generateIndex posts = pageShell "Mouad Tiahi" "css/style.css" $ do
  div_ [class_ "site-header"] $ do
    div_ [class_ "header-row"] $ do
      img_ [class_ "profile-photo", src_ "images/photoshootneut.jpg", alt_ "Mouad Tiahi"]
      div_ [class_ "header-text"] $ do
        h1_ "Mouad Tiahi"
        p_ [class_ "site-tagline"] $ do
          "CS & Physics at Northeastern. GPU Infrastructure Engineer at "
          a_ [href_ "https://osmosis.ai", target_ "_blank"] "OsmosisAI"
          " (YC W25). ML & HPC researcher at "
          a_ [href_ "https://ece.northeastern.edu/groups/nucar/", target_ "_blank"] "NUCAR"
          ". Previously Amazon, Dell. 5x hackathon winner. "
          a_ [href_ "https://top.mlh.io/2025", target_ "_blank"] "MLH Top 50 2025"
          "."
    div_ [class_ "header-links"] $ do
      a_ [href_ "https://github.com/MuuSeoTia", target_ "_blank"] "GitHub"
      span_ [class_ "sep"] "/"
      a_ [href_ "https://www.linkedin.com/in/mouad-tiahi-0b361524b/", target_ "_blank"] "LinkedIn"
      span_ [class_ "sep"] "/"
      a_ [href_ "https://devpost.com/MuuSeoTia", target_ "_blank"] "Devpost"
      span_ [class_ "sep"] "/"
      a_ [href_ "https://github.com/MuuSeoTia/Blog-DSL", target_ "_blank"] "This Site's DSL"

  section_ [class_ "writing-section"] $ do
    h2_ [class_ "section-heading"] "Writing"
    div_ [class_ "post-list"] $
      mapM_ renderPostCard posts

-- blog card (no tags on index)
renderPostCard :: BlogPost -> Html ()
renderPostCard post = article_ [class_ "post-card"] $ do
  div_ [class_ "post-meta"] $ do
    span_ [class_ "post-date"] $ toHtml $ formatDate (date post)
  h3_ [class_ "post-card-title"] $
    a_ [href_ $ "posts/" <> pack (show $ postId post) <> ".html"] $
      toHtml $ title post
  p_ [class_ "post-excerpt"] $ toHtml $ excerpt post

-- about page
generateAbout :: Html ()
generateAbout = pageShell "About - Mouad Tiahi" "css/style.css" $ do
  h1_ "About"
  img_ [class_ "section-img", src_ "images/beanpot.jpg", alt_ "Mouad Tiahi"]

  p_ "I'm a Computer Science and Physics student at Northeastern University, graduating in 2027. I've interned at Amazon building cloud infrastructure and at Dell Technologies working on private cloud engineering. Currently, I work as a Machine Learning and High Performance Computing researcher at the NUCAR Lab under Professor David Kaeli."

  p_ "Outside of research, I serve as Chief Operating Officer of IDEA, Northeastern's venture accelerator, where I lead a team of 30+ students and manage software infrastructure supporting 2,800+ student ventures."

  p_ "My research focuses on high-performance computing optimization, particularly sparse matrix operations on GPU architectures using CUDA and compiler-level instrumentation with NvBit. I've published work at MIT IEEE on distributed RAG retrieval systems and SpMM kernel optimization, and I've given talks at multiple conferences. I've also worked on projects with the NIH, AMD, and NVIDIA."

  h2_ "Interests"
  p_ "Machine learning systems, GPU/TPU kernel engineering, compiler design, quantum computing, and the intersection of physics and computation. I also compete in hackathons, five wins so far."

  h2_ "Press"
  div_ [class_ "press-list"] $ do
    a_ [class_ "press-card", href_ "https://www.khoury.northeastern.edu/mouad-tiahi-started-coding-on-a-decades-old-computer-now-hes-a-2025-top-50-hacker/", target_ "_blank"] $ do
      img_ [class_ "press-thumb", src_ "https://s44427.pcdn.co/wp-content/uploads/2026/01/mouad-tiahi-Tiahi-trophy800x533.jpg", alt_ "Mouad Tiahi at The Beanpot of Ventures"]
      div_ [class_ "press-info"] $ do
        span_ [class_ "press-source"] "Khoury College"
        span_ [class_ "press-title"] "Mouad Tiahi Started Coding on a Decades-Old Computer. Now He's a 2025 Top 50 Hacker."
    a_ [class_ "press-card", href_ "https://www.khoury.northeastern.edu/khoury-undergrads-win-three-categories-at-prestigious-mit-hackathon/", target_ "_blank"] $ do
      img_ [class_ "press-thumb", src_ "https://s44427.pcdn.co/wp-content/uploads/2026/02/hackmit-Eyecraft-and-Griddy-together800x533.jpg", alt_ "Khoury teams at HackMIT"]
      div_ [class_ "press-info"] $ do
        span_ [class_ "press-source"] "Khoury College"
        span_ [class_ "press-title"] "Khoury Undergrads Win Three Categories at Prestigious MIT Hackathon"

  h2_ "Currently Learning"
  ul_ $ do
    li_ "Sparsity in NVIDIA architectures for accelerating inference"
    li_ "MXFP4 micro-scaling formats and low-precision arithmetic for NVIDIA Blackwell architectures"
    li_ "JAX, Pallas, and TPU kernel optimization"

-- experience page
generateExperiences :: [Experience] -> [Education] -> [Skill] -> Html ()
generateExperiences experiences education skills = pageShell "Experience - Mouad Tiahi" "css/style.css" $ do
  h1_ "Experience"
  img_ [class_ "section-img", src_ "images/pitchathon.jpg", alt_ "The Beanpot trophy at Pitchathon 2025"]

  h2_ [class_ "section-heading"] "Work & Research"
  div_ [class_ "entries"] $
    mapM_ toBlogHtml experiences

  h2_ [class_ "section-heading"] "Education"
  div_ [class_ "entries"] $
    mapM_ toBlogHtml education

  h2_ [class_ "section-heading"] "Skills"
  div_ [class_ "skill-legend"] $ do
    span_ [class_ "legend-item expert"] "Expert"
    span_ [class_ "legend-item advanced"] "Advanced"
    span_ [class_ "legend-item intermediate"] "Intermediate"
    span_ [class_ "legend-item learning"] "Learning"
    span_ [class_ "legend-item beginner"] "Beginner"
  div_ [class_ "skills-section"] $
    renderSkillsByCategory skills

-- skills grouped by category
renderSkillsByCategory :: [Skill] -> Html ()
renderSkillsByCategory skills = mapM_ renderGroup (groupByCategory skills)

renderGroup :: (SkillCategory, [Skill]) -> Html ()
renderGroup (cat, ss) = div_ [class_ "skill-group"] $ do
  h4_ [class_ "skill-category-name"] $ toHtml $ skillCategoryText cat
  div_ [class_ "skill-tags"] $
    mapM_ (\s -> span_ [class_ $ "skill-tag " <> skillLevelClass (proficiency s)]
      $ toHtml $ skillName s) ss

groupByCategory :: [Skill] -> [(SkillCategory, [Skill])]
groupByCategory [] = []
groupByCategory skills = foldr addSkill [] skills
  where
    addSkill s [] = [(category s, [s])]
    addSkill s ((cat, ss):rest)
      | category s == cat = (cat, s:ss) : rest
      | otherwise = (cat, ss) : addSkill s rest

-- projects page
generateProjectsFromData :: [Project] -> Html ()
generateProjectsFromData projects = pageShell "Projects - Mouad Tiahi" "css/style.css" $ do
  h1_ "Projects"
  img_ [class_ "section-img", src_ "images/conference.png", alt_ "MIT URTC Conference 2024"]

  section_ [class_ "projects-section"] $
    mapM_ toBlogHtml projects

  div_ [class_ "current-work"] $ do
    h2_ "Currently Working On"
    p_ [class_ "work-item"] "Sparse Matrix Compiler for CUDA, accelerating SpMV and SpMM on NVIDIA GPUs"
    p_ [class_ "work-item"] "MaxText-backed rollout engine for Tunix, scalable LLM post-training on TPU"
    p_ [class_ "work-item"] "Relaxing and taking it easier"

-- blog post page
renderBlogPost :: BlogPost -> Html ()
renderBlogPost post = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml $ title post)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "../css/style.css"]
    link_ [rel_ "preconnect", href_ "https://fonts.googleapis.com"]
    link_ [rel_ "preconnect", href_ "https://fonts.gstatic.com", crossorigin_ ""]
    link_ [rel_ "stylesheet", href_ "https://fonts.googleapis.com/css2?family=Crimson+Pro:wght@400;500;600&family=Space+Grotesk:wght@400;500;600;700&display=swap"]
    link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/themes/prism.min.css"]
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/components/prism-core.min.js"] ("" :: Text)
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/components/prism-haskell.min.js"] ("" :: Text)
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/plugins/autoloader/prism-autoloader.min.js"] ("" :: Text)
  body_ $ do
    div_ [class_ "site"] $ do
      a_ [class_ "skip-link", href_ "#main-content"] "Skip to content"
      siteNav "../css/style.css"
      main_ [class_ "content", id_ "main-content", role_ "main"] $ do
        article_ [class_ "blog-post"] $ do
          div_ [class_ "post-meta"] $ do
            span_ [class_ "post-date"] (toHtml $ formatDate $ date post)
          h1_ [] (toHtml $ title post)
          mapM_ toBlogHtml (content post)
      siteFooter
