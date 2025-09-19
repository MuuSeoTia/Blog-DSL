{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module BlogDSL where

import GHC.Generics
import Data.Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, Day)
import Lucid
import Data.Time.Format (formatTime, defaultTimeLocale)
import Control.Monad (unless)

-- | Blog element types
data BlogElement =
    TextContent Text
  | HeaderContent Text
  | Image
    { path :: Text
    , alt :: Text
    , caption :: Maybe Text
    }
  | CodeBlock
    { language :: Text
    , codeContent :: Text
    } deriving (Generic, Show)

-- | Experience-related data types
data Experience = Experience
  { expId :: Int
  , company :: Text
  , position :: Text
  , location :: Text
  , startDate :: Day
  , endDate :: Maybe Day
  , description :: [Text]
  , technologies :: [Text]
  , achievements :: [Text]
  , companyLogo :: Maybe Text
  } deriving (Generic, Show)

data Education = Education
  { eduId :: Int
  , institution :: Text
  , degree :: Text
  , field :: Text
  , graduationDate :: Day
  , activities :: [Text]
  , honors :: [Text]
  , relevantCourses :: [Text]
  , institutionLogo :: Maybe Text
  } deriving (Generic, Show)

data Skill = Skill
  { skillName :: Text
  , proficiency :: SkillLevel
  , category :: SkillCategory
  , yearsExperience :: Maybe Int
  } deriving (Generic, Show)

data SkillLevel = Beginner | Learning | Intermediate | Advanced | Expert
  deriving (Generic, Show, Eq)

data SkillCategory =
    Programming
  | Framework
  | Database
  | Cloud
  | Tool
  | MachineLearning
  | Other Text
  deriving (Generic, Show, Eq)

data Certificate = Certificate
  { certName :: Text
  , issuer :: Text
  , issueDate :: Day
  , expiryDate :: Maybe Day
  , credentialId :: Maybe Text
  , verificationUrl :: Maybe Text
  } deriving (Generic, Show)

-- blogpost type
data BlogPost = BlogPost
  { postId :: Int
  , title :: Text
  , date :: UTCTime
  , content :: [BlogElement]
  } deriving (Generic, Show)

-- define json instances
instance ToJSON BlogElement where
  toJSON (TextContent txt) = object
    [ "type" .= ("text" :: Text)
    , "content" .= txt
    ]
  toJSON (HeaderContent txt) = object
    [ "type" .= ("header" :: Text)
    , "content" .= txt
    ]
  toJSON (Image path' alt' cap) = object
    [ "type" .= ("image" :: Text)
    , "path" .= path'
    , "alt" .= alt'
    , "caption" .= cap
    ]
  toJSON (CodeBlock lang code) = object
    [ "type" .= ("code" :: Text)
    , "language" .= lang
    , "content" .= code
    ]

instance FromJSON BlogElement
instance ToJSON BlogPost
instance FromJSON BlogPost

-- JSON instances for new types
instance ToJSON Experience
instance FromJSON Experience
instance ToJSON Education
instance FromJSON Education
instance ToJSON Skill
instance FromJSON Skill
instance ToJSON SkillLevel
instance FromJSON SkillLevel
instance ToJSON SkillCategory
instance FromJSON SkillCategory
instance ToJSON Certificate
instance FromJSON Certificate

-- define basic HTML instances
instance ToHtml BlogElement where
  toHtml element = case element of
    TextContent text -> toHtml text
    HeaderContent text -> h2_ $ toHtml text
    Image path alt caption -> do
      img_ [src_ path, alt_ alt]
      case caption of
        Just cap -> p_ [class_ "caption"] $ toHtml cap
        Nothing -> pure ()
    CodeBlock lang code -> pre_ [class_ $ "language-" <> lang] $ code_ $ toHtml code
  toHtmlRaw :: Monad m => BlogElement -> HtmlT m ()
  toHtmlRaw = toHtml

instance ToHtml BlogPost where
  toHtml :: Monad m => BlogPost -> HtmlT m ()
  toHtml post = article_ [class_ "blog-post"] $ do
    h1_ [class_ "post-title"] $ toHtml (title post)
    p_ [class_ "post-date"] $ toHtml (show $ date post)
    mapM_ toHtml (content post)
  toHtmlRaw = toHtml

-- ToHtml instances for new types
instance ToHtml Experience where
  toHtml exp = div_ [class_ "experience-card"] $ do
    div_ [class_ "experience-header"] $ do
      maybe (pure ()) (\logo -> img_ [class_ "company-logo", src_ logo, alt_ $ company exp]) (companyLogo exp)
      div_ [class_ "experience-info"] $ do
        h3_ [class_ "position"] $ toHtml $ position exp
        h4_ [class_ "company"] $ toHtml $ company exp
        p_ [class_ "location"] $ toHtml $ location exp
        p_ [class_ "duration"] $ toHtml $ formatDateRange (startDate exp) (endDate exp)
  toHtmlRaw = toHtml

instance ToHtml Education where
  toHtml edu = div_ [class_ "education-card"] $ do
    h3_ [class_ "degree"] $ toHtml $ degree edu <> " in " <> field edu
    h4_ [class_ "institution"] $ toHtml $ institution edu
    p_ [class_ "graduation-date"] $ toHtml $ formatDay $ graduationDate edu
  toHtmlRaw = toHtml

instance ToHtml Skill where
  toHtml skill = div_ [class_ "skill-item"] $ do
    span_ [class_ "skill-name"] $ toHtml $ skillName skill
    span_ [class_ $ "skill-level " <> skillLevelClass (proficiency skill)] $
      toHtml $ skillLevelText (proficiency skill)
  toHtmlRaw = toHtml

instance ToHtml Certificate where
  toHtml cert = div_ [class_ "certificate-card"] $ do
    h4_ [class_ "cert-name"] $ toHtml $ certName cert
    p_ [class_ "cert-issuer"] $ toHtml $ "Issued by " <> issuer cert
    p_ [class_ "cert-date"] $ toHtml $ formatDay $ issueDate cert
  toHtmlRaw = toHtml

-- HTML generation
class ToHtml a => ToBlogHtml a where
  toBlogHtml :: a -> Html ()

instance ToBlogHtml BlogElement where
  toBlogHtml (TextContent txt) = p_ [] (toHtml txt)
  toBlogHtml (HeaderContent txt) = h2_ [] (toHtml txt)
  toBlogHtml (Image path alt caption) = figure_ [] $ do
    img_ [src_ path, alt_ alt]
    maybe (pure ()) (figcaption_ [] . toHtml) caption
  toBlogHtml (CodeBlock lang code) =
    pre_ [class_ $ "language-" <> lang] $
      code_ [class_ $ "language-" <> lang] (toHtml code)

instance ToBlogHtml BlogPost where
  toBlogHtml post = article_ [class_ "blog-post"] $ do
    h1_ [] (toHtml $ title post)
    div_ [class_ "post-date"] (toHtml $ formatDate $ date post)
    mapM_ toBlogHtml (content post)

-- HTML instances for Experience types
instance ToBlogHtml Experience where
  toBlogHtml exp = div_ [class_ "experience-card"] $ do
    div_ [class_ "experience-header"] $ do
      maybe (pure ()) (\logo -> img_ [class_ "company-logo", src_ logo, alt_ $ company exp]) (companyLogo exp)
      div_ [class_ "experience-info"] $ do
        h3_ [class_ "position"] $ toHtml $ position exp
        h4_ [class_ "company"] $ toHtml $ company exp
        p_ [class_ "location"] $ toHtml $ location exp
        p_ [class_ "duration"] $ toHtml $ formatDateRange (startDate exp) (endDate exp)

    div_ [class_ "experience-content"] $ do
      unless (null $ description exp) $ do
        div_ [class_ "description"] $
          mapM_ (p_ [] . toHtml) (description exp)

      unless (null $ achievements exp) $ do
        div_ [class_ "achievements"] $ do
          h5_ "Key Achievements"
          ul_ $ mapM_ (li_ . toHtml) (achievements exp)

      unless (null $ technologies exp) $ do
        div_ [class_ "technologies"] $ do
          h5_ "Technologies Used"
          div_ [class_ "tech-tags"] $
            mapM_ (span_ [class_ "tech-tag"] . toHtml) (technologies exp)

instance ToBlogHtml Education where
  toBlogHtml edu = div_ [class_ "education-card"] $ do
    div_ [class_ "education-header"] $ do
      maybe (pure ()) (\logo -> img_ [class_ "institution-logo", src_ logo, alt_ $ institution edu]) (institutionLogo edu)
      div_ [class_ "education-info"] $ do
        h3_ [class_ "degree"] $ toHtml $ degree edu <> " in " <> field edu
        h4_ [class_ "institution"] $ toHtml $ institution edu
        p_ [class_ "graduation-date"] $ toHtml $ formatDay $ graduationDate edu

    div_ [class_ "education-content"] $ do
      unless (null $ honors edu) $ do
        div_ [class_ "honors"] $ do
          h5_ "Honors & Awards"
          ul_ $ mapM_ (li_ . toHtml) (honors edu)
      unless (null $ activities edu) $ do
        div_ [class_ "activities"] $ do
          h5_ "Activities and Campus Involvement"
          ul_ $ mapM_ (li_ . toHtml) (activities edu)

      unless (null $ relevantCourses edu) $ do
        div_ [class_ "courses"] $ do
          h5_ "Relevant Coursework"
          div_ [class_ "course-tags"] $
            mapM_ (li_ . toHtml) (relevantCourses edu)

instance ToBlogHtml Skill where
  toBlogHtml skill = div_ [class_ "skill-item"] $ do
    div_ [class_ "skill-header"] $ do
      span_ [class_ "skill-name"] $ toHtml $ skillName skill
      span_ [class_ $ "skill-level " <> skillLevelClass (proficiency skill)] $
        toHtml $ skillLevelText (proficiency skill)
    div_ [class_ "skill-meta"] $ do
      span_ [class_ "skill-category"] $ toHtml $ skillCategoryText (category skill)
      maybe (pure ()) (\years -> span_ [class_ "skill-years"] $
        toHtml $ T.pack (show years) <> " years") (yearsExperience skill)

--instance ToBlogHtml Certificate where
  --toBlogHtml cert = div_ [class_ "certificate-card"] $ do
   -- h4_ [class_ "cert-name"] $ toHtml $ certName cert
   -- p_ [class_ "cert-issuer"] $ toHtml $ "Issued by " <> issuer cert
    --p_ [class_ "cert-date"] $ toHtml $ formatDay $ issueDate cert
    --maybe (pure ()) (\expiry -> p_ [class_ "cert-expiry"] $
    --  toHtml $ "Expires: " <> formatDay expiry) (expiryDate cert)
    -- maybe (pure ()) (\url -> a_ [href_ url, target_ "_blank", class_ "cert-verify"] "Verify Certificate") (verificationUrl cert)

-- Helper functions
formatDateRange :: Day -> Maybe Day -> Text
formatDateRange start Nothing = formatDay start <> " - Present"
formatDateRange start (Just end) = formatDay start <> " - " <> formatDay end

formatDay :: Day -> Text
formatDay = T.pack . formatTime defaultTimeLocale "%B %Y"

skillLevelText :: SkillLevel -> Text
skillLevelText Beginner = "Beginner"
skillLevelText Learning = "Learning"
skillLevelText Intermediate = "Intermediate"
skillLevelText Advanced = "Advanced"
skillLevelText Expert = "Expert"

skillLevelClass :: SkillLevel -> Text
skillLevelClass Beginner = "beginner"
skillLevelClass Learning = "learning"
skillLevelClass Intermediate = "intermediate"
skillLevelClass Advanced = "advanced"
skillLevelClass Expert = "expert"

skillCategoryText :: SkillCategory -> Text
skillCategoryText Programming = "Programming"
skillCategoryText Framework = "Framework"
skillCategoryText Database = "Database"
skillCategoryText Cloud = "Cloud"
skillCategoryText Tool = "Tool"
skillCategoryText MachineLearning = "Machine Learning"
skillCategoryText (Other txt) = txt

-- page generator
renderBlogPost :: BlogPost -> Html ()
renderBlogPost post = doctypehtml_ $ do
  head_ $ do
    meta_ [charset_ "utf-8"]
    meta_ [name_ "viewport", content_ "width=device-width, initial-scale=1"]
    title_ (toHtml $ title post)
    link_ [rel_ "stylesheet", type_ "text/css", href_ "../css/style.css"]
    link_ [rel_ "stylesheet", href_ "https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/themes/prism-tomorrow.min.css"]
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/components/prism-core.min.js"] ("" :: Text)
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/components/prism-haskell.min.js"] ("" :: Text)
    script_ [src_ "https://cdnjs.cloudflare.com/ajax/libs/prism/1.24.1/plugins/autoloader/prism-autoloader.min.js"] ("" :: Text)
  body_ $ do
    div_ [class_ "container"] $ do
      nav_ [class_ "nav"] $ do
        a_ [href_ "../index.html"] "Home"
        a_ [href_ "../about.html"] "About"
        a_ [href_ "../projects.html"] "Projects"
        a_ [href_ "../experience.html"] "Experience"
      main_ [] $ do
        article_ [class_ "blog-post"] $ do
          h1_ [] (toHtml $ title post)
          div_ [class_ "post-date"] (toHtml $ formatDate $ date post)
          mapM_ toBlogHtml (content post)
      footer_ [class_ "footer"] $ do
        p_ [] "Generated in Haskell"

-- date formatter
formatDate :: UTCTime -> String
formatDate = formatTime defaultTimeLocale "%B %e, %Y"