{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Clay hiding (span, map)
import qualified Clay.Elements as E
import Data.Text.Lazy (Text)
import qualified Clay.Render as R
import Prelude hiding (span)
import qualified Prelude as P

-- Main blog style
blogStyle :: Css
blogStyle = do
  globalStyles
  layoutStyles
  navStyles
  heroStyles
  socialStyles
  introStyles
  aboutStyles
  experienceStyles
  blogPostListStyles
  blogPostStyles
  contentStyles
  projectStyles
  techDotStyles
  projectButtonStyles
  footerStyles

-- Enhanced global styles with modern color scheme
globalStyles :: Css
globalStyles = do
  body ? do
    margin (px 0) (px 0) (px 0) (px 0)
    backgroundColor "#f8fafc"
    fontFamily ["Inter", "-apple-system", "BlinkMacSystemFont", "Segoe UI", "Roboto"] [sansSerif]
    lineHeight (unitless 1.7)
    color "#334155"
    minHeight (vh 100)
    display flex
    flexDirection column
    fontSize (px 16)

-- Enhanced layout styles
layoutStyles :: Css
layoutStyles = ".container" ? do
  maxWidth (px 1200)
  margin (px 0) auto (px 0) auto
  padding (px 20) (px 20) (px 20) (px 20)
  flexGrow 1
  width (pct 100)

-- Modern navigation with gradient
navStyles :: Css
navStyles = ".nav" ? do
  padding (px 25) (px 40) (px 25) (px 40)
  marginBottom (px 40)
  textAlign center
  backgroundColor white
  borderRadius (px 16) (px 16) (px 16) (px 16)
  border (px 1) solid "#e2e8f0"
  boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 4) (px 25)
  a ? navLinkStyle
  where
    navLinkStyle = do
      display inlineBlock
      marginRight (px 30)
      color "#475569"
      textDecoration none
      fontWeight (weight 500)
      padding (px 12) (px 24) (px 12) (px 24)
      borderRadius (px 12) (px 12) (px 12) (px 12)
      transition "all" 0.3 ease (sec 0)
      fontSize (px 15)
      hover & do
        color white
        backgroundColor "#3b82f6"
        boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 8) (px 25)
      lastChild & do
        marginRight (px 0)

-- Enhanced hero section with gradient
heroStyles :: Css
heroStyles = ".hero-section" ? do
  textAlign center
  marginBottom (px 60)
  padding (px 80) (px 40) (px 80) (px 40)
  backgroundColor white
  borderRadius (px 20) (px 20) (px 20) (px 20)
  border (px 1) solid "#e2e8f0"
  boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 10) (px 40)
  ".profile-img" ? do
    width (px 180)
    height (px 180)
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
    marginBottom (px 30)
    border (px 4) solid "#3b82f6"
    boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 0) (px 30)
    transition "all" 0.3 ease (sec 0)
    hover & do
      boxShadow . pure $ bsColor "#1d4ed8" $ shadowWithBlur (px 0) (px 0) (px 40)
  h1 ? do
    fontSize (px 48)
    marginBottom (px 20)
    fontWeight (weight 700)
    color "#1e293b"
  ".subtitle" ? do
    fontSize (px 20)
    marginBottom (px 40)
    maxWidth (px 700)
    margin (px 0) auto (px 40) auto
    color "#64748b"
    fontWeight (weight 400)

-- Enhanced social links with modern styling
socialStyles :: Css
socialStyles = ".social-links" ? do
  marginTop (px 40)
  marginBottom (px 20)
  display flex
  justifyContent center
  alignItems center
  ".social-link" ? do
    display inlineBlock
    padding (px 14) (px 24) (px 14) (px 24)
    backgroundColor white
    color "#3b82f6"
    textDecoration none
    borderRadius (px 12) (px 12) (px 12) (px 12)
    fontWeight (weight 500)
    fontSize (px 15)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 2) (px 10)
    transition "all" 0.3 ease (sec 0)
    marginRight (px 15)
    hover & do
      backgroundColor "#3b82f6"
      color white
      transform $ translateY (px (-2))
      boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 8) (px 25)
    lastChild & do
      marginRight (px 0)

-- Enhanced intro section
introStyles :: Css
introStyles = ".intro-section" ? do
  textAlign center
  maxWidth (px 900)
  margin (px 0) auto (px 60) auto
  padding (px 50) (px 40) (px 50) (px 40)
  backgroundColor white
  borderRadius (px 20) (px 20) (px 20) (px 20)
  border (px 1) solid "#e2e8f0"
  boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 8) (px 30)

-- Enhanced about section
aboutStyles :: Css
aboutStyles = ".about-section" ? do
  maxWidth (px 900)
  margin (px 0) auto (px 0) auto
  padding (px 60) (px 40) (px 60) (px 40)
  backgroundColor white
  borderRadius (px 20) (px 20) (px 20) (px 20)
  border (px 1) solid "#e2e8f0"
  boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 10) (px 40)
  textAlign center

-- Enhanced experience section with modern cards
experienceStyles :: Css
experienceStyles = do
  ".experience-section" ? do
    marginTop (px 60)
    marginBottom (px 60)
  ".section-header" ? do
    textAlign center
    marginBottom (px 50)
    padding (px 40) (px 20) (px 40) (px 20)
    backgroundColor white
    borderRadius (px 20) (px 20) (px 20) (px 20)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 8) (px 30)
  ".section-title" ? do
    fontSize (px 36)
    fontWeight (weight 700)
    marginBottom (px 15)
    color "#1e293b"
  ".section-subtitle" ? do
    fontSize (px 18)
    color "#64748b"
    fontWeight (weight 400)
  ".experiences-container" ? do
    display flex
    flexDirection column
  ".experience-card" ? do
    backgroundColor white
    padding (px 40) (px 40) (px 40) (px 40)
    borderRadius (px 20) (px 20) (px 20) (px 20)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 8) (px 30)
    transition "all" 0.3 ease (sec 0)
    marginBottom (px 30)
    hover & do
      transform $ translateY (px (-5))
      boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 20) (px 50)
      borderColor "#3b82f6"
  ".experience-header" ? do
    marginBottom (px 25)
    borderBottom (px 2) solid "#f1f5f9"
    paddingBottom (px 20)
  ".position" ? do
    fontSize (px 24)
    fontWeight (weight 600)
    color "#1e293b"
    marginBottom (px 8)
  ".company" ? do
    fontSize (px 20)
    fontWeight (weight 500)
    color "#3b82f6"
    marginBottom (px 5)
  ".location" ? do
    fontSize (px 16)
    color "#64748b"
    marginBottom (px 5)
  ".duration" ? do
    fontSize (px 16)
    color "#64748b"
    fontStyle italic
  ".experience-content" ? do
    marginTop (px 20)
  ".description" ? do
    marginBottom (px 20)
    p ? do
      color "#475569"
      lineHeight (unitless 1.6)
      marginBottom (px 12)
  ".achievements" ? do
    marginBottom (px 20)
    h5 ? do
      fontSize (px 18)
      fontWeight (weight 600)
      color "#1e293b"
      marginBottom (px 15)
    ul ? do
      paddingLeft (px 20)
      li ? do
        color "#475569"
        marginBottom (px 8)
        listStyleType disc
  ".technologies" ? do
    h5 ? do
      fontSize (px 18)
      fontWeight (weight 600)
      color "#1e293b"
      marginBottom (px 15)
  ".tech-tags" ? do
    display flex
  ".tech-tag" ? do
    padding (px 8) (px 16) (px 8) (px 16)
    backgroundColor "#eff6ff"
    color "#1d4ed8"
    borderRadius (px 20) (px 20) (px 20) (px 20)
    fontSize (px 14)
    fontWeight (weight 500)
    border (px 1) solid "#dbeafe"
    transition "all" 0.2 ease (sec 0)
    marginRight (px 10)
    marginBottom (px 10)
    hover & do
      backgroundColor "#1d4ed8"
      color white
  ".education-container" ? do
    display flex
    flexDirection column
  ".education-card" ? do
    backgroundColor white
    padding (px 35) (px 35) (px 35) (px 35)
    borderRadius (px 18) (px 18) (px 18) (px 18)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 6) (px 25)
    transition "all" 0.3 ease (sec 0)
    marginBottom (px 25)
    hover & do
      transform $ translateY (px (-3))
      boxShadow . pure $ bsColor "#06b6d4" $ shadowWithBlur (px 0) (px 15) (px 35)
  ".skills-section" ? do
    backgroundColor white
    padding (px 40) (px 40) (px 40) (px 40)
    borderRadius (px 20) (px 20) (px 20) (px 20)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 8) (px 30)
  ".skills-grid" ? do
    display grid
    gridTemplateColumns [fr 1, fr 1]
  ".skill-item" ? do
    padding (px 20) (px 25) (px 20) (px 25)
    backgroundColor white
    borderRadius (px 16) (px 16) (px 16) (px 16)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 2) (px 10)
    transition "all" 0.3 ease (sec 0)
    margin (px 10) (px 10) (px 10) (px 10)
    hover & do
      transform $ translateY (px (-2))
      boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 8) (px 25)
  ".skill-header" ? do
    display flex
    justifyContent spaceBetween
    alignItems center
    marginBottom (px 10)
  ".skill-name" ? do
    fontSize (px 16)
    fontWeight (weight 600)
    color "#1e293b"
  ".skill-level" ? do
    fontSize (px 14)
    fontWeight (weight 500)
    padding (px 4) (px 12) (px 4) (px 12)
    borderRadius (px 12) (px 12) (px 12) (px 12)
  ".skill-level.expert" ? do
    backgroundColor "#ecfdf5"
    color "#059669"
  ".skill-level.advanced" ? do
    backgroundColor "#eff6ff"
    color "#2563eb"
  ".skill-level.intermediate" ? do
    backgroundColor "#fef3c7"
    color "#d97706"
  ".skill-level.beginner" ? do
    backgroundColor "#fef2f2"
    color "#dc2626"
  ".skill-meta" ? do
    display flex
    justifyContent spaceBetween
    fontSize (px 13)
    color "#64748b"
  ".certificates-container" ? do
    display flex
    flexDirection column
  ".certificate-card" ? do
    backgroundColor white
    padding (px 25) (px 25) (px 25) (px 25)
    borderRadius (px 16) (px 16) (px 16) (px 16)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 4) (px 20)
    transition "all" 0.3 ease (sec 0)
    marginBottom (px 20)
    hover & do
      transform $ translateY (px (-2))
      boxShadow . pure $ bsColor "#06b6d4" $ shadowWithBlur (px 0) (px 10) (px 30)

-- Enhanced blog post list
blogPostListStyles :: Css
blogPostListStyles = do
  ".post-list" ? do
    maxWidth (px 900)
    margin (px 40) auto (px 0) auto
  ".post-preview" ? do
    backgroundColor white
    padding (px 35) (px 35) (px 35) (px 35)
    borderRadius (px 18) (px 18) (px 18) (px 18)
    border (px 1) solid "#e2e8f0"
    marginBottom (px 30)
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 6) (px 25)
    transition "all" 0.3 ease (sec 0)
    hover & do
      transform $ translateY (px (-3))
      boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 15) (px 40)

-- Enhanced blog post content
blogPostStyles :: Css
blogPostStyles = ".blog-post" ? do
  backgroundColor white
  padding (px 60) (px 40) (px 60) (px 40)
  marginBottom (px 40)
  borderRadius (px 20) (px 20) (px 20) (px 20)
  border (px 1) solid "#e2e8f0"
  boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 10) (px 40)

-- Enhanced content styles
contentStyles :: Css
contentStyles = do
  h2 ? do
    fontSize (px 32)
    color "#1e293b"
    marginTop (px 60)
    marginBottom (px 30)
    textAlign center
    fontWeight (weight 600)
  img ? do
    maxWidth (pct 100)
    height auto
    display block
    margin (px 30) auto (px 30) auto
    borderRadius (px 12) (px 12) (px 12) (px 12)
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 8) (px 25)
  pre ? do
    backgroundColor "#1e293b"
    color "#e2e8f0"
    padding (px 25) (px 25) (px 25) (px 25)
    borderRadius (px 12) (px 12) (px 12) (px 12)
    overflow auto
    marginTop (px 20)
    marginBottom (px 30)
    border (px 1) solid "#334155"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 4) (px 15)

-- Enhanced project styles
projectStyles :: Css
projectStyles = do
  ".project-section" ? do
    marginTop (px 60)
  ".project-card" ? do
    backgroundColor white
    padding (px 40) (px 40) (px 40) (px 40)
    marginBottom (px 40)
    borderRadius (px 20) (px 20) (px 20) (px 20)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 8) (px 30)
    transition "all" 0.3 ease (sec 0)
    hover & do
      transform $ translateY (px (-5))
      boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 20) (px 50)
  ".project-title" ? do
    fontSize (px 28)
    fontWeight (weight 600)
    color "#1e293b"
    marginBottom (px 15)
  ".project-description" ? do
    fontSize (px 16)
    color "#475569"
    lineHeight (unitless 1.6)
    marginBottom (px 25)
  ".project-links" ? do
    marginTop (px 20)
    marginBottom (px 25)
    display flex
  ".project-tech" ? do
    marginTop (px 25)
  ".tech-title" ? do
    fontWeight (weight 600)
    marginBottom (px 15)
    color "#1e293b"
    fontSize (px 16)
  ".tech-list" ? do
    display flex
  ".tech-item" ? do
    display flex
    alignItems center
    padding (px 8) (px 14) (px 8) (px 14)
    backgroundColor white
    borderRadius (px 16) (px 16) (px 16) (px 16)
    border (px 1) solid "#e2e8f0"
    fontSize (px 14)
    fontWeight (weight 500)
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 2) (px 8)
    marginRight (px 12)
    marginBottom (px 12)
  ".current-projects" ? do
    marginTop (px 60)
    backgroundColor white
    padding (px 40) (px 40) (px 40) (px 40)
    borderRadius (px 20) (px 20) (px 20) (px 20)
    border (px 1) solid "#e2e8f0"
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 8) (px 30)
  ".research-list" ? do
    marginTop (px 20)
  ".research-item" ? do
    display flex
    alignItems center
    marginBottom (px 15)
    fontSize (px 16)
    color "#475569"
  ".research-dot" ? do
    marginRight (px 12)
    color "#06b6d4"
    fontSize (px 20)

-- Colorful tech dots with enhanced colors
techDotStyles :: Css
techDotStyles = do
  ".tech-dot" ? do
    marginRight (px 8)
    fontSize (px 16)
    fontWeight bold
  ".tech-dot.gcp" ? color "#4285f4"      -- Google Cloud blue
  ".tech-dot.react" ? color "#61dafb"    -- React cyan
  ".tech-dot.typescript" ? color "#3178c6"  -- TypeScript blue
  ".tech-dot.python" ? color "#3776ab"   -- Python blue
  ".tech-dot.ml" ? color "#ff6b35"       -- ML orange
  ".tech-dot.flask" ? color "#000000"    -- Flask black
  ".tech-dot.expo" ? color "#000020"     -- Expo dark
  ".tech-dot.cnn" ? color "#ff6b6b"      -- CNN red

-- Enhanced project button styles
projectButtonStyles :: Css
projectButtonStyles = do
  ".demo-link" ? do
    display inlineBlock
    padding (px 14) (px 24) (px 14) (px 24)
    backgroundColor "#3b82f6"
    color white
    textDecoration none
    borderRadius (px 12) (px 12) (px 12) (px 12)
    fontWeight (weight 500)
    fontSize (px 15)
    transition "all" 0.3 ease (sec 0)
    boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 4) (px 15)
    marginRight (px 15)
    hover & do
      backgroundColor "#1d4ed8"
      transform $ translateY (px (-2))
      boxShadow . pure $ bsColor "#1d4ed8" $ shadowWithBlur (px 0) (px 8) (px 25)
  ".github-link" ? do
    display inlineBlock
    padding (px 14) (px 24) (px 14) (px 24)
    backgroundColor white
    color "#475569"
    textDecoration none
    borderRadius (px 12) (px 12) (px 12) (px 12)
    fontWeight (weight 500)
    fontSize (px 15)
    border (px 1) solid "#e2e8f0"
    transition "all" 0.3 ease (sec 0)
    boxShadow . pure $ bsColor "#0f172a" $ shadowWithBlur (px 0) (px 2) (px 10)
    hover & do
      backgroundColor "#f8fafc"
      borderColor "#3b82f6"
      color "#3b82f6"
      transform $ translateY (px (-2))
      boxShadow . pure $ bsColor "#3b82f6" $ shadowWithBlur (px 0) (px 8) (px 25)
  ".icon" ? do
    marginRight (px 6)

-- Enhanced footer
footerStyles :: Css
footerStyles = ".footer" ? do
  marginTop (px 80)
  padding (px 30) (px 0) (px 30) (px 0)
  borderTop (px 1) solid "#e2e8f0"
  textAlign center
  color "#64748b"
  backgroundColor "#f8fafc"
  fontSize (px 14)

-- CSS renderer
renderCSS :: Text
renderCSS = R.renderWith R.compact [] blogStyle 