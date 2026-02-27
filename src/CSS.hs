{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Clay hiding (span, map, query, screen)
import Clay.Stylesheet (key, query, Feature(..))
import Clay.Media (screen)
import Data.Text.Lazy (Text)
import qualified Clay.Render as R
import Prelude hiding (span)

-- main style
blogStyle :: Css
blogStyle = do
  globalStyles
  accessibilityStyles
  siteLayout
  navStyles
  headerStyles
  imageStyles
  sectionStyles
  entryStyles
  blogStyles
  projectStyles
  pressStyles
  skillStyles
  tagStyles
  codeStyles
  footerStyles
  mobileStyles

-- base reset + typography
globalStyles :: Css
globalStyles = do
  star ? do
    margin (px 0) (px 0) (px 0) (px 0)
    padding (px 0) (px 0) (px 0) (px 0)
    boxSizing borderBox
  body ? do
    fontFamily ["Inter", "-apple-system", "BlinkMacSystemFont", "system-ui"] [sansSerif]
    fontSize (px 16)
    lineHeight (unitless 1.75)
    color "#1a1a1a"
    backgroundColor "#ffffff"
    key "-webkit-font-smoothing" (Value "antialiased")
  h1 ? do
    fontFamily ["Source Serif 4", "Georgia", "Times New Roman"] [serif]
    fontSize (px 32)
    fontWeight (weight 700)
    lineHeight (unitless 1.3)
    marginBottom (px 12)
    color "#0f0f0f"
  h2 ? do
    fontFamily ["Source Serif 4", "Georgia", "Times New Roman"] [serif]
    fontSize (px 24)
    fontWeight (weight 600)
    lineHeight (unitless 1.4)
    marginTop (px 48)
    marginBottom (px 16)
    color "#0f0f0f"
  h3 ? do
    fontSize (px 18)
    fontWeight (weight 600)
    lineHeight (unitless 1.4)
    marginBottom (px 4)
    color "#1a1a1a"
  h4 ? do
    fontSize (px 16)
    fontWeight (weight 500)
    color "#6b7280"
    marginBottom (px 4)
  p ? do
    marginBottom (px 16)
    color "#374151"
  a ? do
    color "#475569"
    textDecoration none
    transition "color" 0.15 ease (sec 0)
    hover & do
      color "#1a1a1a"
  img ? do
    maxWidth (pct 100)
    height auto

-- accessibility: focus states, skip link
accessibilityStyles :: Css
accessibilityStyles = do
  ".skip-link" ? do
    key "position" (Value "absolute")
    key "top" (Value "-40px")
    left (px 0)
    backgroundColor "#1a1a1a"
    color white
    padding (px 8) (px 16) (px 8) (px 16)
    fontSize (px 14)
    key "z-index" (Value "100")
    focus & do
      key "top" (Value "0")
  -- focus outlines
  a ? do
    focus & do
      key "outline" (Value "2px solid #475569")
      key "outline-offset" (Value "2px")
      borderRadius (px 2) (px 2) (px 2) (px 2)

-- narrow reading column
siteLayout :: Css
siteLayout = ".site" ? do
  maxWidth (px 720)
  margin (px 0) auto (px 0) auto
  padding (px 0) (px 24) (px 0) (px 24)

-- nav
navStyles :: Css
navStyles = ".nav" ? do
  display flex
  justifyContent spaceBetween
  alignItems baseline
  padding (px 28) (px 0) (px 28) (px 0)
  borderBottom (px 1) solid "#e5e7eb"
  marginBottom (px 48)
  ".nav-name" ? do
    fontFamily ["Source Serif 4", "Georgia"] [serif]
    fontSize (px 20)
    fontWeight (weight 700)
    color "#0f0f0f"
    textDecoration none
    ".short-name" ? do
      display none
  ".nav-links" ? do
    display flex
    key "gap" (Value "20px")
    a ? do
      fontSize (px 15)
      color "#6b7280"
      textDecoration none
      fontWeight (weight 400)
      padding (px 4) (px 0) (px 4) (px 0)
      borderBottom (px 2) solid (other "transparent")
      transition "all" 0.15 ease (sec 0)
      hover & do
        color "#1a1a1a"
        borderBottom (px 2) solid "#e5e7eb"

-- site header
headerStyles :: Css
headerStyles = do
  ".site-header" ? do
    marginBottom (px 56)
  ".header-row" ? do
    display flex
    flexDirection column
    alignItems center
    key "gap" (Value "20px")
    marginBottom (px 20)
    key "text-align" (Value "center")
  ".profile-photo" ? do
    width (px 112)
    height (px 112)
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
    key "object-fit" (Value "cover")
    border (px 2) solid "#f3f4f6"
  ".header-text" ? do
    key "text-align" (Value "center")
  ".site-tagline" ? do
    fontSize (px 16)
    color "#6b7280"
    lineHeight (unitless 1.6)
    marginBottom (px 16)
    maxWidth (px 560)
    margin (px 0) auto (px 16) auto
  ".header-links" ? do
    display flex
    alignItems center
    justifyContent center
    key "flex-wrap" (Value "wrap")
    key "gap" (Value "8px")
    a ? do
      color "#6b7280"
      fontSize (px 14)
      textDecoration underline
      key "text-decoration-color" (Value "#d1d5db")
      key "text-underline-offset" (Value "2px")
      hover & do
        color "#1a1a1a"
        key "text-decoration-color" (Value "#1a1a1a")
    ".sep" ? do
      color "#d1d5db"
      fontSize (px 14)

-- section images (bigger, full width)
imageStyles :: Css
imageStyles = do
  ".section-img" ? do
    width (pct 100)
    height auto
    borderRadius (px 8) (px 8) (px 8) (px 8)
    marginTop (px 16)
    marginBottom (px 32)
    key "object-fit" (Value "cover")
    key "max-height" (Value "400px")

-- section headings
sectionStyles :: Css
sectionStyles = do
  ".section-heading" ? do
    fontFamily ["Source Serif 4", "Georgia"] [serif]
    fontSize (px 22)
    fontWeight (weight 600)
    color "#0f0f0f"
    marginBottom (px 32)
    paddingBottom (px 12)
    borderBottom (px 1) solid "#e5e7eb"
  ".section-intro" ? do
    color "#374151"
    marginBottom (px 32)
    lineHeight (unitless 1.75)

-- experience / education entries
entryStyles :: Css
entryStyles = do
  ".experience-entry" ? do
    marginBottom (px 36)
    paddingBottom (px 36)
    borderBottom (px 1) solid "#f3f4f6"
    lastChild & do
      borderBottom (px 0) solid "#f3f4f6"
      marginBottom (px 0)
      paddingBottom (px 0)
  ".entry-header" ? do
    display flex
    justifyContent spaceBetween
    alignItems flexStart
    marginBottom (px 12)
  ".entry-title" ? do
    key "flex" (Value "1")
    h3 ? do
      fontSize (px 18)
      fontWeight (weight 600)
      color "#1a1a1a"
      marginBottom (px 2)
  ".entry-org" ? do
    fontSize (px 15)
    color "#6b7280"
    display block
    a ? do
      color "#6b7280"
      textDecoration underline
      key "text-decoration-color" (Value "#d1d5db")
      key "text-underline-offset" (Value "2px")
      hover & do
        color "#1a1a1a"
        key "text-decoration-color" (Value "#1a1a1a")
  ".entry-date" ? do
    fontSize (px 14)
    color "#9ca3af"
    key "white-space" (Value "nowrap")
    marginLeft (px 16)
  ".entry-body" ? do
    marginTop (px 8)
  ".entry-list" ? do
    paddingLeft (px 20)
    marginBottom (px 12)
    li ? do
      color "#374151"
      marginBottom (px 6)
      fontSize (px 15)
      lineHeight (unitless 1.65)
  ".entry-detail" ? do
    fontSize (px 15)
    color "#374151"
    marginBottom (px 8)
  ".entry-tech" ? do
    display flex
    key "flex-wrap" (Value "wrap")
    key "gap" (Value "6px")
    marginTop (px 12)

-- blog cards + post page
blogStyles :: Css
blogStyles = do
  ".writing-section" ? do
    marginBottom (px 56)
  ".post-list" ? do
    display flex
    flexDirection column
  ".post-card" ? do
    paddingBottom (px 28)
    marginBottom (px 28)
    borderBottom (px 1) solid "#f3f4f6"
    transition "border-color" 0.15 ease (sec 0)
    lastChild & do
      borderBottom (px 0) solid "#f3f4f6"
    hover & do
      borderBottom (px 1) solid "#d1d5db"
  ".post-card-title" ? do
    fontFamily ["Source Serif 4", "Georgia"] [serif]
    fontSize (px 20)
    fontWeight (weight 600)
    marginBottom (px 6)
    a ? do
      color "#1a1a1a"
      textDecoration none
      hover & do
        color "#475569"
  ".post-excerpt" ? do
    fontSize (px 15)
    color "#6b7280"
    lineHeight (unitless 1.6)
    marginBottom (px 4)
  ".post-meta" ? do
    display flex
    alignItems center
    key "gap" (Value "16px")
    marginBottom (px 8)
    fontSize (px 13)
    color "#9ca3af"
  ".post-date" ? do
    fontSize (px 13)
    color "#9ca3af"
    key "text-transform" (Value "uppercase")
    key "letter-spacing" (Value "0.05em")
  ".post-tags" ? do
    display flex
    key "gap" (Value "6px")
  -- blog post page
  ".blog-post" ? do
    marginBottom (px 48)
    h1 ? do
      fontSize (px 36)
      marginBottom (px 24)
    h2 ? do
      marginTop (px 40)
      marginBottom (px 16)
    p ? do
      fontSize (px 16)
      lineHeight (unitless 1.8)
      marginBottom (px 20)
      color "#374151"
      a ? do
        color "#475569"
        key "text-decoration" (Value "underline")
        key "text-decoration-color" (Value "#cbd5e1")
        key "text-underline-offset" (Value "3px")
        transition "text-decoration-color" 0.15 ease (sec 0)
        hover & do
          key "text-decoration-color" (Value "#475569")
          color "#1a1a1a"
    ul ? do
      paddingLeft (px 24)
      marginBottom (px 20)
      li ? do
        marginBottom (px 8)
        lineHeight (unitless 1.7)
        color "#374151"
    figure ? do
      marginTop (px 32)
      marginBottom (px 32)
      img ? do
        borderRadius (px 6) (px 6) (px 6) (px 6)
        width (pct 100)
      figcaption ? do
        fontSize (px 13)
        color "#9ca3af"
        marginTop (px 10)
        fontStyle italic

-- project entries
projectStyles :: Css
projectStyles = do
  ".projects-section" ? do
    marginBottom (px 56)
  ".project-entry" ? do
    marginBottom (px 36)
    paddingBottom (px 36)
    borderBottom (px 1) solid "#f3f4f6"
    lastChild & do
      borderBottom (px 0) solid "#f3f4f6"
      marginBottom (px 0)
      paddingBottom (px 0)
  ".project-header" ? do
    display flex
    alignItems baseline
    key "gap" (Value "12px")
    marginBottom (px 8)
  ".project-title" ? do
    fontFamily ["Source Serif 4", "Georgia"] [serif]
    fontSize (px 20)
    fontWeight (weight 600)
    color "#1a1a1a"
    marginBottom (px 0)
  ".project-highlight" ? do
    fontSize (px 13)
    color "#059669"
    fontWeight (weight 500)
  ".project-desc" ? do
    fontSize (px 15)
    color "#6b7280"
    lineHeight (unitless 1.6)
    marginBottom (px 12)
  ".project-footer" ? do
    display flex
    justifyContent spaceBetween
    alignItems flexStart
    key "flex-wrap" (Value "wrap")
    key "gap" (Value "12px")
  ".project-links" ? do
    display flex
    key "gap" (Value "16px")
    ".link" ? do
      fontSize (px 14)
      color "#475569"
      textDecoration underline
      key "text-decoration-color" (Value "#d1d5db")
      key "text-underline-offset" (Value "2px")
      hover & do
        color "#1a1a1a"
        key "text-decoration-color" (Value "#1a1a1a")
  ".current-work" ? do
    marginTop (px 48)
    h2 ? do
      fontSize (px 20)
      marginBottom (px 16)
    ".work-item" ? do
      fontSize (px 15)
      color "#374151"
      marginBottom (px 8)
      paddingLeft (px 16)
      key "position" (Value "relative")

-- press items
pressStyles :: Css
pressStyles = do
  ".press-list" ? do
    display flex
    flexDirection column
    key "gap" (Value "12px")
    marginBottom (px 8)
  ".press-item" ? do
    display flex
    alignItems flexStart
    key "gap" (Value "12px")
    padding (px 12) (px 16) (px 12) (px 16)
    backgroundColor "#f9fafb"
    borderRadius (px 6) (px 6) (px 6) (px 6)
    border (px 1) solid "#f3f4f6"
    textDecoration none
    transition "border-color" 0.15 ease (sec 0)
    hover & do
      border (px 1) solid "#e5e7eb"
  ".press-source" ? do
    fontSize (px 12)
    color "#9ca3af"
    key "text-transform" (Value "uppercase")
    key "letter-spacing" (Value "0.05em")
    key "white-space" (Value "nowrap")
    marginTop (px 2)
  ".press-title" ? do
    fontSize (px 14)
    color "#374151"
    lineHeight (unitless 1.5)

-- skill legend + tags
skillStyles :: Css
skillStyles = do
  ".skill-legend" ? do
    display flex
    key "flex-wrap" (Value "wrap")
    key "gap" (Value "10px")
    marginBottom (px 24)
    ".legend-item" ? do
      fontSize (px 12)
      fontWeight (weight 500)
      padding (px 2) (px 10) (px 2) (px 10)
      borderRadius (px 4) (px 4) (px 4) (px 4)
    ".legend-item.expert" ? do
      backgroundColor "#ecfdf5"
      color "#059669"
    ".legend-item.advanced" ? do
      backgroundColor "#eff6ff"
      color "#2563eb"
    ".legend-item.intermediate" ? do
      backgroundColor "#fffbeb"
      color "#d97706"
    ".legend-item.learning" ? do
      backgroundColor "#faf5ff"
      color "#7c3aed"
    ".legend-item.beginner" ? do
      backgroundColor "#f9fafb"
      color "#6b7280"
  ".skills-section" ? do
    marginBottom (px 56)
  ".skill-group" ? do
    marginBottom (px 24)
  ".skill-category-name" ? do
    fontSize (px 14)
    fontWeight (weight 600)
    color "#6b7280"
    key "text-transform" (Value "uppercase")
    key "letter-spacing" (Value "0.05em")
    marginBottom (px 10)
  ".skill-tags" ? do
    display flex
    key "flex-wrap" (Value "wrap")
    key "gap" (Value "8px")
  ".skill-tag" ? do
    display inlineBlock
    fontSize (px 13)
    fontWeight (weight 500)
    padding (px 4) (px 12) (px 4) (px 12)
    borderRadius (px 6) (px 6) (px 6) (px 6)
    transition "all" 0.15 ease (sec 0)
  ".skill-tag.expert" ? do
    backgroundColor "#ecfdf5"
    color "#059669"
    border (px 1) solid "#d1fae5"
  ".skill-tag.advanced" ? do
    backgroundColor "#eff6ff"
    color "#2563eb"
    border (px 1) solid "#dbeafe"
  ".skill-tag.intermediate" ? do
    backgroundColor "#fffbeb"
    color "#d97706"
    border (px 1) solid "#fef3c7"
  ".skill-tag.learning" ? do
    backgroundColor "#faf5ff"
    color "#7c3aed"
    border (px 1) solid "#ede9fe"
  ".skill-tag.beginner" ? do
    backgroundColor "#f9fafb"
    color "#6b7280"
    border (px 1) solid "#f3f4f6"

-- tech pills and tags
tagStyles :: Css
tagStyles = do
  ".tech-pill" ? do
    display inlineBlock
    fontSize (px 13)
    color "#475569"
    backgroundColor "#f3f4f6"
    padding (px 3) (px 10) (px 3) (px 10)
    borderRadius (px 4) (px 4) (px 4) (px 4)
  ".tag" ? do
    display inlineBlock
    fontSize (px 12)
    color "#6b7280"
    backgroundColor "#f9fafb"
    padding (px 2) (px 8) (px 2) (px 8)
    borderRadius (px 3) (px 3) (px 3) (px 3)

-- code blocks
codeStyles :: Css
codeStyles = do
  pre ? do
    backgroundColor "#f9fafb"
    border (px 1) solid "#e5e7eb"
    borderRadius (px 6) (px 6) (px 6) (px 6)
    padding (px 20) (px 24) (px 20) (px 24)
    overflow auto
    marginBottom (px 24)
    fontSize (px 14)
    lineHeight (unitless 1.6)
    key "-webkit-overflow-scrolling" (Value "touch")
  code ? do
    fontFamily ["JetBrains Mono", "Fira Code", "Consolas"] [monospace]
    fontSize (px 14)

-- footer
footerStyles :: Css
footerStyles = ".footer" ? do
  marginTop (px 80)
  paddingTop (px 24)
  paddingBottom (px 32)
  borderTop (px 1) solid "#e5e7eb"
  textAlign center
  fontSize (px 13)
  color "#9ca3af"
  a ? do
    color "#9ca3af"
    textDecoration underline
    key "text-decoration-color" (Value "#d1d5db")
    hover & do
      color "#6b7280"

-- mobile responsive
mobileStyles :: Css
mobileStyles = query screen [Feature "max-width" (Just $ value (px 640))] $ do
  ".nav" ? do
    padding (px 20) (px 0) (px 20) (px 0)
    marginBottom (px 32)
    ".nav-name" ? do
      fontSize (px 18)
      ".full-name" ? display none
      ".short-name" ? display inlineBlock
    ".nav-links" ? do
      key "gap" (Value "14px")
      a ? fontSize (px 14)
  ".site" ? do
    padding (px 0) (px 16) (px 0) (px 16)
  ".profile-photo" ? do
    width (px 88)
    height (px 88)
  ".section-img" ? do
    key "max-height" (Value "280px")
    borderRadius (px 6) (px 6) (px 6) (px 6)
  h1 ? fontSize (px 26)
  h2 ? do
    fontSize (px 20)
    marginTop (px 36)
  ".entry-header" ? do
    flexDirection column
  ".entry-date" ? do
    marginLeft (px 0)
    marginTop (px 4)
  ".project-footer" ? do
    flexDirection column
  ".blog-post" ? do
    h1 ? fontSize (px 28)
  pre ? do
    padding (px 16) (px 16) (px 16) (px 16)
    fontSize (px 13)

-- render
renderCSS :: Text
renderCSS = R.renderWith R.compact [] blogStyle
