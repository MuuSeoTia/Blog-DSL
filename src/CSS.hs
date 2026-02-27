{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Clay hiding (span, map)
import qualified Clay.Elements as E
import Data.Text.Lazy (Text)
import qualified Clay.Render as R
import Prelude hiding (span)
import qualified Prelude as P

-- main style
blogStyle :: Css
blogStyle = do
  globalStyles
  siteLayout
  navStyles
  headerStyles
  sectionStyles
  entryStyles
  blogStyles
  projectStyles
  skillStyles
  tagStyles
  codeStyles
  footerStyles

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
  h1 ? do
    fontFamily ["Source Serif 4", "Georgia", "Times New Roman"] [serif]
    fontSize (px 32)
    fontWeight (weight 700)
    lineHeight (unitless 1.3)
    marginBottom (px 8)
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

-- narrow reading column
siteLayout :: Css
siteLayout = ".site" ? do
  maxWidth (px 720)
  margin (px 0) auto (px 0) auto
  padding (px 0) (px 24) (px 0) (px 24)

-- nav: name left, links right
navStyles :: Css
navStyles = ".nav" ? do
  display flex
  justifyContent spaceBetween
  alignItems baseline
  padding (px 32) (px 0) (px 32) (px 0)
  borderBottom (px 1) solid "#e5e7eb"
  marginBottom (px 48)
  ".nav-name" ? do
    fontFamily ["Source Serif 4", "Georgia"] [serif]
    fontSize (px 20)
    fontWeight (weight 700)
    color "#0f0f0f"
    textDecoration none
  ".nav-links" ? do
    display flex
    key "gap" "24px"
    a ? do
      fontSize (px 15)
      color "#6b7280"
      textDecoration none
      fontWeight (weight 400)
      hover & do
        color "#1a1a1a"

-- site header / hero
headerStyles :: Css
headerStyles = do
  ".site-header" ? do
    marginBottom (px 56)
  ".header-row" ? do
    display flex
    alignItems center
    key "gap" "24px"
    marginBottom (px 16)
  ".profile-photo" ? do
    width (px 88)
    height (px 88)
    borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
    key "object-fit" "cover"
  ".header-text" ? do
    key "flex" "1"
  ".site-tagline" ? do
    fontSize (px 16)
    color "#6b7280"
    lineHeight (unitless 1.6)
    marginBottom (px 16)
  ".header-links" ? do
    display flex
    alignItems center
    key "gap" "8px"
    a ? do
      color "#6b7280"
      fontSize (px 14)
      textDecoration underline
      hover & do
        color "#1a1a1a"
    ".sep" ? do
      color "#d1d5db"
      fontSize (px 14)

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
  ".subsection-heading" ? do
    fontSize (px 18)
    fontWeight (weight 600)
    color "#1a1a1a"
    marginTop (px 40)
    marginBottom (px 24)

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
    key "flex" "1"
    h3 ? do
      fontSize (px 18)
      fontWeight (weight 600)
      color "#1a1a1a"
      marginBottom (px 2)
  ".entry-org" ? do
    fontSize (px 15)
    color "#6b7280"
  ".entry-date" ? do
    fontSize (px 14)
    color "#9ca3af"
    key "white-space" "nowrap"
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
    key "flex-wrap" "wrap"
    key "gap" "6px"
    marginTop (px 12)

-- blog post cards + post styles
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
    lastChild & do
      borderBottom (px 0) solid "#f3f4f6"
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
    marginBottom (px 8)
  ".post-meta" ? do
    display flex
    alignItems center
    key "gap" "16px"
    marginBottom (px 8)
    fontSize (px 13)
    color "#9ca3af"
  ".post-date" ? do
    fontSize (px 13)
    color "#9ca3af"
    key "text-transform" "uppercase"
    key "letter-spacing" "0.05em"
  ".post-tags" ? do
    display flex
    key "gap" "6px"
  -- full blog post page
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
    ul ? do
      paddingLeft (px 24)
      marginBottom (px 20)
      li ? do
        marginBottom (px 8)
        lineHeight (unitless 1.7)
    figure ? do
      marginTop (px 24)
      marginBottom (px 24)
      img ? do
        borderRadius (px 6) (px 6) (px 6) (px 6)
      figcaption ? do
        fontSize (px 13)
        color "#9ca3af"
        marginTop (px 8)

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
    key "gap" "12px"
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
  ".project-links" ? do
    display flex
    key "gap" "16px"
    ".link" ? do
      fontSize (px 14)
      color "#475569"
      textDecoration underline
      hover & do
        color "#1a1a1a"
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
      key "position" "relative"

-- skill display
skillStyles :: Css
skillStyles = do
  ".skills-section" ? do
    marginBottom (px 56)
  ".skill-group" ? do
    marginBottom (px 20)
  ".skill-category-name" ? do
    fontSize (px 14)
    fontWeight (weight 600)
    color "#6b7280"
    key "text-transform" "uppercase"
    key "letter-spacing" "0.05em"
    marginBottom (px 8)
  ".skill-list-text" ? do
    fontSize (px 15)
    color "#374151"
    lineHeight (unitless 1.7)
  ".skill-row" ? do
    display flex
    justifyContent spaceBetween
    alignItems center
    padding (px 8) (px 0) (px 8) (px 0)
    borderBottom (px 1) solid "#f9fafb"
  ".skill-badge" ? do
    fontSize (px 12)
    fontWeight (weight 500)
    padding (px 2) (px 10) (px 2) (px 10)
    borderRadius (px 10) (px 10) (px 10) (px 10)
  ".skill-badge.expert" ? do
    backgroundColor "#ecfdf5"
    color "#059669"
  ".skill-badge.advanced" ? do
    backgroundColor "#eff6ff"
    color "#2563eb"
  ".skill-badge.intermediate" ? do
    backgroundColor "#fffbeb"
    color "#d97706"
  ".skill-badge.learning" ? do
    backgroundColor "#faf5ff"
    color "#7c3aed"
  ".skill-badge.beginner" ? do
    backgroundColor "#f9fafb"
    color "#6b7280"

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

-- render
renderCSS :: Text
renderCSS = R.renderWith R.compact [] blogStyle
