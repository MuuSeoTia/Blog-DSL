{-# LANGUAGE OverloadedStrings #-}
module CSS where

import Clay hiding (span)
import qualified Clay.Elements as E
import Data.Text.Lazy (Text)
import qualified Clay.Render as R

-- main blog style
blogStyle :: Css
blogStyle = do
  -- global styles
  body ? do
    margin (px 0) (px 0) (px 0) (px 0)
    backgroundColor "#f5f5f5"
    fontFamily ["system-ui"] [sansSerif]
    lineHeight (unitless 1.6)
    color "#333"
    minHeight (vh 100)
    display flex
    flexDirection column

  -- layout
  ".container" ? do
    maxWidth (px 900)
    margin (px 0) auto (px 0) auto
    padding (px 40) (px 20) (px 40) (px 20)
    flexGrow 1
    width (pct 100)

  -- nav
  ".nav" ? do
    padding (px 20) (px 0) (px 20) (px 0)
    marginBottom (px 60)
    borderBottom (px 1) solid "#eee"
    textAlign center
    backgroundColor white
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)
    a ? do
      display inlineBlock
      marginRight (px 30)
      color "#333"
      textDecoration none
      fontWeight bold
      padding (px 10) (px 15) (px 10) (px 15)
      hover & do
        color "#007acc"
        backgroundColor "#f8f9fa"
        borderRadius (px 4) (px 4) (px 4) (px 4)

  -- Hero Section
  ".hero-section" ? do
    textAlign center
    marginBottom (px 60)
    padding (px 60) (px 20) (px 60) (px 20)
    backgroundColor white
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)
    ".profile-img" ? do
      width (px 200)
      height (px 200)
      borderRadius (pct 50) (pct 50) (pct 50) (pct 50)
      marginBottom (px 30)
      border (px 4) solid "#fff"
      boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 4) (px 8)
    h1 ? do
      fontSize (px 42)
      marginBottom (px 15)
      color "#333"
      fontWeight bold
    ".subtitle" ? do
      fontSize (px 20)
      color "#666"
      marginBottom (px 30)
      maxWidth (px 600)
      margin (px 0) auto (px 30) auto

  -- Social Links
  ".social-links" ? do
    marginTop (px 30)
    marginBottom (px 20)
    display flex
    justifyContent center
    alignItems center
    ".social-link" ? do
      display inlineBlock
      marginRight (px 15)
      padding (px 10) (px 20) (px 10) (px 20)
      backgroundColor "#007acc"
      color white
      textDecoration none
      borderRadius (px 4) (px 4) (px 4) (px 4)
      transition "all" 0.3 ease (sec 0)
      hover & do
        backgroundColor "#005c99"
        transform $ scale 1.05 1.05
      lastChild & do
        marginRight (px 0)

  -- Intro Section
  ".intro-section" ? do
    textAlign center
    maxWidth (px 800)
    margin (px 0) auto (px 60) auto
    padding (px 40) (px 20) (px 40) (px 20)
    backgroundColor white
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)
    h2 ? do
      fontSize (px 36)
      color "#333"
      marginBottom (px 30)
      fontWeight bold
      lineHeight (unitless 1.2)
    p ? do
      fontSize (px 18)
      color "#555"
      maxWidth (px 600)
      margin (px 0) auto (px 20) auto
      lineHeight (unitless 1.8)
      "&:last-child" & do
        marginBottom (px 0)

  -- About Section
  ".about-section" ? do
    maxWidth (px 800)
    margin (px 0) auto (px 0) auto
    padding (px 60) (px 20) (px 60) (px 20)
    backgroundColor white
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)
    textAlign center
    ul ? do
      listStyleType none
      padding (px 0) (px 0) (px 0) (px 0)
      margin (px 30) auto (px 40) auto
      maxWidth (px 600)
    li ? do
      marginBottom (px 15)
      fontSize (px 18)
      color "#555"
      padding (px 10) (px 0) (px 10) (px 0)
      borderBottom (px 1) solid "#eee"
      lastChild & do
        marginBottom (px 0)
        borderBottom (px 0) solid "#eee"

  -- Blog Post List
  ".post-list" ? do
    maxWidth (px 800)
    margin (px 40) auto (px 0) auto
    article ? do
      marginBottom (px 30)
      lastChild & do
        marginBottom (px 0)

  ".post-preview" ? do
    backgroundColor white
    padding (px 30) (px 30) (px 30) (px 30)
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)
    transition "transform" 0.2 ease (sec 0)
    hover & do
      transforms [scale 1.02 1.02]
    h3 ? do
      margin (px 0) (px 0) (px 15) (px 0)
      fontSize (px 24)
      a ? do
        color "#333"
        textDecoration none
        hover & do
          color "#007acc"
    ".post-date" ? do
      color "#666"
      fontSize (px 14)
      fontStyle italic

  -- Blog Post Content
  ".blog-post" ? do
    backgroundColor white
    padding (px 60) (px 40) (px 60) (px 40)
    marginBottom (px 40)
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)
    h1 ? do
      fontSize (px 36)
      color "#333"
      marginBottom (px 20)
      fontWeight bold
      lineHeight (unitless 1.2)
    h2 ? do
      fontSize (px 28)
      color "#444"
      marginTop (px 40)
      marginBottom (px 20)
      lineHeight (unitless 1.3)
    p ? do
      fontSize (px 18)
      lineHeight (unitless 1.8)
      color "#555"
      marginBottom (px 25)
      marginTop (px 0)
    ".post-date" ? do
      color "#666"
      fontSize (px 16)
      fontStyle italic
      marginBottom (px 30)
      display block

  -- headers
  h2 ? do
    fontSize (px 32)
    color "#444"
    marginTop (px 60)
    marginBottom (px 30)
    textAlign center
    fontWeight normal

  -- content elements
  img ? do
    maxWidth (pct 100)
    height auto
    display block
    margin (px 30) auto (px 30) auto
    borderRadius (px 4) (px 4) (px 4) (px 4)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)

  -- code blocks
  pre ? do
    backgroundColor "#2d2d2d"
    padding (px 25) (px 25) (px 25) (px 25)
    borderRadius (px 4) (px 4) (px 4) (px 4)
    overflow auto
    marginTop (px 20)
    marginBottom (px 30)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)
    code ? do
      fontFamily ["Fira Code", "Consolas", "Monaco", "Andale Mono"] [monospace]
      fontSize (px 14)
      lineHeight (unitless 1.6)
      color "#ccc"

  -- footer
  ".footer" ? do
    marginTop (px 80)
    padding (px 20) (px 0) (px 20) (px 0)
    borderTop (px 1) solid "#eee"
    textAlign center
    color "#666"
    backgroundColor white
    borderRadius (px 8) (px 8) (px 8) (px 8)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 2) (px 4)

  -- Project Section Styles
  ".projects-main" ? do
    maxWidth (px 1000)
    margin (px 0) auto (px 0) auto

  ".section-title" ? do
    fontSize (px 36)
    color "#333"
    marginBottom (px 40)
    textAlign center
    fontWeight bold

  ".project-card" ? do
    backgroundColor white
    padding (px 40) (px 40) (px 40) (px 40)
    marginBottom (px 40)
    borderRadius (px 12) (px 12) (px 12) (px 12)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 4) (px 8)
    transition "transform" 0.3 ease (sec 0)
    hover & do
      transform $ scale 1.02 1.02

  ".project-title" ? do
    fontSize (px 28)
    color "#333"
    marginBottom (px 20)
    fontWeight bold

  ".project-description" ? do
    fontSize (px 18)
    color "#555"
    lineHeight (unitless 1.6)
    marginBottom (px 30)

  ".project-links" ? do
    display flex
    marginBottom (px 30)
    a ? do
      display inlineBlock
      padding (px 12) (px 20) (px 12) (px 20)
      marginRight (px 20)
      borderRadius (px 6) (px 6) (px 6) (px 6)
      textDecoration none
      fontWeight bold
      transition "all" 0.3 ease (sec 0)
      lastChild & do
        marginRight (px 0)
    ".demo-link" ? do
      backgroundColor "#007acc"
      color white
      hover & do
        backgroundColor "#005c99"
    ".github-link" ? do
      backgroundColor "#333"
      color white
      hover & do
        backgroundColor "#222"
    ".icon" ? do
      marginRight (px 8)
      fontSize (px 16)

  ".tech-title" ? do
    fontSize (px 18)
    color "#666"
    marginBottom (px 15)
    fontWeight bold

  ".tech-list" ? do
    display block
    overflow hidden
    marginLeft (px (-15))

  ".tech-item" ? do
    display inlineBlock
    padding (px 8) (px 15) (px 8) (px 15)
    fontSize (px 16)
    color "#555"
    E.span ? do
      marginRight (px 8)
      lastChild & do
        marginRight (px 0)

  ".tech-dot" ? do
    fontSize (px 20)
    lineHeight (unitless 1)
    ".gcp" & do
      color "#4285F4"
    ".react" & do
      color "#61DAFB"
    ".typescript" & do
      color "#3178C6"
    ".python" & do
      color "#3776AB"
    ".flask" & do
      color "#000000"
    ".expo" & do
      color "#000020"
    ".ml" & do
      color "#FF6F00"

  ".current-projects" ? do
    backgroundColor white
    padding (px 40) (px 40) (px 40) (px 40)
    borderRadius (px 12) (px 12) (px 12) (px 12)
    boxShadow $ pure $ bsColor (rgba 0 0 0 0.1) $ shadowWithBlur (px 0) (px 4) (px 8)
    marginTop (px 60)

  ".research-list" ? do
    display block

  ".research-item" ? do
    display block
    fontSize (px 18)
    color "#555"
    padding (px 15) (px 20) (px 15) (px 20)
    backgroundColor "#f8f9fa"
    borderRadius (px 8) (px 8) (px 8) (px 8)
    transition "transform" 0.2 ease (sec 0)
    marginBottom (px 20)
    lastChild & do
      marginBottom (px 0)
    hover & do
      transform $ scale 1.02 1.02

  ".research-dot" ? do
    display inlineBlock
    color "#007acc"
    fontSize (px 16)
    marginRight (px 15)
    verticalAlign middle

-- CSS renderer
renderCSS :: Text
renderCSS = R.renderWith R.compact [] blogStyle 