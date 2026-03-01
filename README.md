# Blog-DSL

My personal website built entirely with Haskell EDSLs. No templates, no markdown, no frameworks. Just types and functions.

**Live at [tiahi.dev](https://www.tiahi.dev)**

## Quick Start

```bash
stack build          # compile everything
stack run            # generate dist/
cd dist && python3 -m http.server 8000   # preview at localhost:8000
```

## How the DSL Works

The site is generated from pure Haskell data types. Content is defined as values, rendered to HTML via typeclasses, and styled with Clay's CSS EDSL.

### Architecture

```
app/Main.hs      -- all site data (experiences, posts, skills, projects)
src/BlogDSL.hs   -- core types, rendering typeclasses, helpers
src/Pages.hs     -- page generators (index, about, experience, blog posts)
src/CSS.hs       -- full stylesheet via Clay EDSL
```

### Core Types

```haskell
-- blog content is a list of elements
data BlogElement =
    TextContent Text
  | RichText [InlineContent]       -- mixed text + clickable links
  | HeaderContent Text
  | Image { path, alt, caption }
  | CodeBlock { language, codeContent }
  | BulletList [Text]

-- inline links within text
data InlineContent = Plain Text | Link Text Text  -- (display, url)

-- a blog post
data BlogPost = BlogPost
  { postId, title, date, excerpt, tags, content :: [BlogElement] }

-- work experience with optional company links
data Experience = Experience
  { company, position, location, startDate, endDate,
    description, technologies, achievements,
    companyUrl, companyDetail :: Maybe (Text, Text) }
```

### Adding Content

**New blog post** in `app/Main.hs`:
```haskell
BlogPost 4 "Post Title"
    (UTCTime (fromGregorian 2025 6 1) 0)
    "Excerpt text for the card."
    ["tag1", "tag2"]
    [ HeaderContent "Section"
    , TextContent "Paragraph here."
    , RichText [Plain "Check out ", Link "this" "https://example.com", Plain "."]
    , CodeBlock "python" "print('hello')"
    , BulletList ["point one", "point two"]
    ]
```

**New experience** in `app/Main.hs`:
```haskell
Experience 6 "Company" "Role" "Location"
    (fromGregorian 2025 1 1) Nothing       -- start, end (Nothing = present)
    ["bullet 1", "bullet 2"]               -- description
    ["Tech1", "Tech2"]                      -- technologies
    ["achievement 1"]                       -- achievements
    Nothing                                 -- company logo
    (Just "https://company.com")            -- company url (clickable)
    Nothing                                 -- company detail (label, url)
```

**New skill** in `app/Main.hs`:
```haskell
Skill "SkillName" Advanced Programming (Just 3)  -- name, level, category, years
-- levels: Beginner | Learning | Intermediate | Advanced | Expert
-- categories: Programming | MachineLearning | HPC | Cloud | Framework | Tool
```

### Styling

All CSS lives in `src/CSS.hs` using the Clay EDSL:
```haskell
".my-class" ? do
  fontSize (px 16)
  color "#1a1a1a"
  key "custom-prop" (Value "value")  -- for props Clay doesn't have
```

### Pages

Page generators live in `src/Pages.hs`. Each function takes data from Main.hs and returns `Html ()`:
- `generateIndex` -- home page with header + blog cards
- `generateAbout` -- about, press, currently learning
- `generateExperiences` -- work, education, skills
- `generateProjectsFromData` -- project cards
- `renderBlogPost` -- individual blog post pages

## Deployment

Pushes to `main` auto-deploy to [muuseotia.github.io](https://github.com/MuuSeoTia/muuseotia.github.io) via GitHub Actions.

**Setup**: add a `DEPLOY_TOKEN` secret to this repo (Settings > Secrets > Actions) with a GitHub personal access token that has `repo` scope.

## Stack

Haskell (GHC 9.4.8) + Stack LTS 21.25. Dependencies: lucid (HTML), clay (CSS), aeson (JSON), time, text, containers.
