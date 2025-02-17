# Personal Blog Generator

A static blog generator built in Haskell using EDSLs (Embedded Domain-Specific Languages) for both content and styling.

## Features

- Blog content defined using a custom EDSL
- CSS styling using additional EDSL
- Support for:
  - Text content
  - Headers
  - Images with captions
  - Code blocks with syntax highlighting
- Clean and modern design
- Static site generation

## Building and Running

1. Build the project:
   ```bash
   stack build
   ```

2. Generate the static site:
   ```bash
   stack run
   ```

3. Serve the site locally:
   ```bash
   cd dist && python -m http.server 8000
   ```

4. Visit http://localhost:8000 in your browser

## Adding Content

Edit `app/Main.hs` and modify the `samplePosts` function to add your blog posts. Each post can contain:

- Text content
- Headers
- Images
- Code blocks

## Customizing Style

Edit `src/CSS.hs` to modify the styling.
