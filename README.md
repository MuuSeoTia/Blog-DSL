
# Personal Blog Generator

A static blog generator built with Haskell, using EDSLs for content and styling.

## Features

- Blog post generation using a custom EDSL
- CSS styling using Clay EDSL
- Static site generation
- Code syntax highlighting
- Responsive design

## Building

```bash
stack build
```

## Running

```bash
stack run
```

This will generate the static site in the `dist` directory.

## Project Structure

- `src/BlogDSL.hs` - Blog content EDSL
- `src/CSS.hs` - CSS styling EDSL
- `app/Main.hs` - Static site generator
- `templates/` - HTML templates
- `dist/` - Generated static site

## License

BSD3
=======
# Personal Blog Generator

A static blog generator built in Haskell using EDSLs (Embedded Domain-Specific Languages) for both content and styling.

## Features

- Blog content defined using a custom EDSL
- CSS styling using Clay EDSL
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
# Haskell_Site

