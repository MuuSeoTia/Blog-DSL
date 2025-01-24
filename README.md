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
