# Animate vector graphics for html-output generated with Quarto or R-Markdown with one line of code to achieve simple and easy morph-like transitions

## A quick and low-effort workflow for powerpoint-like morph effects without any manual coding and without dependencies.

- Works by directly outputting the html code at the required position (e.g. in a quarto presentation, a quarto website, a bookdown document, etc.)
- Automatically integrates into the quarto slide animation/fragment order
- IMPORTANT: If svg elements do not have IDs (see below), I have included a simple algorithm that tries to match elements based on common attributes across frames. This does not work well and may break your intended animations if different elements are more similar to each other across frames. You can give every element an ID by saving an image with a vector program like inkscape (free & open source) - see workflow below.

## Workflow 

Installing: Run `devtools::install_github("klingelhoefer/quartoMorphSvg")`
Activating Package: `library(quartoMorphSvg)`

### From Powerpoint
1. Save a slide that includes shapes, text, paths, etc. as svg with powerpoint
1. Copy the slide in powerpoint and make the desired changes, then save all slides as svg. Do not move objects outside of the slide boundaries, as they will not be saved into the SVG file.
3. Alternatively if 1-2 lead to unexpected outcomes: Open initial slide in a vector graphics program like inkscape, save the file as svg to add IDs, copy the file and make the desired changes
4. Put the following inline-R-code where you want the image to load and input/paste the path to the folder PowerPoint generates: `` `r morph_svg(folder = "images/monstera/")` ``[^1]. Note that you need to change the path to your local relative/absolute path.

### From any vector graphic (e.g. created with inkscape)
1. Save the vector graphic to create IDs
2. Copy the initial file and make desired changes in the copy
3. Put the following inline-R-code with the path to the image(s) or folder where you want the image to load, e.g.: `` `r morph_svg("images/moderation/Slide1.svg", "images/moderation/Slide2.svg")` ``[^1]. 
## Limitations & Troubleshooting
  - Works best for documents in which every element has an id. 
    - *Troubleshooting*: 
      1. Generate IDs with inkscape by using File > Save As... > Inkscape SVG (*.svg) (the default)
      2. Copy the document (ensures that ids match) edit in inkscape without adding or removing elements, saving second image
    - Documents saved in the inkscape default format 
  - Path transitions only work if they have the same number of nodes. Otherwise the paths will "jump" to the next frame 
  - The code crudely matches the ids of svg elements by tag and order
    - *Troubleshooting*: 
      1. Try grouping objects and changing their attributes/position
      1. Or edit plain text svg to maintain similar path structures
  - If an object does not exist in the initial image, it will not show up
    - *Troubleshooting*: 
      1. Have all objects and matching ids in all images and make them transparent or move them out of frame as needed

[^1]: Alternatively, you can call the function (`morph_svg(...)`) from a codechunk and hide the code output (`#| echo: false`). 



