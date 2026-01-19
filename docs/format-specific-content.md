# Format-Specific Content Guide

This guide explains how to handle content that should appear differently (or not at all) in HTML vs PDF outputs.

## Conditional Content Blocks

Use Quarto's conditional rendering to show/hide content based on format:

### Show content only in HTML

```markdown
::: {.content-visible-when-format=html}
This text only appears in the HTML version.
:::
```

Or using the shorter syntax:

```markdown
::: {.content-visible-when-format=html}
HTML-only content here
:::
```

### Show content only in PDF

```markdown
::: {.content-visible-when-format=pdf}
This text only appears in the PDF version.
:::
```

### Alternative: Using HTML comments (for HTML-only)

```html
<!-- This is HTML-only content -->
Some HTML-specific instructions here.
```

Note: HTML comments are automatically ignored in PDF.

## Code Chunk Options

### Code folding (HTML only)

Code folding works in HTML but not in PDF. You can make it conditional:

````markdown
```{r}
#| echo: true
#| code-fold: true
#| code-summary: "Load the data"
#| eval: false
library(tidyverse)
```
````

**For PDF**: Code folding is ignored, so the code will always be visible. If you want different behavior:

- **Option 1**: Always show code in PDF (default behavior - no action needed)
- **Option 2**: Hide code in PDF but show in HTML:

````markdown
```{r}
#| echo: true
#| code-fold: true
#| code-summary: "Load the data"
#| eval: false
library(tidyverse)
```

::: {.content-visible-when-format=pdf}
```{r}
#| echo: false
#| eval: false
# Same code but hidden in PDF
library(tidyverse)
```
:::
````

## Links

### External links

In HTML, links work well. In PDF, you might want to show full URLs:

```markdown
[Link text](https://example.com){.uri}
```

Or conditionally:

```markdown
::: {.content-visible-when-format=html}
[Click here for the dataset](https://github.com/user/repo)
:::

::: {.content-visible-when-format=pdf}
Dataset available at: https://github.com/user/repo
:::
```

## Images

Images work in both formats, but you might want different sizing:

```markdown
![](image.png){fig-alt="Description"}

::: {.content-visible-when-format=pdf}
![](image.png){width=80% fig-alt="Description"}
:::
```

## CSS Styling

Custom CSS (like in `index.qmd`) only applies to HTML:

```yaml
---
format:
  html:
    css: index-custom.css
---
```

This is automatically ignored in PDF.

## Examples from Your Book

### Code folding in 1_principles.qmd

The code chunks with `code-fold: true` will:
- **HTML**: Show as collapsible blocks
- **PDF**: Show code fully expanded (folding is ignored)

This is usually fine - PDF readers can see all code, which is often desirable.

### Links in datasets.qmd

The external links will:
- **HTML**: Clickable links
- **PDF**: Clickable links (if PDF viewer supports it) or you can add full URLs

### Custom CSS in index.qmd

The `index-custom.css` only affects HTML output, which is correct.

## Best Practices

1. **Code chunks**: Code folding in HTML is fine - PDF will show all code anyway
2. **Links**: Consider adding full URLs in parentheses for PDF readers
3. **Interactive elements**: Use conditional blocks to hide/show appropriately
4. **Long tables**: Consider different pagination strategies
5. **Callout boxes**: Work in both formats but may need styling adjustments

## Testing

After making changes, test both formats:

```bash
# Render HTML
quarto render --to html

# Render PDF
quarto render --to pdf

# Render both
quarto render --to html,pdf
```
