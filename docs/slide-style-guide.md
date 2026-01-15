# Quarto Slide Deck Style Guide

Quick reference for styling options in RevealJS slides using `intro-ai-teaching.css`.

---

## Setup

Include the CSS in your YAML header:

```yaml
format: 
  revealjs:
    css: intro-ai-teaching.css
```

---

## Inline Notes

Add smaller, grey text after bullet points:

```markdown
- **Main point.** [Additional context in grey]{.note}
```

**Result:** Main point in bold, followed by smaller grey text on the same line.

---

## Incremental Reveal with Highlight

Current item highlighted, previous items grey:

```markdown
::: {.incremental .highlight-last}
- First point
- Second point
- Third point
:::
```

**Result:** Items appear one at a time. Current item is purple (#5500ff), previous items are grey.

---

## Fragments

Reveal content on click:

```markdown
Some visible text.

::: {.fragment}
This appears after a click.
:::
```

**Note:** Slides with fragments are automatically vertically centered.

---

## Spacing Classes

### `.spaced`
Add space between title and content (for slides with few items):

```markdown
## My Title {.spaced}

Content starts lower on the slide.
```

### `.center`
Vertically center all content:

```markdown
## My Title {.center}

This content is vertically centered.
```

---

## Automatic Spacing

The CSS automatically adds spacing for:
- Slides with 1-3 bullet points (no fragments)
- Text-only slides with minimal paragraphs

No class needed—it just works.

---

## RevealJS Tips & Tricks

### Hide a slide (skip during presentation)

```markdown
## Slide Title {visibility="hidden"}
```

### Uncounted slide (doesn't affect slide numbers)

```markdown
## Bonus Slide {visibility="uncounted"}
```

### Smaller text on a slide

```markdown
## Dense Content {.smaller}
```

### Scrollable slide (for long content)

```markdown
## Long List {.scrollable}
```

### Speaker notes (only you see these)

```markdown
## My Slide

Content here.

::: {.notes}
Reminder: mention the example from last week.
:::
```

### Pause within a slide

Use `. . .` on its own line to create a pause:

```markdown
## Step by Step

First thing.

. . .

Second thing (appears after click).
```

### Two-column layout (text + image/chart)

Simple layout with text on left, visual on right:

```markdown
## My Slide

:::: {.columns}
::: {.column-left}
Your explanation text goes here.

- Key point one
- Key point two
:::
::: {.column-right}
![](path/to/image.png)
:::
::::
```

**Note:** Left column is wider (1.5), right column is narrower (1). Content is vertically centered.

For large text in the right column, add `.large-text`:

```markdown
::: {.column-right .large-text}
Big number or label
:::
```

### Two-column layout (equal width)

Standard Quarto syntax for equal columns:

```markdown
:::: {.columns}
::: {.column width="50%"}
Left content
:::
::: {.column width="50%"}
Right content
:::
::::
```

### Background color

```markdown
## Important {background-color="#e8f4f8"}
```

---

## Quick Reference

| Feature | Syntax |
|---------|--------|
| Grey inline note | `[text]{.note}` |
| Highlight last item | `::: {.incremental .highlight-last}` |
| Fragment (reveal on click) | `::: {.fragment}` |
| Manual spacing | `## Title {.spaced}` |
| Vertical centering | `## Title {.center}` |
| Hide slide | `{visibility="hidden"}` |
| Uncounted slide | `{visibility="uncounted"}` |
| Smaller text | `{.smaller}` |
| Scrollable | `{.scrollable}` |
| Pause | `. . .` |
| Speaker notes | `::: {.notes}` |
| Two columns (text + visual) | `.column-left` + `.column-right` |
| Two columns (equal) | `{.column width="50%"}` |
| Background color | `{background-color="#hex"}` |

---

## Example Slide

```markdown
## Key Points {.spaced}

::: {.incremental .highlight-last}
- **First insight.** [Supporting detail]{.note}
- **Second insight.** [More context]{.note}
- **Conclusion.**
:::
```
