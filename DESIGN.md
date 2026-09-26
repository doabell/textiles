# Interface system

The source prose and collection records are authoritative. New functional text has at most three
words. No decorative copy.

## Typography

Instrument Sans is used for navigation, controls, data, and section headings. Newsreader is used for
essays, definitions, and artwork titles. Load real roman and italic variable faces locally.

- --type-body: one responsive reading size and 1.62 line height, including every Context paragraph
  and About prose.
- --type-section: shared sans section heading.
- --type-label: shared interface label.
- --reading-width: 62 characters. Large editorial mastheads are separate from reading text. Italics
  identify definitions and artwork titles, not arbitrary paragraphs.

## Images

Preserve original pixels and collection borders. Natural aspect ratios in collection listings;
deliberate focal crops only in editorial features. Waterfall rows follow DOM order, so visual and
keyboard order match. Annotations use the original coordinates and words. Pointer, touch, and
keyboard selection connect the note to its image region. Frame selections update instantly,
without\nshadow animations or changing the caption layout. Image insets never alter the annotation
coordinate\nsystem. Related-work counts scale with their heading rather than using metadata-sized
text.

## Controls

Long lists use searchable checkbox selectors, with removable selections and a clear action. Choices
within a field use OR; different fields narrow the result together. Swatches and map modifiers
expose explicit Match all / Match any modes. Never require modifier keys. Exports use current
filters and units, with descriptive dated filenames. Chart PNGs include the current metric, series
and filter context.

## Navigation

Home appears explicitly in primary navigation. Project routes and downloads are local. Museum,
archive, publication, and license links retain their external destinations.

## Design feels

The header's **Feel** control switches the same content tree between four design recipes. On phones,
open the navigation menu to reach it. Selection is saved locally and can be shared with
`?feel=editorial`, `?feel=gallery`, `?feel=immersive`, or `?feel=folio`. Changing a feel uses
shallow history; it does not navigate, remount an app, or reset its filters. A small bootstrap sets
the choice before first paint. Invalid values fall back to the saved choice, then Editorial.

| Feel      | Typography and composition                                                                              | Collection behavior                                                 |
| --------- | ------------------------------------------------------------------------------------------------------- | ------------------------------------------------------------------- |
| Editorial | Warm paper, mixed Newsreader and Instrument Sans, staggered editorial features                          | Two generous columns, natural image proportions                     |
| Gallery   | White, bold sans headings, sans reading text, split opening, compact ruled evidence rows                | Three glossary columns; four related-work columns on wide screens   |
| Immersive | Dark surfaces, large serif titles, full-screen opening; evidence images on their own with notes below   | Two related-work columns; portraits fit the viewport                |
| Folio     | Rose paper, burgundy ink, Newsreader display titles, 16px Instrument Sans navigation and section labels | Aligned framed artworks, two related-work columns, prominent counts |

Semantic typography and surface tokens live in `src/lib/design/feels.css`; the registry is in
`src/lib/design/feels.ts`. Shared components consume those tokens. Structural recipes are
centralized in the same CSS file rather than duplicated page templates. The original image-viewer
palette, annotation contrast, map geography colors, and portable PNG output are deliberately
independent of the page palette. Source images are not processed or made transparent.

Start new feels with visually inspected museum or gallery references and record which interface
principles are being adapted. To add a feel, add a registry entry and a matching token/recipe block,
then include its identifier in the pre-paint allowlist in `src/app.html`. Keep a single source of
content and data. Check the homepage, glossary, a textile essay, About, and each research app at
desktop and phone widths.

## Museum research

Reviewed September 24 and 26, 2026. These are references for interface principles, not copied
designs or assets.

- [Whitney collection](https://whitney.org/collection/works): visually inspected its bold sans
  hierarchy, fine rules, prominent search, natural-proportion image rows, and labelled filters.
  Gallery uses those principles to make a dense archive easy to scan.
- [Rijksmuseum collection](https://www.rijksmuseum.nl/en/collection): visually inspected the
  full-viewport artwork opening, large title, explicit Home link, and search/filter controls at the
  image edge. Immersive adapts that artwork-first scale to this project's own material.
- [MoMA collection](https://www.moma.org/collection/): reviewed its publicly indexed collection
  structure, artist/work search, on-view filter, dates, and routes into artwork types. Its browser
  security screen prevented direct visual inspection, so no typography claims rely on it.

- [The Met collection](https://www.metmuseum.org/art/collection): visually inspected the desktop
  collection page and computed type styles on September 26. Its navigation and search field use 16px
  proportional sans-serif type; strong headings, clear search and simple rules establish hierarchy.
  Folio applies the readable control scale with Instrument Sans.
- [Courtauld Gallery](https://courtauld.ac.uk/gallery/): visually inspected its split artwork and
  color-panel opening, aligned exhibition rows, readable sans-serif labels, and 18px introductory
  text. Folio adapts the separation of expressive presentation and straightforward navigation, and
  now keeps image plates aligned without rotation or offset shadows.
- [National Gallery collection overview](https://www.nationalgallery.org.uk/paintings/collection-overview):
  visually inspected its calm image-and-text hierarchy and 16px section navigation. Folio uses that
  label scale for Context, Definition, related-textile labels, and About navigation.

Folio's rose palette and Newsreader display type are our interpretation, not the museums' branding.
Its interface uses no monospace. Navigation and section labels are 16px; artwork metadata is 15px.
At narrower widths, the menu replaces the full navigation row rather than reducing its type size.

These references define interface principles. Museum copy, logos, fonts, and artwork are not
imported. Search remains labelled and keyboard-operable in every feel; artwork notes and exports
retain the original record content.

## Scholarly references

Original bracketed reference numbers link to stable note anchors. Each note returns to the exact
reference selected, including references in Related textiles. Native links work without JavaScript;
with JavaScript, focus follows the jump for keyboard and screen-reader users. Colors and target
highlights use the active feel’s tokens. The source prose and numbering remain unchanged.

The footer cites the project’s published address and links to the local homepage. Its local CC badge
and license name link to the license; the surrounding sentence is plain text.
