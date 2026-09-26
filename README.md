# Dutch Textile Trade

A modern, local-first remake of the Dutch Textile Trade research website. The new SvelteKit front
end brings the project narrative, textile glossary, image archive, downloadable data, and four
redesigned research applications into one responsive site. The original R Shiny applications remain
in the repository for reference.

## Run locally

Install Node.js 26.5.1 or later and Bun 1.3.14 or later, then run:

```sh
bun install
bun run dev
```

Open the local URL printed by Vite. The development server binds only to `127.0.0.1`.

If an editor or local preview proxy replaces that URL with its own hostname, Vite may return
`403 Blocked request`. Allow only that exact hostname for the current shell, then start the server
again:

```powershell
$env:__VITE_ADDITIONAL_SERVER_ALLOWED_HOSTS='the-hostname-from-the-403-message'
bun run dev
```

Direct `127.0.0.1`, `localhost`, and IP-address requests are allowed by default. Avoid allowing
every hostname, because doing so removes Vite's source code protection against DNS rebinding.

Useful commands:

```sh
bun run check       # TypeScript and Svelte diagnostics
bun run audit:copy  # Original content and three-word interface copy
bun run audit:data  # Verify canonical data, calculations, and parser
bun run build       # Generate the static site in build/
bun run audit:site  # Verify local links and assets after building
bun run preview     # Preview the production build locally
```

No deployment or hosting configuration is included.

## What is included

- An editorial homepage and complete project overview
- A searchable glossary with fourteen textile entries
- A zoomable OpenFreeMap route map
- A two-cohort values and modifiers comparison lab
- A client-side explorer for all 31,897 original VOC and WIC app records
- A searchable, paginated archive of 315 textile images with full-size views and comparison tools
- Data, sources, contributors, contact, and methodology pages
- Preserved URLs for the original project features
- Fully prerendered output using SvelteKit's static adapter

The application source is in `src/`. It reads the existing research material directly from `data/`
and `pictures/`, so the legacy data is not duplicated. The social sharing image is in
`static/og.png`.

Published project copy was imported from [dutchtextiletrade.org](https://dutchtextiletrade.org/) and
retained under the project's stated CC BY-NC-SA 4.0 license. Original prose, captions, and research
data stay authoritative. Do not add decorative copy; new functional labels must contain no more than
three words. Run `bun run audit:copy` after changing interface text.

The main research applications are:

- `/map/` — route map, ranked journeys, filters, and export
- `/values/` — unit-aware cohort comparison
- `/swatches/` — visual search, details, and side-by-side comparison
- `/explore/` — the complete chart, route, and record workbench

The interactive map uses OpenFreeMap's Natural Earth raster tiles. If OpenFreeMap cannot be loaded,
the map presents a retry control instead of substituting a different map.

## Design experiments

Use **Feel** in the header (inside the menu on phones) to compare **Editorial**, **Gallery**,
**Immersive**, and **Folio**. The choice persists locally and is shareable through the `feel` URL
parameter. Filters and application state stay in place when switching. [DESIGN.md](DESIGN.md)
documents the museum research, shared tokens, layout recipes, and how to add another feel.

Long filters support type-to-search and checkbox selection. Image listings retain natural aspect
ratios. Original annotation notes select their image regions. Research apps export filtered CSVs and
labelled PNGs; swatch comparisons export the selected original images with their metadata. Downloads
have descriptive dated filenames. Old project links and the primary sources workbook are local;
collection, archive, and publication links retain their external destinations.

## Updating the website data

The trade applications read `data/week4.rds`, the same processed data frame used by the original
Shiny applications in `maps/` and `values/`. The three copies currently match byte for byte and
contain 31,897 records. `src/lib/server/rds.ts` reads their uncompressed XDR version 2 format at
build time and rejects unsupported or damaged files. No R runtime is needed to run or build the
site.

Values and unit prices come directly from the original `data/clean.R` calculations, including the
Indian-to-Dutch guilder conversion. For the 25 records already converted from half pieces to pieces,
the interface uses the calculated quantity unit and retains `originalUnit` in exports.

The map uses representative centers derived from the original `data/geoJSON/` region geometries;
these are regional positions, not port coordinates. Six records for Mauritius have no source
geometry and remain available in the data explorer without invented map positions.

`/data/trade-records.csv` provides all 30 original app columns and all 31,897 records. The original
Excel workbook remains downloadable. The older `pictures/datasets/WIC_VOC_Cleaned.csv` is retained
for legacy reference; its incomplete extract and malformed monetary values are not used by the port.

`static/SRC_Primary.xlsx` is the original primary-source lookup workbook linked from the Sources
page, downloaded unchanged from the
[published workbook](https://dutchtextiletrade.org/wp-content/uploads/2023/03/SRC_Primary.xlsx). It
resolves the archival `src_#####` references and is separate from the trade dataset. The local copy
is 17,814 bytes; SHA-256: `94b6cfeb39ec472a344adceffc534933741d2fce9f11cd4d35f1ec18142a6429`.

The swatch archive is generated from:

- `pictures/datasets/material_gallery.csv`
- `static/gallery/thumbs/` for the paginated result grid
- `static/gallery/full/` for full-size record and comparison views

Those 315 records and source images were imported from
[drewd1231/Textile_Image_Gallery](https://github.com/drewd1231/Textile_Image_Gallery) at commit
`b1b1b62ee3340c200d0890a4fc0e578ce0f70bb1`. Annotated glossary images, captions, and collection
credits are stored under `static/textile-media/` and indexed by
`src/lib/data/original-textile-media.json`; they were recovered from the corresponding published
entries on dutchtextiletrade.org.

After changing source data, run `bun run audit:data`, `bun run check`, and `bun run build`. The data
audit checks source parity, record counts, original calculations, units, geography, CSV round trips,
and parser failures. Update its explicit source expectations when intentionally updating the
research dataset. Keep all three RDS copies synchronized in the uncompressed XDR version 2 format.
SvelteKit will regenerate the static pages, canonical CSV, and bundled archive data.

## License

This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0
International License. Collection images and third-party map data retain the rights and attribution
requirements identified in the interface.

The local [CC BY-NC-SA badge](static/marks/cc-by-nc-sa.svg) is the unmodified SVG from
[Creative Commons’ official downloads](https://mirrors.creativecommons.org/presskit/buttons/88x31/svg/by-nc-sa.svg).

The historical [VOC monogram](https://commons.wikimedia.org/wiki/File:VOC.svg) is public domain. The
[WIC monogram](https://commons.wikimedia.org/wiki/File:Monogram_WIC.svg) is attributed to an unknown
author and shared under CC BY-SA 4.0; the site uses local copies sourced from Wikimedia Commons.

## Legacy Shiny applications

`maps`, `pictures`, and `values` are the original standalone Shiny apps.

- [Maps App](https://dutchtextiletradeapps.shinyapps.io/maps/)
- [Values App](https://dutchtextiletradeapps.shinyapps.io/values/)

To run one, open its `server.R` or `ui.R` in RStudio and select **Run App**.

After updating the Excel spreadsheet under `data`:

1. Source `clean.R`.
2. Replace `week4.rds` and `unitvec.rds` in the relevant app folders.
3. Redeploy and verify the Shiny apps.

## Code of Conduct

Refer to the [Middlebury Handbook](https://www.middlebury.edu/handbook/).
