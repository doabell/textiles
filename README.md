# Dutch Textile Trade

A modern, local-first remake of the Dutch Textile Trade research website. The new SvelteKit front
end brings the project narrative, textile glossary, image archive, downloadable data, and four
redesigned research applications into one responsive site. The original R Shiny applications remain
in the repository for reference.

## Run locally

Install a current Node.js LTS release, then run:

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
bun run check     # TypeScript and Svelte diagnostics
bun run build     # Generate the static site in build/
bun run preview   # Preview the production build locally
```

No deployment or hosting configuration is included.

## What is included

- An editorial homepage and complete project overview
- A searchable glossary with fourteen textile entries
- A zoomable OpenFreeMap route map
- A two-cohort values and modifiers comparison lab
- A client-side explorer for 9,463 VOC and WIC trade records
- A searchable, paginated archive of 315 textile images with full-size views and comparison tools
- Data, sources, contributors, contact, and methodology pages
- Preserved URLs for the original project features
- Fully prerendered output using SvelteKit's static adapter

The application source is in `src/`. It reads the existing research material directly from `data/`
and `pictures/`, so the legacy data is not duplicated. The social sharing image is in
`static/og.png`.

Published project copy was imported from [dutchtextiletrade.org](https://dutchtextiletrade.org/) and
retained under the project's stated CC BY-NC-SA 4.0 license. Interface labels for the redesigned
tools remain local to this implementation.

The main research applications are:

- `/map/` — route map, ranked journeys, filters, and export
- `/values/` — unit-aware cohort comparison
- `/swatches/` — visual search, details, and side-by-side comparison
- `/explore/` — the complete chart, route, and record workbench

The interactive map uses OpenFreeMap's Natural Earth raster tiles. If OpenFreeMap cannot be loaded,
the map presents a retry control instead of substituting a different map.

## Updating the website data

The trade explorer is generated from:

`pictures/datasets/WIC_VOC_Cleaned.csv`

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

After changing source data, run `bun run check` and `bun run build`. SvelteKit will regenerate the
static pages and bundled archive data.

## License

This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0
International License. Collection images and third-party map data retain the rights and attribution
requirements identified in the interface.

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
