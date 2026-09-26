<script lang="ts">
    import { waterfall } from "$lib/utils/waterfall";
    import { imageSize } from "$lib/utils/image-size";
    import { ArrowUpRight, LayoutGrid, List, Search, X } from "@lucide/svelte";
    import { textiles } from "$lib/data/textiles";
    import { assetPath } from "$lib/utils/asset-path";
    let query = $state("");
    let view = $state<"gallery" | "index">("gallery");
    const filtered = $derived(
        textiles.filter((textile) => {
            const terms = [
                textile.name,
                textile.definition,
                ...textile.variants,
                textile.relatedDescription,
            ]
                .join(" ")
                .toLowerCase();
            return query
                .trim()
                .toLowerCase()
                .split(/\s+/)
                .every((term) => terms.includes(term));
        }),
    );
    function clearFilters() {
        query = "";
    }
</script>

<svelte:head>
    <title>Visual Textile Glossary — Dutch Textile Trade</title>
    <meta
        name="description"
        content="The centerpiece of this project, the Visual Textile Glossary, provides each historical textile term with a short definition and a longer essay contextualizing that textile’s production and circulation. Each essay also includes visual and material examples, an interactive web application, and open access data."
    />
</svelte:head>

<header class="catalogue-intro page-shell">
    <h1>Visual Textile<br /><em>Glossary</em></h1>
    <p>
        The centerpiece of this project, the Visual Textile Glossary, provides each historical
        textile term with a short definition and a longer essay contextualizing that textile’s
        production and circulation. Each essay also includes visual and material examples, an
        interactive web application, and open access data.
    </p>
</header>

<div class="catalogue-toolbar">
    <div class="toolbar-inner page-shell">
        <label class="search-box">
            <Search size={20} strokeWidth={1.5} aria-hidden="true" />
            <span class="sr-only">Search textiles</span>
            <input type="search" placeholder="Search textiles…" bind:value={query} />
            {#if query}<button type="button" aria-label="Clear search" onclick={clearFilters}
                    ><X size={18} /></button
                >{/if}
        </label>
        <span class="result-count" role="status" aria-live="polite"
            >{filtered.length} {filtered.length === 1 ? "entry" : "entries"}</span
        >
        <div class="view-switch" aria-label="Display">
            <button
                type="button"
                aria-label="Gallery view"
                aria-pressed={view === "gallery"}
                onclick={() => (view = "gallery")}
                ><LayoutGrid size={19} strokeWidth={1.5} /></button
            >
            <button
                type="button"
                aria-label="Index view"
                aria-pressed={view === "index"}
                onclick={() => (view = "index")}><List size={21} strokeWidth={1.5} /></button
            >
        </div>
    </div>
</div>

<section class="results page-shell">
    {#if filtered.length}
        <div
            class:as-index={view === "index"}
            class="glossary-grid"
            use:waterfall={view === "gallery"}
        >
            {#each filtered as textile, index}
                <a class="entry-card" href={"/textiles/" + textile.slug + "/"}>
                    <figure>
                        <img
                            {...imageSize(textile.image)}
                            src={assetPath(textile.image)}
                            alt=""
                            loading={index > 3 ? "lazy" : "eager"}
                        />
                    </figure>
                    <div class="entry-copy">
                        <h2>{textile.name}</h2>
                        <p>{textile.definition}</p>
                    </div>
                    <span class="entry-arrow"
                        ><ArrowUpRight size={26} strokeWidth={1.3} aria-hidden="true" /></span
                    >
                </a>
            {/each}
        </div>
    {:else}
        <div class="empty-state">
            <p>No matching entries</p>
            <button class="button" type="button" onclick={clearFilters}>Show all textiles</button>
        </div>
    {/if}
</section>

<style>
    .catalogue-intro {
        display: grid;
        grid-template-columns: 1.2fr 0.8fr;
        align-items: end;
        gap: clamp(2rem, 6vw, 7rem);
        padding-top: clamp(3rem, 6vw, 6rem);
        padding-bottom: clamp(3rem, 6vw, 6rem);
    }
    h1 {
        margin: 0;
        font-family: var(--display-font, var(--sans));
        font-size: clamp(3.7rem, 7.3vw, 7.6rem);
        font-weight: var(--display-weight, 500);
        letter-spacing: var(--display-tracking, -0.045em);
        line-height: 0.99;
    }
    h1 em {
        font-family: var(--editorial-font);
        font-size: 1.12em;
        font-weight: var(--display-weight, 400);
        letter-spacing: var(--display-tracking, -0.035em);
    }
    .catalogue-intro > p {
        max-width: 46ch;
        margin: 0 0 0.3rem;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: clamp(1.1875rem, 1.4vw, 1.375rem);
        line-height: 1.65;
    }
    .catalogue-toolbar {
        position: sticky;
        z-index: 20;
        top: 4.75rem;
        background: var(--paper);
        border-top: 1px solid var(--line-strong);
        border-bottom: 1px solid var(--line-strong);
    }
    .toolbar-inner {
        display: flex;
        align-items: center;
        gap: 2rem;
        min-height: 5.5rem;
    }
    .search-box {
        display: flex;
        flex: 1;
        align-items: center;
        gap: 1rem;
        min-width: 0;
    }
    .search-box input {
        width: 100%;
        padding: 0.8rem 0;
        color: var(--ink);
        border: 0;
        background: transparent;
        font-family: var(--sans);
        font-size: 1rem;
    }
    .search-box input:focus {
        outline-offset: 5px;
    }
    .search-box input::-webkit-search-cancel-button {
        display: none;
    }
    .search-box button {
        display: grid;
        place-items: center;
        min-width: 2.5rem;
        height: 2.5rem;
        border: 0;
        background: transparent;
    }
    .result-count {
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.8rem;
        font-variant-numeric: tabular-nums;
        white-space: nowrap;
    }
    .view-switch {
        display: flex;
        gap: 0.3rem;
        padding-left: 2rem;
        border-left: 1px solid var(--line);
    }
    .view-switch button {
        display: grid;
        place-items: center;
        width: 2.8rem;
        height: 2.8rem;
        color: var(--ink-soft);
        border: 0;
        border-radius: 50%;
        background: transparent;
    }
    .view-switch button[aria-pressed="true"] {
        color: var(--paper);
        background: var(--ink);
    }
    .results {
        padding-top: 3.5rem;
        padding-bottom: clamp(5rem, 10vw, 10rem);
    }
    .glossary-grid {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        align-items: start;
        column-gap: clamp(2rem, 5vw, 5rem);
        row-gap: 4.5rem;
        row-gap: 0;
    }
    .entry-card {
        position: relative;
        display: grid;
        grid-template-columns: 1fr auto;
        min-width: 0;
        text-decoration: none;
        padding-bottom: 3.5rem;
    }
    .entry-card figure {
        display: grid;
        place-items: center;
        grid-column: 1 / -1;
        margin: 0 0 1.5rem;
        padding: 0;
        background: var(--paper-deep);
        overflow: hidden;
    }
    .entry-card img {
        width: 100%;
        height: auto;
        object-fit: contain;
        transition: transform 650ms cubic-bezier(0.2, 0.7, 0.2, 1);
    }
    .entry-card:hover img {
        transform: scale(1.045);
    }
    .entry-copy h2 {
        margin: 0 0 1rem;
        font-family: var(--display-font, var(--sans));
        font-size: clamp(2rem, 3.3vw, 3.5rem);
        font-weight: var(--display-weight, 450);
        letter-spacing: var(--display-tracking, -0.035em);
        line-height: 1.1;
    }
    .entry-copy p {
        max-width: 47ch;
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 1.1875rem;
        line-height: 1.6;
    }
    .entry-arrow {
        display: grid;
        place-items: center;
        width: 3.2rem;
        height: 3.2rem;
        margin-left: 1rem;
        border: 1px solid var(--line-strong);
        border-radius: 50%;
        transition:
            background 200ms,
            color 200ms;
    }
    .entry-card:hover .entry-arrow {
        color: var(--paper);
        background: var(--ink);
    }
    .as-index {
        display: block;
    }
    .as-index .entry-card {
        grid-template-columns: 8rem minmax(0, 1fr) auto;
        align-items: center;
        gap: 2rem;
        margin: 0;
        padding: 1.8rem 0;
        border-bottom: 1px solid var(--line);
        padding-bottom: 1.8rem;
    }
    .as-index .entry-card:first-child {
        padding-top: 0;
    }
    .as-index .entry-card figure {
        grid-column: 1;
        width: 8rem;
        aspect-ratio: 0.9;
        margin: 0;
        padding: 0.5rem;
        height: 8rem;
    }
    .as-index .entry-copy {
        display: grid;
        grid-template-columns: minmax(0, 0.8fr) minmax(0, 1fr);
        align-items: center;
        gap: 2rem;
    }
    .as-index .entry-copy h2 {
        margin: 0;
        font-size: clamp(1.7rem, 2.6vw, 2.8rem);
    }
    .empty-state {
        display: grid;
        justify-items: center;
        align-content: center;
        gap: 1rem;
        min-height: 22rem;
    }
    .empty-state p {
        font-family: var(--editorial-font);
        font-size: 2rem;
    }
    @media (max-width: 800px) {
        .catalogue-intro {
            grid-template-columns: 1fr;
            gap: 2rem;
        }
        h1 {
            font-size: 11vw;
        }
        .catalogue-intro > p {
            max-width: 57ch;
        }
        .glossary-grid {
            column-gap: 1.5rem;
            row-gap: 0;
        }
        .entry-copy h2 {
            font-size: 1.7rem;
        }
        .entry-arrow {
            width: 2.5rem;
            height: 2.5rem;
            margin-left: 0.5rem;
        }
        .entry-copy p {
            font-size: 1.125rem;
        }
        .as-index .entry-copy {
            display: block;
        }
        .as-index .entry-copy h2 {
            margin-bottom: 0.7rem;
        }
    }
    @media (max-width: 560px) {
        .catalogue-intro {
            padding-top: 3rem;
        }
        h1 {
            font-size: 14vw;
        }
        .catalogue-toolbar {
            top: 4.8rem;
        }
        .toolbar-inner {
            gap: 1rem;
            min-height: 4.5rem;
        }
        .search-box {
            gap: 0.6rem;
        }
        .search-box input {
            font-size: 1rem;
            min-width: 0;
        }
        .result-count {
            font-size: 0.75rem;
        }
        .view-switch {
            gap: 0;
            padding-left: 0.7rem;
        }
        .view-switch button {
            width: 2.3rem;
            height: 2.7rem;
        }
        .glossary-grid {
            grid-template-columns: 1fr;
            row-gap: 3.5rem;
            row-gap: 0;
        }
        .entry-copy h2 {
            font-size: 2rem;
        }
        .results {
            padding-top: 2rem;
        }
        .as-index .entry-card {
            grid-template-columns: 4.5rem minmax(0, 1fr) auto;
            gap: 0.8rem;
            padding-bottom: 1.8rem;
        }
        .as-index .entry-card figure {
            width: 4.5rem;
            height: 8rem;
        }
        .as-index .entry-copy h2 {
            font-size: 1.35rem;
            margin: 0;
        }
        .as-index .entry-copy p {
            display: none;
        }
    }
</style>
