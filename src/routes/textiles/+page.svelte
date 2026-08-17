<script lang="ts">
    import { ArrowUpRight, Search, X } from "@lucide/svelte";
    import { textiles } from "$lib/data/textiles";
    import { assetPath } from "$lib/utils/asset-path";

    let query = $state("");

    const filtered = $derived(
        textiles.filter((textile) => {
            const terms = [
                textile.name,
                textile.definition,
                ...textile.variants,
                ...textile.related,
            ]
                .join(" ")
                .toLowerCase();
            return terms.includes(query.trim().toLowerCase());
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

<div class="page-shell">
    <header class="page-intro">
        <div>
            <p class="eyebrow">The centerpiece of this project</p>
            <h1>Visual textile glossary</h1>
        </div>
        <p class="lede">
            The centerpiece of this project, the Visual Textile Glossary, provides each historical
            textile term with a short definition and a longer essay contextualizing that textile’s
            production and circulation. Each essay also includes visual and material examples, an
            interactive web application, and open access data.
        </p>
    </header>
</div>

<section class="glossary-controls page-shell" aria-label="Filter glossary">
    <label class="search-box">
        <Search size={18} strokeWidth={1.6} aria-hidden="true" />
        <span class="sr-only">Search textile names, materials, and techniques</span>
        <input type="search" placeholder="Search a name, material, technique…" bind:value={query} />
        {#if query}
            <button type="button" aria-label="Clear search" onclick={() => (query = "")}>
                <X size={16} />
            </button>
        {/if}
    </label>
</section>

<section class="results page-shell">
    <div class="results-meta">
        <p><strong>{filtered.length}</strong> {filtered.length === 1 ? "entry" : "entries"}</p>
        {#if query}
            <button type="button" onclick={clearFilters}>Reset filters</button>
        {/if}
    </div>

    {#if filtered.length}
        <div class="glossary-grid">
            {#each filtered as textile, index}
                <a
                    class:wide={index % 7 === 0}
                    class="entry-card"
                    href={`/textiles/${textile.slug}/`}
                >
                    <figure>
                        <img
                            src={assetPath(textile.image)}
                            alt=""
                            loading={index > 5 ? "lazy" : "eager"}
                        />
                    </figure>
                    <div class="entry-card-copy">
                        <div class="entry-index">{String(index + 1).padStart(2, "0")}</div>
                        <div>
                            <h2>{textile.name}</h2>
                            <span>{textile.definition}</span>
                        </div>
                        <ArrowUpRight
                            class="entry-arrow"
                            size={18}
                            strokeWidth={1.5}
                            aria-hidden="true"
                        />
                    </div>
                </a>
            {/each}
        </div>
    {:else}
        <div class="empty-state">
            <p class="eyebrow">No matching entries</p>
            <button class="button" type="button" onclick={clearFilters}>Show all textiles</button>
        </div>
    {/if}
</section>

<style>
    .sr-only {
        position: absolute;
        width: 1px;
        height: 1px;
        padding: 0;
        overflow: hidden;
        clip: rect(0, 0, 0, 0);
        white-space: nowrap;
        border: 0;
    }

    .page-intro h1 {
        max-width: 9ch;
    }

    .glossary-controls {
        display: grid;
        grid-template-columns: minmax(18rem, 1fr);
        gap: 1rem;
        align-items: end;
        padding-top: 2rem;
        padding-bottom: 2rem;
        border-bottom: 1px solid var(--line);
    }

    .search-box {
        display: flex;
        align-items: center;
        gap: 0.8rem;
        min-height: 3.25rem;
        padding: 0 1rem;
        border: 1px solid var(--line-strong);
        background: var(--cream);
    }

    .search-box input {
        width: 100%;
        border: 0;
        outline: 0;
        background: transparent;
        font-family: var(--serif);
        font-size: 1rem;
    }

    .search-box input::placeholder {
        color: var(--ink-soft);
    }

    .search-box button {
        display: grid;
        place-items: center;
        padding: 0.3rem;
        border: 0;
        background: transparent;
        cursor: pointer;
    }

    .results {
        padding-top: 2rem;
        padding-bottom: clamp(6rem, 10vw, 10rem);
    }

    .results-meta {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 2rem;
    }

    .results-meta p {
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.62rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
    }

    .results-meta strong {
        color: var(--madder);
        font-weight: 700;
    }

    .results-meta button {
        padding: 0;
        color: var(--ink-soft);
        border: 0;
        border-bottom: 1px solid currentColor;
        background: transparent;
        font-size: 0.7rem;
        cursor: pointer;
    }

    .glossary-grid {
        display: grid;
        grid-template-columns: repeat(3, minmax(0, 1fr));
        gap: 1px;
        border: 1px solid var(--line-strong);
        background: var(--line-strong);
    }

    .entry-card {
        min-width: 0;
        background: var(--paper);
        text-decoration: none;
    }

    .entry-card figure {
        position: relative;
        aspect-ratio: 1.15;
        margin: 0;
        overflow: hidden;
        background: var(--paper-deep);
    }

    .entry-card.wide {
        grid-column: span 2;
    }

    .entry-card.wide figure {
        aspect-ratio: 2.3;
    }

    .entry-card img {
        width: 100%;
        height: 100%;
        object-fit: cover;
        filter: saturate(0.82) contrast(0.96);
        transition:
            filter 500ms ease,
            transform 600ms ease;
    }

    .entry-card:hover img {
        filter: saturate(1) contrast(1);
        transform: scale(1.035);
    }

    .entry-card-copy {
        display: grid;
        grid-template-columns: 2rem 1fr 1.5rem;
        gap: 1rem;
        min-height: 13rem;
        padding: 1.2rem;
        border-top: 1px solid var(--line-strong);
    }

    .entry-index {
        color: var(--madder);
        font-family: var(--sans);
        font-size: 0.58rem;
    }

    .entry-card-copy h2 {
        margin-bottom: 0.65rem;
        font-family: var(--serif);
        font-size: clamp(1.55rem, 2.7vw, 2.65rem);
        font-weight: 400;
        letter-spacing: -0.03em;
        line-height: 1;
    }

    .entry-card-copy div > span {
        display: block;
        max-width: 30rem;
        color: var(--ink-soft);
        font-size: 0.74rem;
        line-height: 1.45;
    }

    .entry-card :global(.entry-arrow) {
        transition: transform 180ms ease;
    }

    .entry-card:hover :global(.entry-arrow) {
        transform: translate(0.25rem, -0.25rem);
    }

    .empty-state {
        display: grid;
        place-items: center;
        min-height: 30rem;
        padding: 4rem 1rem;
        border: 1px solid var(--line);
        text-align: center;
    }

    .empty-state .button {
        margin-top: 1rem;
        cursor: pointer;
    }

    @media (max-width: 1000px) {
        .glossary-grid {
            grid-template-columns: repeat(2, minmax(0, 1fr));
        }
    }

    @media (max-width: 650px) {
        .glossary-controls {
            grid-template-columns: 1fr;
        }

        .search-box {
            grid-column: auto;
        }

        .glossary-grid {
            grid-template-columns: 1fr;
        }

        .entry-card.wide {
            grid-column: auto;
        }

        .entry-card.wide figure,
        .entry-card figure {
            aspect-ratio: 1.25;
        }

        .entry-card-copy {
            min-height: 11rem;
        }
    }
</style>
