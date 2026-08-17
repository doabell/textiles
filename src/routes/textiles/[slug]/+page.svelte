<script lang="ts">
    import {
        ArrowLeft,
        ArrowRight,
        ArrowUpRight,
        BookOpenText,
        ExternalLink,
    } from "@lucide/svelte";
    import { textiles } from "$lib/data/textiles";
    import { assetPath } from "$lib/utils/asset-path";
    import type { PageData } from "./$types";

    let { data }: { data: PageData } = $props();

    function relatedLink(name: string) {
        const target = name.toLowerCase();
        return textiles.find(
            (entry) =>
                entry.shortName.toLowerCase() === target ||
                entry.name.toLowerCase().includes(target) ||
                target.includes(entry.shortName.toLowerCase()),
        );
    }
</script>

<svelte:head>
    <title>{data.textile.name} — Visual Textile Glossary</title>
    <meta name="description" content={data.textile.definition} />
</svelte:head>

<article>
    <div class="page-shell entry-breadcrumb">
        <a href="/textiles/"><ArrowLeft size={14} /> Visual textile glossary</a>
        <span>/</span>
        <p>{data.textile.shortName}</p>
    </div>

    <header class={`entry-hero ${data.textile.accent}`}>
        <div class="entry-title">
            <p class="eyebrow">Visual Textile Glossary</p>
            <h1>{data.textile.name}</h1>
            <a
                class="entry-data-link"
                href={`/explore/?textile=${encodeURIComponent(data.textile.dataTerms[0])}`}
            >
                Find this textile in the data <ArrowUpRight size={15} />
            </a>
        </div>

        <figure>
            <img src={assetPath(data.textile.image)} alt="" />
        </figure>
    </header>

    <section class="definition-section page-shell">
        <div class="section-index">
            <span>01</span>
            <p>Definition</p>
        </div>
        <div class="definition-copy">
            <blockquote>{data.textile.definition}</blockquote>
            {#if data.textile.aat}
                <a
                    href={`https://vocab.getty.edu/page/aat/${data.textile.aat}`}
                    target="_blank"
                    rel="noreferrer"
                >
                    Getty AAT · {data.textile.aat}
                    <ExternalLink size={13} />
                </a>
            {/if}
        </div>

        <div class="names-panel">
            <div>
                <h2>Name variants</h2>
                <div class="term-list">
                    {#each data.textile.variants as variant}
                        <span>{variant}</span>
                    {/each}
                </div>
            </div>
            <div>
                <h2>Related textiles</h2>
                <div class="term-list related">
                    {#each data.textile.related as related}
                        {@const linked = relatedLink(related)}
                        {#if linked}
                            <a href={`/textiles/${linked.slug}/`}
                                >{related} <ArrowUpRight size={12} /></a
                            >
                        {:else}
                            <span>{related}</span>
                        {/if}
                    {/each}
                </div>
                {#if data.textile.relatedDescription}
                    <p class="related-description">{data.textile.relatedDescription}</p>
                {/if}
            </div>
        </div>
    </section>

    <section class="essay-section">
        <div class="page-shell essay-grid">
            <div class="section-index">
                <span>02</span>
                <p>Context</p>
            </div>
            <div class="essay-copy">
                <h2>{data.textile.name}</h2>
                {#each data.textile.essay as paragraph, index}
                    <p class:opening={index === 0}>{paragraph}</p>
                {/each}
                {#if data.textile.footnotes?.length}
                    <div class="footnotes">
                        <h3>Notes</h3>
                        <ol>
                            {#each data.textile.footnotes as footnote}
                                <li>{footnote}</li>
                            {/each}
                        </ol>
                    </div>
                {/if}
            </div>
            <aside class="reference-card">
                <figure>
                    <img src={assetPath(data.textile.image)} alt="" />
                </figure>
                {#if data.textile.sourceUrl}
                    <a href={data.textile.sourceUrl} target="_blank" rel="noreferrer">
                        Original glossary entry <ExternalLink size={12} />
                    </a>
                {/if}
            </aside>
        </div>
    </section>

    <section class="data-section">
        <div class="page-shell data-grid">
            <div class="section-index">
                <span>03</span>
                <p>Quantitative record</p>
            </div>
            <div class="data-heading">
                <p class="eyebrow">Textiles, Modifiers, and Values</p>
                <h2>{data.stats.records.toLocaleString()} matching trade records.</h2>
                <p>
                    Explore specific textiles in greater detail, like the quantities, total values,
                    or per-piece values of imported or exported textiles over time or across
                    geographies.
                </p>
            </div>

            {#if data.stats.records > 0}
                <div class="data-stats">
                    <div>
                        <strong>{data.stats.origins}</strong>
                        <span>recorded origin regions</span>
                    </div>
                    <div>
                        <strong>{data.stats.destinations}</strong>
                        <span>recorded destination regions</span>
                    </div>
                    <div>
                        <strong>{data.stats.companies.join(" + ") || "—"}</strong>
                        <span>company archive</span>
                    </div>
                    <div>
                        <strong>
                            {#if data.stats.firstYear && data.stats.lastYear}
                                {data.stats.firstYear}–{data.stats.lastYear}
                            {:else}
                                —
                            {/if}
                        </strong>
                        <span>years represented</span>
                    </div>
                </div>

                {#if data.stats.topDestinations.length}
                    <div class="destination-list">
                        <p>Most frequent destinations by record count</p>
                        {#each data.stats.topDestinations as destination}
                            <div>
                                <span>{destination.name}</span>
                                <i
                                    style={`--size: ${(destination.count / data.stats.topDestinations[0].count) * 100}%`}
                                ></i>
                                <strong>{destination.count}</strong>
                            </div>
                        {/each}
                    </div>
                {/if}
            {:else}
                <div class="no-data">
                    <BookOpenText size={24} strokeWidth={1.4} aria-hidden="true" />
                    <p>No matching trade records.</p>
                </div>
            {/if}

            <a
                class="button data-button"
                href={`/explore/?textile=${encodeURIComponent(data.textile.dataTerms[0])}`}
            >
                Explore matching records <ArrowRight size={16} />
            </a>
        </div>
    </section>

    <nav class="entry-pagination" aria-label="Glossary entries">
        <a href={`/textiles/${data.previous.slug}/`}>
            <ArrowLeft size={18} />
            <span>Previous entry<strong>{data.previous.name}</strong></span>
        </a>
        <a href={`/textiles/${data.next.slug}/`}>
            <span>Next entry<strong>{data.next.name}</strong></span>
            <ArrowRight size={18} />
        </a>
    </nav>
</article>

<style>
    .entry-breadcrumb {
        display: flex;
        align-items: center;
        gap: 0.7rem;
        padding-top: 1rem;
        padding-bottom: 1rem;
        color: var(--ink-soft);
        border-bottom: 1px solid var(--line);
        font-family: var(--sans);
        font-size: 0.57rem;
        letter-spacing: 0.06em;
        text-transform: uppercase;
    }

    .entry-breadcrumb a {
        display: inline-flex;
        align-items: center;
        gap: 0.4rem;
        text-decoration: none;
    }

    .entry-breadcrumb p {
        margin: 0;
        color: var(--ink);
    }

    .entry-hero {
        display: grid;
        grid-template-columns: minmax(0, 0.9fr) minmax(28rem, 1.1fr);
        min-height: min(48rem, calc(100svh - 8rem));
        color: var(--paper);
        background: var(--indigo-deep);
    }

    .entry-hero.madder {
        background: var(--madder-dark);
    }

    .entry-hero.saffron {
        color: var(--ink);
        background: #cba14e;
    }

    .entry-hero.moss {
        background: #46523f;
    }

    .entry-title {
        display: flex;
        flex-direction: column;
        align-items: flex-start;
        justify-content: center;
        padding: clamp(3rem, 7vw, 8rem) var(--page-pad);
    }

    .entry-title .eyebrow {
        color: var(--saffron);
    }

    .saffron .entry-title .eyebrow {
        color: var(--madder-dark);
    }

    h1 {
        max-width: 8ch;
        margin-bottom: 1.5rem;
        font-family: var(--serif);
        font-size: clamp(4rem, 8vw, 9rem);
        font-weight: 400;
        letter-spacing: -0.065em;
        line-height: 0.83;
    }

    .entry-data-link {
        display: inline-flex;
        align-items: center;
        gap: 0.5rem;
        margin-top: 1.5rem;
        font-size: 0.78rem;
        font-weight: 700;
        text-underline-offset: 0.3rem;
    }

    .entry-hero > figure {
        position: relative;
        min-height: 32rem;
        margin: 0;
        overflow: hidden;
        background: var(--paper-deep);
    }

    .entry-hero > figure::after {
        position: absolute;
        inset: 45% 0 0;
        content: "";
        background: linear-gradient(transparent, rgba(0, 0, 0, 0.7));
    }

    .entry-hero > figure img {
        width: 100%;
        height: 100%;
        object-fit: cover;
    }

    .definition-section,
    .essay-grid,
    .data-grid {
        display: grid;
        grid-template-columns: 7rem minmax(0, 1.1fr) minmax(19rem, 0.65fr);
        gap: clamp(2rem, 6vw, 7rem);
    }

    .definition-section {
        padding-top: clamp(5rem, 10vw, 10rem);
        padding-bottom: clamp(5rem, 10vw, 10rem);
    }

    .section-index {
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.7rem;
        font-weight: 620;
        letter-spacing: 0;
    }

    .section-index span {
        display: block;
        margin-bottom: 0.7rem;
        color: var(--madder);
    }

    .section-index p {
        width: max-content;
        margin: 0;
        padding-top: 0.7rem;
        border-top: 1px solid var(--line-strong);
        writing-mode: vertical-rl;
    }

    .definition-copy blockquote {
        max-width: 21ch;
        margin: 0 0 2rem;
        font-family: var(--serif);
        font-size: clamp(2rem, 4vw, 4rem);
        letter-spacing: -0.04em;
        line-height: 1.08;
    }

    .definition-copy > a {
        display: inline-flex;
        align-items: center;
        gap: 0.4rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.6rem;
        text-underline-offset: 0.3rem;
    }

    .names-panel {
        align-self: start;
        border-top: 1px solid var(--line-strong);
    }

    .names-panel > div {
        padding: 1.4rem 0;
        border-bottom: 1px solid var(--line);
    }

    .names-panel h2 {
        margin-bottom: 1rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.72rem;
        font-weight: 650;
        letter-spacing: 0;
    }

    .term-list {
        display: flex;
        flex-wrap: wrap;
        gap: 0.45rem;
    }

    .term-list span,
    .term-list a {
        display: inline-flex;
        align-items: center;
        gap: 0.25rem;
        padding: 0.4rem 0.55rem;
        color: var(--ink-soft);
        border: 1px solid var(--line);
        font-family: var(--serif);
        font-size: 0.76rem;
        line-height: 1;
        text-decoration: none;
    }

    .term-list a:hover {
        color: var(--cream);
        background: var(--ink);
    }

    .related-description {
        margin: 1rem 0 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 0.78rem;
        line-height: 1.55;
    }

    .essay-section {
        padding: clamp(4.5rem, 8vw, 7.5rem) 0;
        background: var(--paper-deep);
    }

    .essay-copy h2 {
        max-width: 12ch;
        margin-bottom: 2.5rem;
        font-family: var(--serif);
        font-size: clamp(3rem, 5vw, 5.5rem);
        font-weight: 400;
        letter-spacing: -0.05em;
        line-height: 0.96;
    }

    .essay-copy > p:not(.eyebrow) {
        max-width: 64ch;
        margin: 0 0 1.15rem;
        color: #3d3c34;
        font-family: var(--reading);
        font-size: clamp(1.03rem, 1.05vw, 1.1rem);
        font-variation-settings: "wght" 410;
        line-height: 1.68;
        text-wrap: pretty;
    }

    .essay-copy > p:last-of-type {
        margin-top: 2rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.76rem;
        font-weight: 620;
    }

    .essay-copy .opening::first-letter {
        float: left;
        margin: 0.08em 0.12em 0 0;
        color: var(--madder);
        font-family: var(--serif);
        font-size: 4.2em;
        line-height: 0.72;
    }

    .footnotes {
        margin-top: 4rem;
        padding-top: 1.5rem;
        border-top: 1px solid var(--line-strong);
    }

    .footnotes h3 {
        margin-bottom: 1rem;
        font-family: var(--sans);
        font-size: 0.76rem;
        font-weight: 650;
        letter-spacing: 0;
    }

    .footnotes ol {
        display: grid;
        gap: 0.8rem;
        margin: 0;
        padding-left: 1.1rem;
    }

    .footnotes li {
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 0.83rem;
        line-height: 1.62;
    }

    .reference-card {
        align-self: start;
        padding-bottom: 1.2rem;
        border-bottom: 1px solid var(--line-strong);
    }

    .reference-card figure {
        aspect-ratio: 1;
        margin: 0 0 1rem;
        overflow: hidden;
        background: #d3c9b8;
    }

    .reference-card img {
        width: 100%;
        height: 100%;
        object-fit: cover;
    }

    .reference-card p {
        margin-bottom: 0.35rem;
        font-family: var(--reading);
        font-size: 0.92rem;
        line-height: 1.4;
    }

    .reference-card a {
        display: inline-flex;
        align-items: center;
        gap: 0.35rem;
        margin-top: 1rem;
        font-size: 0.68rem;
        font-weight: 700;
        text-underline-offset: 0.25rem;
    }

    .data-section {
        padding: clamp(5rem, 10vw, 10rem) 0;
        color: var(--paper);
        background: var(--indigo-deep);
    }

    .data-section .section-index {
        color: rgba(244, 239, 229, 0.5);
    }

    .data-section .section-index span,
    .data-section .eyebrow {
        color: var(--saffron);
    }

    .data-section .section-index p {
        border-color: rgba(244, 239, 229, 0.3);
    }

    .data-heading {
        grid-column: 2 / -1;
        display: grid;
        grid-template-columns: 1fr minmax(17rem, 0.55fr);
        gap: 3rem;
        align-items: end;
    }

    .data-heading .eyebrow {
        grid-column: 1 / -1;
        margin-bottom: -1rem;
    }

    .data-heading h2 {
        margin-bottom: 0;
        font-family: var(--serif);
        font-size: clamp(3rem, 6vw, 6.5rem);
        font-weight: 400;
        letter-spacing: -0.055em;
        line-height: 0.92;
    }

    .data-heading > p:last-child {
        margin: 0;
        color: rgba(244, 239, 229, 0.62);
        font-family: var(--reading);
        font-size: 1rem;
    }

    .data-stats {
        grid-column: 2 / -1;
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        margin-top: 4rem;
        border-top: 1px solid rgba(244, 239, 229, 0.3);
        border-bottom: 1px solid rgba(244, 239, 229, 0.3);
    }

    .data-stats > div {
        padding: 1.5rem;
        border-left: 1px solid rgba(244, 239, 229, 0.18);
    }

    .data-stats > div:first-child {
        border-left: 0;
    }

    .data-stats strong {
        display: block;
        min-height: 2.3rem;
        font-family: var(--serif);
        font-size: clamp(1.6rem, 3vw, 2.8rem);
        font-weight: 400;
        line-height: 1;
    }

    .data-stats span {
        color: rgba(244, 239, 229, 0.52);
        font-family: var(--sans);
        font-size: 0.68rem;
        letter-spacing: 0;
    }

    .destination-list {
        grid-column: 2 / -1;
        margin-top: 3rem;
    }

    .destination-list > p {
        color: rgba(244, 239, 229, 0.5);
        font-family: var(--sans);
        font-size: 0.7rem;
        letter-spacing: 0;
    }

    .destination-list > div {
        display: grid;
        grid-template-columns: minmax(11rem, 0.55fr) 1fr 3rem;
        gap: 1rem;
        align-items: center;
        padding: 0.65rem 0;
    }

    .destination-list span {
        overflow: hidden;
        font-family: var(--serif);
        font-size: 0.85rem;
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    .destination-list i {
        width: var(--size);
        height: 0.45rem;
        background: var(--saffron);
    }

    .destination-list strong {
        color: rgba(244, 239, 229, 0.65);
        font-family: var(--sans);
        font-size: 0.6rem;
        font-weight: 400;
        text-align: right;
    }

    .no-data {
        grid-column: 2 / -1;
        display: flex;
        gap: 1rem;
        align-items: flex-start;
        margin-top: 3rem;
        padding: 1.5rem;
        color: rgba(244, 239, 229, 0.68);
        border: 1px solid rgba(244, 239, 229, 0.2);
    }

    .no-data p {
        max-width: 42rem;
        margin: 0;
        font-family: var(--reading);
    }

    .data-button {
        grid-column: 2;
        justify-self: start;
        margin-top: 3rem;
        color: var(--ink);
        border-color: var(--saffron);
        background: var(--saffron);
    }

    .data-button:hover {
        color: var(--saffron);
        border-color: var(--saffron);
        background: transparent;
    }

    .entry-pagination {
        display: grid;
        grid-template-columns: 1fr 1fr;
    }

    .entry-pagination > a {
        display: flex;
        gap: 1.5rem;
        align-items: center;
        min-height: 10rem;
        padding: 2rem var(--page-pad);
        border-right: 1px solid var(--line-strong);
        text-decoration: none;
        transition:
            color 180ms ease,
            background 180ms ease;
    }

    .entry-pagination > a:last-child {
        justify-content: flex-end;
        border-right: 0;
        text-align: right;
    }

    .entry-pagination > a:hover {
        color: var(--paper);
        background: var(--madder-dark);
    }

    .entry-pagination span {
        display: grid;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.68rem;
        letter-spacing: 0;
    }

    .entry-pagination a:hover span {
        color: rgba(244, 239, 229, 0.62);
    }

    .entry-pagination strong {
        margin-top: 0.35rem;
        color: var(--ink);
        font-family: var(--serif);
        font-size: clamp(1.4rem, 3vw, 2.5rem);
        font-weight: 400;
        letter-spacing: normal;
        line-height: 1;
        text-transform: none;
    }

    .entry-pagination a:hover strong {
        color: var(--paper);
    }

    @media (max-width: 950px) {
        .entry-hero {
            grid-template-columns: 1fr;
            min-height: auto;
        }

        .entry-title {
            min-height: 33rem;
        }

        .entry-hero > figure {
            min-height: 40rem;
        }

        .definition-section,
        .essay-grid,
        .data-grid {
            grid-template-columns: 4rem 1fr;
        }

        .names-panel,
        .reference-card {
            grid-column: 2;
        }

        .data-heading,
        .data-stats,
        .destination-list,
        .no-data {
            grid-column: 2;
        }
    }

    @media (max-width: 650px) {
        .entry-hero > figure {
            min-height: 30rem;
        }

        .definition-section,
        .essay-grid,
        .data-grid {
            display: block;
        }

        .section-index {
            display: none;
        }

        .names-panel,
        .reference-card {
            margin-top: 3rem;
        }

        .data-heading {
            display: block;
        }

        .data-heading .eyebrow {
            margin-bottom: 1rem;
        }

        .data-heading > p:last-child {
            margin-top: 1.5rem;
        }

        .data-stats {
            grid-template-columns: repeat(2, 1fr);
        }

        .data-stats > div:nth-child(3) {
            border-top: 1px solid rgba(244, 239, 229, 0.18);
            border-left: 0;
        }

        .data-stats > div:nth-child(4) {
            border-top: 1px solid rgba(244, 239, 229, 0.18);
        }

        .destination-list > div {
            grid-template-columns: 8rem 1fr 2rem;
        }

        .entry-pagination {
            grid-template-columns: 1fr;
        }

        .entry-pagination > a,
        .entry-pagination > a:last-child {
            min-height: 8rem;
            border-right: 0;
            border-bottom: 1px solid var(--line-strong);
        }
    }
</style>
