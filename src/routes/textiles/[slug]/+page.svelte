<script lang="ts">
    import { ArrowLeft, ArrowRight, ArrowUpRight, ArrowUpLeft, BookOpenText } from "@lucide/svelte";
    import TextileMedia from "$lib/components/TextileMedia.svelte";
    import ReferencedText from "$lib/components/ReferencedText.svelte";
    import { referenceParts } from "$lib/utils/footnotes";
    import { originalTextileMedia } from "$lib/data/original-textile-media";
    import { assetPath } from "$lib/utils/asset-path";
    import type { PageData } from "./$types";

    let { data }: { data: PageData } = $props();

    let lastReferences = $state<Record<string, string>>({});
    const firstReferences = $derived.by(() => {
        const references: Record<number, string> = {};
        const passages = [
            { section: "definition", text: data.textile.definition },
            { section: "related", text: data.textile.relatedDescription },
            ...data.textile.essay.map((text, index) => ({ section: "context-" + index, text })),
        ];
        for (const passage of passages) {
            for (const part of referenceParts(
                passage.text,
                `ref-${data.textile.slug}-${passage.section}`,
                data.textile.footnotes.length,
            )) {
                if (part.number && !references[part.number]) references[part.number] = part.id;
            }
        }
        return references;
    });
    const noteId = (number: number) => `note-${data.textile.slug}-${number}`;
    const returnId = (number: number) => lastReferences[noteId(number)] || firstReferences[number];
    function focusAfterJump(id: string, event?: MouseEvent) {
        if (
            event &&
            (event.metaKey || event.ctrlKey || event.altKey || event.shiftKey || event.button !== 0)
        )
            return;
        requestAnimationFrame(() => document.getElementById(id)?.focus({ preventScroll: true }));
    }
    function followNote(number: number, reference: string) {
        lastReferences[noteId(number)] = reference;
        focusAfterJump(noteId(number));
    }
    function noteText(text: string, number: number) {
        const prefix = "[" + number + "]";
        return text.startsWith(prefix) ? text.slice(prefix.length).trimStart() : text;
    }

    const media = $derived(originalTextileMedia[data.textile.slug]);
    const tradeTerm = $derived(encodeURIComponent(data.textile.dataTerms[0]));
    const swatchTerm = $derived(encodeURIComponent(data.textile.shortName));
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

    <header class="entry-hero page-shell">
        <div class="entry-title">
            <h1>{data.textile.name}</h1>
            <a
                class="entry-data-link"
                href={`/explore/?textile=${encodeURIComponent(data.textile.dataTerms[0])}`}
            >
                Explore records <ArrowUpRight size={15} />
            </a>
        </div>

        <figure>
            <img src={assetPath(data.textile.image)} alt="" />
        </figure>
    </header>

    <section class="definition-section page-shell">
        <div class="section-index">
            <h2>Definition</h2>
        </div>
        <div class="definition-copy">
            <blockquote>
                <ReferencedText
                    text={data.textile.definition}
                    scope={data.textile.slug}
                    section="definition"
                    noteCount={data.textile.footnotes.length}
                    onfollow={followNote}
                />
            </blockquote>
        </div>

        <div class="names-panel">
            {#if data.textile.variants.length}
                <div>
                    <h2>Name variants</h2>
                    <div class="term-list">
                        {#each data.textile.variants as variant}
                            <span>{variant}</span>
                        {/each}
                    </div>
                </div>
            {/if}
            {#if data.textile.relatedDescription}
                <div>
                    <h2>Related textiles</h2>
                    <p class="related-description">
                        <ReferencedText
                            text={data.textile.relatedDescription}
                            scope={data.textile.slug}
                            section="related"
                            noteCount={data.textile.footnotes.length}
                            onfollow={followNote}
                        />
                    </p>
                </div>
            {/if}
        </div>
    </section>

    <section class="essay-section">
        <div class="page-shell essay-grid">
            <div class="section-index">
                <h2>Context</h2>
            </div>
            <div class="essay-copy">
                {#each data.textile.essay as paragraph, index}
                    <p>
                        <ReferencedText
                            text={paragraph}
                            scope={data.textile.slug}
                            section={"context-" + index}
                            noteCount={data.textile.footnotes.length}
                            onfollow={followNote}
                        />
                    </p>
                {/each}
                {#if data.textile.footnotes?.length}
                    <section
                        class="footnotes"
                        role="doc-endnotes"
                        aria-labelledby={"notes-" + data.textile.slug}
                    >
                        <h3 id={"notes-" + data.textile.slug}>Notes</h3>
                        <ol role="list">
                            {#each data.textile.footnotes as footnote, index}
                                <li id={noteId(index + 1)} tabindex="-1">
                                    <span class="note-number">[{index + 1}]</span>
                                    <div>
                                        <p class="note-text">{noteText(footnote, index + 1)}</p>
                                        {#if returnId(index + 1)}
                                            <a
                                                class="note-return"
                                                href={"#" + returnId(index + 1)}
                                                role="doc-backlink"
                                                data-sveltekit-reload
                                                onclick={(event) =>
                                                    focusAfterJump(returnId(index + 1), event)}
                                            >
                                                <ArrowUpLeft size={14} aria-hidden="true" /> Back to text
                                            </a>
                                        {/if}
                                    </div>
                                </li>
                            {/each}
                        </ol>
                    </section>
                {/if}
            </div>
            <aside class="reference-card">
                <figure>
                    <img src={assetPath(data.textile.image)} alt="" />
                </figure>
            </aside>
        </div>
    </section>

    {#if media}
        <TextileMedia {media} textileName={data.textile.name} />
    {/if}

    <section class="data-section">
        <div class="page-shell data-grid">
            <div class="section-index">
                <p>Quantitative record</p>
            </div>
            <div class="data-heading">
                <p class="eyebrow">Textiles, Modifiers, and Values</p>
                <h2>{data.stats.records.toLocaleString()} trade records</h2>
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
                        <p>Top destinations</p>
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
                    <p>No trade records</p>
                </div>
            {/if}

            <nav class="research-app-links" aria-label={`Research ${data.textile.shortName}`}>
                <a href={`/explore/?textile=${tradeTerm}`}>
                    <strong>Explore</strong>
                    <ArrowUpRight size={16} />
                </a>
                <a href={`/map/?textile=${tradeTerm}`}>
                    <strong>Map</strong>
                    <ArrowUpRight size={16} />
                </a>
                <a href={`/values/?textile=${tradeTerm}`}>
                    <strong>Compare</strong>
                    <ArrowUpRight size={16} />
                </a>
                <a href={`/swatches/?textile=${swatchTerm}`}>
                    <strong>Swatches</strong>
                    <ArrowUpRight size={16} />
                </a>
            </nav>
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
        flex-wrap: wrap;
        gap: 0.75rem;
        align-items: center;
        padding-top: 1.5rem;
        padding-bottom: 1rem;
        color: var(--ink-soft);
        font: 0.75rem var(--sans);
    }
    .entry-breadcrumb a {
        display: inline-flex;
        align-items: center;
        gap: 0.5rem;
        min-height: 2rem;
        text-decoration: none;
    }
    .entry-breadcrumb a:hover {
        color: var(--madder);
    }
    .entry-breadcrumb p {
        margin: 0;
        color: var(--ink);
    }
    .entry-hero {
        display: grid;
        grid-template-columns: minmax(0, 0.95fr) minmax(0, 1.05fr);
        gap: clamp(2rem, 5vw, 6rem);
        align-items: center;
        padding-top: 2rem;
        padding-bottom: clamp(4rem, 8vw, 8rem);
        min-height: 70svh;
    }
    .entry-title {
        position: relative;
        z-index: 1;
        display: flex;
        flex-direction: column;
        align-items: flex-start;
    }
    h1 {
        max-width: 11ch;
        margin: 0;
        font: 400 clamp(4rem, 8.7vw, 11rem)/0.95 var(--editorial-font);
        letter-spacing: var(--display-tracking, -0.03em);
        overflow-wrap: anywhere;
    }
    .entry-data-link {
        display: inline-flex;
        gap: 1.5rem;
        align-items: center;
        margin-top: 3rem;
        min-height: 2.75rem;
        color: var(--madder);
        font: 500 0.75rem var(--sans);
        text-decoration: none;
        border-bottom: 1px solid currentColor;
    }
    .entry-data-link:hover {
        color: var(--ink);
    }
    .entry-hero > figure {
        display: grid;
        place-items: center;
        min-width: 0;
        margin: 0;
    }
    .entry-hero > figure img {
        display: block;
        width: 100%;
        height: auto;
        max-height: 76svh;
        object-fit: contain;
    }
    .definition-section,
    .essay-grid,
    .data-grid {
        display: grid;
        grid-template-columns: minmax(5rem, 0.3fr) minmax(0, 1.45fr) minmax(13rem, 0.7fr);
        gap: clamp(2rem, 4vw, 5rem);
    }
    .definition-section {
        padding-top: clamp(3rem, 6vw, 6rem);
        padding-bottom: clamp(4rem, 8vw, 8rem);
        border-top: 1px solid var(--ink);
    }
    .section-index h2,
    .section-index p {
        width: fit-content;
        margin: 0;
        padding-top: 0.3rem;
        color: var(--ink-soft);
        font: 500 0.75rem var(--sans);
        letter-spacing: 0;
    }
    .definition-copy blockquote {
        max-width: 29ch;
        margin: 0;
        font: italic 400 clamp(2rem, 3.1vw, 3.6rem)/1.18 var(--editorial-font);
        letter-spacing: -0.025em;
    }
    .names-panel {
        align-self: start;
    }
    .names-panel > div + div {
        margin-top: 2rem;
    }
    .names-panel h2 {
        margin: 0 0 0.9rem;
        color: var(--ink-soft);
        font: 500 0.75rem var(--sans);
        letter-spacing: 0;
    }
    .term-list {
        display: flex;
        flex-wrap: wrap;
        gap: 0.3rem 0.8rem;
    }
    .term-list span {
        color: var(--ink);
        font: 400 1.1rem/1.4 var(--editorial-font);
    }
    .related-description {
        margin: 0;
        color: var(--ink-soft);
        font: 1.05rem/1.6 var(--reading);
    }
    .essay-section {
        padding: clamp(3rem, 5vw, 5rem) 0 clamp(5rem, 9vw, 9rem);
    }
    .essay-copy {
        min-width: 0;
    }
    .essay-copy > p {
        max-width: 62ch;
        margin: 0 0 1.5rem;
        color: var(--ink);
        font: var(--type-body);
        letter-spacing: 0;
        text-wrap: pretty;
    }

    .footnotes {
        margin-top: 4rem;
        padding-top: 1.5rem;
        border-top: 1px solid var(--line-strong);
    }
    .footnotes h3 {
        margin: 0 0 1.25rem;
        font: 500 0.75rem var(--sans);
        letter-spacing: 0;
    }
    .footnotes ol {
        display: grid;
        gap: 1rem;
        margin: 0;
        padding: 0;
        list-style: none;
    }
    .footnotes li {
        display: grid;
        grid-template-columns: 2rem minmax(0, 1fr);
        gap: 0.6rem;
        padding: 1rem 0.75rem;
        border-top: 1px solid var(--line);
        scroll-margin-block: 8rem 3rem;
        min-width: 0;
        overflow-wrap: anywhere;
        color: var(--ink-soft);
        font: 1.03rem/1.6 var(--reading);
    }
    .footnotes li:target,
    .footnotes li:focus-visible {
        background: var(--paper-deep);
        box-shadow: -3px 0 0 var(--madder);
    }
    .note-number {
        padding-top: 0.2rem;
        color: var(--madder);
        font: 600 0.8125rem/1.5 var(--sans);
    }
    .note-text {
        margin: 0;
    }
    .note-return {
        display: inline-flex;
        align-items: center;
        gap: 0.4rem;
        min-height: 2.75rem;
        margin-top: 0.3rem;
        color: var(--madder);
        font: var(--type-label);
        text-underline-offset: 0.2em;
    }
    .note-return:hover {
        color: var(--ink);
    }
    .reference-card {
        position: sticky;
        top: 7rem;
        align-self: start;
    }
    .reference-card figure {
        margin: 0;
    }
    .reference-card img {
        display: block;
        width: 100%;
        max-height: 26rem;
        object-fit: contain;
    }
    .data-section {
        padding: clamp(4rem, 7vw, 8rem) 0;
        color: #f7f5ef;
        background: #171711;
    }
    .data-grid {
        gap: 0 clamp(2rem, 4vw, 5rem);
    }
    .data-section .section-index p {
        color: #b4b4a8;
    }
    .data-heading {
        grid-column: 2 / -1;
        display: grid;
        grid-template-columns: minmax(0, 1.25fr) minmax(15rem, 0.65fr);
        gap: 1.5rem 3rem;
        align-items: end;
    }
    .data-heading .eyebrow {
        grid-column: 1 / -1;
        margin: 0;
        color: #d88976;
        font-size: 0.75rem;
        letter-spacing: 0;
        text-transform: none;
    }
    .data-heading h2 {
        margin: 0;
        max-width: 12ch;
        font: 400 clamp(3rem, 5.5vw, 6.5rem)/0.97 var(--editorial-font);
        letter-spacing: -0.03em;
    }
    .data-heading > p:last-child {
        margin: 0;
        color: #bfbfb3;
        font: 1.15rem/1.6 var(--reading);
    }
    .data-stats {
        grid-column: 2 / -1;
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 1.5rem;
        margin-top: 4rem;
        padding-top: 1.5rem;
        border-top: 1px solid #f7f5ef40;
    }
    .data-stats strong {
        display: block;
        margin-bottom: 0.7rem;
        font: 400 clamp(1.7rem, 2.7vw, 3rem)/1 var(--editorial-font);
    }
    .data-stats span {
        color: #b4b4a8;
        font: 0.75rem var(--sans);
    }
    .destination-list {
        grid-column: 2 / -1;
        margin-top: 3.5rem;
    }
    .destination-list > p {
        margin: 0 0 1.2rem;
        color: #b4b4a8;
        font: 0.75rem var(--sans);
    }
    .destination-list > div {
        display: grid;
        grid-template-columns: minmax(10rem, 0.5fr) 1fr 3rem;
        gap: 1rem;
        align-items: center;
        padding: 0.65rem 0;
    }
    .destination-list span {
        font: 1.15rem var(--editorial-font);
    }
    .destination-list i {
        width: var(--size);
        height: 0.25rem;
        background: #b9402b;
    }
    .destination-list strong {
        color: #d3d3c7;
        font: 0.75rem var(--sans);
        text-align: right;
    }
    .no-data {
        grid-column: 2 / -1;
        display: flex;
        gap: 1rem;
        align-items: center;
        margin-top: 2rem;
        color: #b4b4a8;
    }
    .no-data p {
        margin: 0;
    }
    .research-app-links {
        grid-column: 2 / -1;
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 2rem;
        margin-top: 4rem;
    }
    .research-app-links a {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.6rem;
        min-height: 4rem;
        padding: 0.5rem 0;
        border-bottom: 1px solid #f7f5ef70;
        color: #f7f5ef;
        text-decoration: none;
        transition: color 160ms ease;
    }
    .research-app-links strong {
        font: 400 clamp(1.4rem, 2.5vw, 2.8rem)/1 var(--editorial-font);
        letter-spacing: -0.025em;
    }
    .research-app-links a:hover {
        color: #df8a75;
    }
    .entry-pagination {
        display: grid;
        grid-template-columns: 1fr 1fr;
        padding-inline: var(--page-pad);
    }
    .entry-pagination > a {
        display: flex;
        gap: 1.5rem;
        align-items: center;
        min-width: 0;
        min-height: 14rem;
        padding: 3rem 2rem 3rem 0;
        text-decoration: none;
    }
    .entry-pagination > a:last-child {
        justify-content: flex-end;
        padding: 3rem 0 3rem 2rem;
        text-align: right;
    }
    .entry-pagination span {
        display: grid;
        gap: 0.9rem;
        color: var(--ink-soft);
        font: 0.75rem var(--sans);
    }
    .entry-pagination strong {
        color: var(--ink);
        font: 400 clamp(1.8rem, 3.2vw, 3.8rem)/1.05 var(--editorial-font);
        letter-spacing: -0.03em;
        overflow-wrap: anywhere;
        transition: color 160ms ease;
    }
    .entry-pagination a:hover strong {
        color: var(--madder);
    }
    @media (max-width: 1100px) {
        .definition-section,
        .essay-grid,
        .data-grid {
            grid-template-columns: 4rem minmax(0, 1.4fr) minmax(12rem, 0.65fr);
            gap: 2rem;
        }
        .data-heading {
            grid-template-columns: minmax(0, 1fr);
        }
        .data-heading > p:last-child {
            max-width: 35rem;
        }
    }
    @media (max-width: 850px) {
        .entry-hero {
            gap: 2rem;
            min-height: 0;
            padding-top: 2rem;
        }
        h1 {
            font-size: clamp(3.5rem, 9vw, 6rem);
        }
        .entry-data-link {
            margin-top: 2rem;
        }
        .definition-section,
        .essay-grid,
        .data-grid {
            grid-template-columns: minmax(0, 1.35fr) minmax(12rem, 0.65fr);
            gap: 2rem;
        }
        .section-index {
            grid-column: 1 / -1;
        }
        .data-heading,
        .data-stats,
        .destination-list,
        .no-data,
        .research-app-links {
            grid-column: 1 / -1;
        }
        .definition-copy blockquote {
            font-size: 2.2rem;
        }
        .reference-card {
            top: 6rem;
        }
        .data-stats {
            margin-top: 1.5rem;
        }
        .research-app-links,
        .destination-list {
            margin-top: 1.5rem;
        }
    }
    @media (max-width: 600px) {
        .entry-breadcrumb {
            gap: 0.5rem;
            padding-top: 0.8rem;
            font-size: 0.75rem;
        }
        .entry-hero {
            display: flex;
            flex-direction: column;
            align-items: stretch;
            gap: 2.5rem;
            padding-top: 1.5rem;
        }
        h1 {
            max-width: 12ch;
            font-size: clamp(3.8rem, 18vw, 6rem);
            line-height: 0.98;
        }
        .entry-data-link {
            margin-top: 1.5rem;
        }
        .entry-hero > figure img {
            max-height: 65svh;
        }
        .definition-section,
        .essay-grid,
        .data-grid {
            display: block;
        }
        .section-index {
            margin-bottom: 1.5rem;
        }
        .definition-copy blockquote {
            max-width: 25ch;
            font-size: clamp(2rem, 8.5vw, 2.7rem);
        }
        .names-panel {
            display: grid;
            grid-template-columns: 1fr 1fr;
            gap: 1.5rem;
            margin-top: 2.5rem;
        }
        .names-panel > div + div {
            margin-top: 0;
        }
        .names-panel > div:only-child {
            grid-column: 1 / -1;
        }
        .essay-section {
            padding-top: 1rem;
        }
        .reference-card {
            position: static;
            margin-top: 2rem;
        }

        .reference-card figure {
            display: none;
        }
        .essay-copy > p {
            font-size: var(--text-body);
        }
        .data-heading h2 {
            font-size: clamp(3rem, 13vw, 4.5rem);
        }
        .data-stats {
            grid-template-columns: 1fr 1fr;
            gap: 2rem 1rem;
            margin-top: 2rem;
        }
        .destination-list > div {
            grid-template-columns: minmax(7rem, 1fr) 1fr 2.5rem;
            gap: 0.5rem;
        }
        .research-app-links {
            grid-template-columns: 1fr 1fr;
            gap: 1rem;
        }
        .entry-pagination > a {
            gap: 0.75rem;
            padding-block: 2rem;
            min-height: 10rem;
        }
        .entry-pagination strong {
            font-size: 1.8rem;
        }
    }
    @media (prefers-reduced-motion: reduce) {
        .research-app-links a,
        .entry-pagination strong {
            transition: none;
        }
    }
</style>
