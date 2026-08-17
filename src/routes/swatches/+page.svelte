<script lang="ts">
    import {
        ArrowUpRight,
        Check,
        GitCompareArrows,
        Info,
        RotateCcw,
        Search,
        X,
    } from "@lucide/svelte";
    import { archiveItems, archiveOptions, type ArchiveItem } from "$lib/data/archive";
    import ResearchAppHeader from "$lib/components/ResearchAppHeader.svelte";
    import { assetPath } from "$lib/utils/asset-path";

    let view = $state<"all" | "sample" | "painting">("all");
    let query = $state("");
    let color = $state("");
    let pattern = $state("");
    let process = $state("");
    let fiber = $state("");
    let detailItem = $state<ArchiveItem | null>(null);
    let comparison = $state<ArchiveItem[]>([]);
    let comparisonOpen = $state(false);

    const filtered = $derived.by(() =>
        archiveItems.filter((item) => {
            const terms = [
                item.title,
                item.artist,
                item.textile,
                item.primaryColor,
                item.secondaryColor,
                item.pattern,
                item.process,
                item.weave,
                item.fiber,
                item.geography,
                item.collection,
                item.date,
            ]
                .join(" ")
                .toLocaleLowerCase("en");

            return (
                (view === "all" || item.type === view) &&
                (!query || terms.includes(query.toLocaleLowerCase("en").trim())) &&
                (!color || item.primaryColor === color || item.secondaryColor === color) &&
                (!pattern || item.pattern === pattern) &&
                (!process || item.process === process) &&
                (!fiber || item.fiber === fiber)
            );
        }),
    );

    const activeFilters = $derived(
        [query, color, pattern, process, fiber].filter(Boolean).length + (view !== "all" ? 1 : 0),
    );

    function resetFilters() {
        view = "all";
        query = "";
        color = "";
        pattern = "";
        process = "";
        fiber = "";
    }

    function toggleCompare(item: ArchiveItem) {
        if (comparison.some((entry) => entry.id === item.id)) {
            comparison = comparison.filter((entry) => entry.id !== item.id);
            if (comparison.length < 2) comparisonOpen = false;
            return;
        }

        if (comparison.length < 2) comparison = [...comparison, item];
    }

    function isCompared(item: ArchiveItem) {
        return comparison.some((entry) => entry.id === item.id);
    }

    function comparisonSlot(item: ArchiveItem) {
        return comparison.findIndex((entry) => entry.id === item.id);
    }

    function clearComparison() {
        comparison = [];
        comparisonOpen = false;
    }

    function handleKeydown(event: KeyboardEvent) {
        if (event.key === "Escape") {
            if (comparisonOpen) comparisonOpen = false;
            else detailItem = null;
        }
    }
</script>

<svelte:window onkeydown={handleKeydown} />

<svelte:head>
    <title>Swatch Search — Dutch Textile Trade</title>
    <meta
        name="description"
        content="Explore our growing database of textile samples and swatches by textile name (if you know it) or (if you don’t) search by attributes (modifiers) like color, pattern, process, weave structure, and fiber."
    />
</svelte:head>

<ResearchAppHeader active="swatch-search" />

<section class="archive-app" aria-label="Filter the visual archive">
    <div class="archive-panel page-shell">
        <header class="filter-heading">
            <div>
                <Search size={18} />
                <h2>Filters</h2>
            </div>
            {#if activeFilters}
                <button type="button" onclick={resetFilters}><RotateCcw size={14} /> Reset</button>
            {/if}
        </header>

        <div class="archive-toolbar">
            <div class="view-switcher" role="group" aria-label="Record type">
                <button class:active={view === "all"} type="button" onclick={() => (view = "all")}>
                    All records
                </button>
                <button
                    class:active={view === "sample"}
                    type="button"
                    onclick={() => (view = "sample")}
                >
                    Material samples
                </button>
                <button
                    class:active={view === "painting"}
                    type="button"
                    onclick={() => (view = "painting")}
                >
                    Pictured textiles
                </button>
            </div>

            <label class="archive-search">
                <Search size={17} strokeWidth={1.5} />
                <span class="sr-only">Search the visual archive</span>
                <input
                    type="search"
                    placeholder="Search names, makers, collections…"
                    bind:value={query}
                />
                {#if query}
                    <button type="button" aria-label="Clear search" onclick={() => (query = "")}>
                        <X size={15} />
                    </button>
                {/if}
            </label>

            <div class="select-filters">
                <label>
                    <span>Color</span>
                    <select bind:value={color}>
                        <option value="">All colors</option>
                        {#each archiveOptions.colors as item}
                            <option value={item}>{item}</option>
                        {/each}
                    </select>
                </label>
                <label>
                    <span>Pattern</span>
                    <select bind:value={pattern}>
                        <option value="">All patterns</option>
                        {#each archiveOptions.patterns as item}
                            <option value={item}>{item}</option>
                        {/each}
                    </select>
                </label>
                <label>
                    <span>Process</span>
                    <select bind:value={process}>
                        <option value="">All processes</option>
                        {#each archiveOptions.processes as item}
                            <option value={item}>{item}</option>
                        {/each}
                    </select>
                </label>
                <label>
                    <span>Fiber</span>
                    <select bind:value={fiber}>
                        <option value="">All fibers</option>
                        {#each archiveOptions.fibers as item}
                            <option value={item}>{item}</option>
                        {/each}
                    </select>
                </label>
            </div>
        </div>
    </div>
</section>

<section class:comparison-active={comparison.length > 0} class="archive-results page-shell">
    <div class="results-head">
        <p>
            <strong>{filtered.length}</strong> visual {filtered.length === 1 ? "record" : "records"}
        </p>
        <div>
            {#if comparison.length}<span>{comparison.length}/2 selected</span>{/if}
        </div>
    </div>

    {#if filtered.length}
        <div class="archive-grid">
            {#each filtered as item, index}
                <article class:chosen={isCompared(item)} class="archive-card">
                    <button
                        class="image-button"
                        type="button"
                        aria-label="View record"
                        onclick={() => (detailItem = item)}
                    >
                        <img
                            src={assetPath(item.image)}
                            alt={item.title}
                            loading={index > 10 ? "lazy" : "eager"}
                        />
                        <span class={`record-type ${item.type}`}
                            >{item.type === "sample" ? "Material" : "Pictorial"}</span
                        >
                        <i><Info size={16} /> View record</i>
                    </button>
                    <div class="card-copy">
                        <div>
                            <p>{item.date || "Date unknown"} · {item.collection}</p>
                            <h2>{item.title}</h2>
                            {#if item.artist}<span>{item.artist}</span>{/if}
                        </div>
                        <button
                            class:active={isCompared(item)}
                            class:slot-b={comparisonSlot(item) === 1}
                            class="compare-button"
                            type="button"
                            disabled={comparison.length >= 2 && !isCompared(item)}
                            aria-label={isCompared(item)
                                ? "Remove from comparison"
                                : "Add to comparison"}
                            onclick={() => toggleCompare(item)}
                        >
                            {#if isCompared(item)}
                                <span class="slot-label"
                                    >{comparisonSlot(item) === 0 ? "A" : "B"}</span
                                >
                                <Check size={14} />
                            {:else}
                                <GitCompareArrows size={14} />
                            {/if}
                            {isCompared(item)
                                ? "Selected"
                                : comparison.length >= 2
                                  ? "Comparison full"
                                  : "Compare"}
                        </button>
                    </div>
                </article>
            {/each}
        </div>
    {:else}
        <div class="empty-state">
            <Search size={25} strokeWidth={1.3} />
            <h2>No results</h2>
            <button class="button" type="button" onclick={resetFilters}>Show all records</button>
        </div>
    {/if}
</section>

{#if comparison.length}
    <aside class="compare-dock" aria-label="Comparison selection">
        <div class="compare-dock-inner page-shell">
            <div class="dock-title">
                <GitCompareArrows size={18} />
                <strong>Comparison Tool</strong>
            </div>
            <div class="dock-slots">
                {#each [0, 1] as slot}
                    {@const item = comparison[slot]}
                    {#if item}
                        <div class="dock-record">
                            <span class="dock-letter">{slot === 0 ? "A" : "B"}</span>
                            <img src={assetPath(item.image)} alt="" />
                            <strong>{item.title}</strong>
                            <button
                                type="button"
                                aria-label="Remove from comparison"
                                onclick={() => toggleCompare(item)}
                            >
                                <X size={15} />
                            </button>
                        </div>
                    {:else}
                        <div class="dock-record empty"><span class="dock-letter">B</span></div>
                    {/if}
                {/each}
            </div>
            <button
                class="open-comparison"
                type="button"
                disabled={comparison.length !== 2}
                onclick={() => (comparisonOpen = true)}
            >
                Compare selected
            </button>
            <button
                class="clear-comparison"
                type="button"
                aria-label="Clear comparison"
                onclick={clearComparison}
            >
                <X size={17} />
            </button>
        </div>
    </aside>
{/if}

<aside class="image-note">
    <div class="page-shell">
        <p class="eyebrow">A note about image quality</p>
        <p>
            We have tried to include the highest quality, openly available images, but even with
            good institutional digitization efforts, many of our swatches are small so will appear
            blurry or pixelated.
        </p>
    </div>
</aside>

{#if comparisonOpen && comparison.length === 2}
    {@const first = comparison[0]}
    {@const second = comparison[1]}
    <div
        class="compare-modal-backdrop"
        role="presentation"
        onclick={(event) => {
            if (event.currentTarget === event.target) comparisonOpen = false;
        }}
    >
        <div
            class="compare-modal"
            role="dialog"
            aria-modal="true"
            aria-labelledby="comparison-title"
            tabindex="-1"
        >
            <header>
                <div>
                    <p class="eyebrow">Comparing two textiles</p>
                    <h2 id="comparison-title">Comparison Tool</h2>
                </div>
                <button
                    type="button"
                    aria-label="Close comparison"
                    onclick={() => (comparisonOpen = false)}
                >
                    <X size={20} />
                </button>
            </header>

            <div class="compare-images">
                {#each [first, second] as item, index}
                    <article>
                        <div class={`compare-letter ${index === 0 ? "a" : "b"}`}>
                            {index === 0 ? "A" : "B"}
                        </div>
                        <figure><img src={assetPath(item.image)} alt={item.title} /></figure>
                        <div>
                            <h3>{item.title}</h3>
                            {#if item.artist}<p>{item.artist}, {item.date}</p>{/if}
                        </div>
                    </article>
                {/each}
            </div>

            <div class="comparison-table">
                <table>
                    <thead>
                        <tr><th>Record</th><th>A</th><th>B</th></tr>
                    </thead>
                    <tbody>
                        <tr
                            ><th>Textile name</th><td>{first.textile || "—"}</td><td
                                >{second.textile || "—"}</td
                            ></tr
                        >
                        <tr
                            ><th>Colors</th><td
                                >{[first.primaryColor, first.secondaryColor]
                                    .filter(Boolean)
                                    .join(", ") || "—"}</td
                            ><td
                                >{[second.primaryColor, second.secondaryColor]
                                    .filter(Boolean)
                                    .join(", ") || "—"}</td
                            ></tr
                        >
                        <tr
                            ><th>Pattern</th><td>{first.pattern || "—"}</td><td
                                >{second.pattern || "—"}</td
                            ></tr
                        >
                        <tr
                            ><th>Process</th><td>{first.process || "—"}</td><td
                                >{second.process || "—"}</td
                            ></tr
                        >
                        <tr
                            ><th>Weave</th><td>{first.weave || "—"}</td><td
                                >{second.weave || "—"}</td
                            ></tr
                        >
                        <tr
                            ><th>Fiber</th><td>{first.fiber || "—"}</td><td
                                >{second.fiber || "—"}</td
                            ></tr
                        >
                        <tr
                            ><th>Catalogue geography</th><td>{first.geography || "—"}</td><td
                                >{second.geography || "—"}</td
                            ></tr
                        >
                        <tr
                            ><th>Collection</th><td>{first.collection || "—"}</td><td
                                >{second.collection || "—"}</td
                            ></tr
                        >
                        <tr
                            ><th>Inventory</th><td>{first.inventory || "—"}</td><td
                                >{second.inventory || "—"}</td
                            ></tr
                        >
                    </tbody>
                </table>
            </div>
        </div>
    </div>
{/if}

{#if detailItem}
    <div
        class="modal-backdrop"
        role="presentation"
        onclick={(event) => {
            if (event.currentTarget === event.target) detailItem = null;
        }}
    >
        <div class="record-modal" role="dialog" aria-modal="true" aria-labelledby="record-title">
            <button
                class="modal-close"
                type="button"
                aria-label="Close record"
                onclick={() => (detailItem = null)}
            >
                <X size={19} />
            </button>
            <figure>
                <img src={assetPath(detailItem.image)} alt={detailItem.title} />
            </figure>
            <div class="modal-copy">
                <p class="eyebrow">
                    {detailItem.type === "sample" ? "Material record" : "Pictorial record"}
                </p>
                <h2 id="record-title">{detailItem.title}</h2>
                {#if detailItem.artist}<p class="artist">
                        {detailItem.artist}, {detailItem.date}
                    </p>{/if}
                <dl>
                    <div>
                        <dt>Textile name</dt>
                        <dd>{detailItem.textile}</dd>
                    </div>
                    <div>
                        <dt>Colors</dt>
                        <dd>
                            {[detailItem.primaryColor, detailItem.secondaryColor]
                                .filter(Boolean)
                                .join(", ") || "Not recorded"}
                        </dd>
                    </div>
                    <div>
                        <dt>Pattern</dt>
                        <dd>{detailItem.pattern || "Not recorded"}</dd>
                    </div>
                    <div>
                        <dt>Process</dt>
                        <dd>{detailItem.process || "Not recorded"}</dd>
                    </div>
                    {#if detailItem.weave}<div>
                            <dt>Weave</dt>
                            <dd>{detailItem.weave}</dd>
                        </div>{/if}
                    {#if detailItem.fiber}<div>
                            <dt>Fiber</dt>
                            <dd>{detailItem.fiber}</dd>
                        </div>{/if}
                    {#if detailItem.geography}<div>
                            <dt>Catalogue geography</dt>
                            <dd>{detailItem.geography}</dd>
                        </div>{/if}
                    <div>
                        <dt>Collection</dt>
                        <dd>{detailItem.collection}</dd>
                    </div>
                    <div>
                        <dt>Inventory</dt>
                        <dd>{detailItem.inventory || "Not recorded"}</dd>
                    </div>
                </dl>
                <div class="modal-actions">
                    <button
                        class:active={isCompared(detailItem)}
                        class="button secondary"
                        type="button"
                        disabled={comparison.length >= 2 && !isCompared(detailItem)}
                        onclick={() => {
                            if (detailItem) toggleCompare(detailItem);
                        }}
                    >
                        <GitCompareArrows size={15} />
                        {isCompared(detailItem)
                            ? "Remove from comparison"
                            : comparison.length >= 2
                              ? "Comparison full"
                              : "Add to comparison"}
                    </button>
                    {#if detailItem.catalogueUrl && detailItem.catalogueUrl.startsWith("http")}
                        <a
                            class="button"
                            href={detailItem.catalogueUrl}
                            target="_blank"
                            rel="noreferrer"
                        >
                            Collection record <ArrowUpRight size={15} />
                        </a>
                    {/if}
                </div>
            </div>
        </div>
    </div>
{/if}

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

    .archive-app {
        padding: 1.25rem 0 2rem;
        color: var(--paper);
        background: var(--ink);
    }

    .archive-panel {
        padding-top: 1.2rem;
        padding-bottom: 1.2rem;
        background: #22221d;
        border: 1px solid rgba(244, 239, 229, 0.14);
    }

    .filter-heading,
    .filter-heading > div {
        display: flex;
        gap: 0.55rem;
        align-items: center;
        justify-content: space-between;
    }

    .filter-heading > div {
        justify-content: flex-start;
    }

    .filter-heading h2 {
        margin: 0;
        font-family: var(--sans);
        font-size: 0.96rem;
        font-weight: 650;
        letter-spacing: -0.015em;
    }

    .filter-heading button {
        display: inline-flex;
        gap: 0.35rem;
        align-items: center;
        padding: 0;
        color: rgba(244, 239, 229, 0.66);
        border: 0;
        background: none;
        font-size: 0.72rem;
        cursor: pointer;
    }

    .archive-toolbar {
        display: grid;
        grid-template-columns: auto minmax(18rem, 1fr);
        gap: 1rem;
        margin-top: 1.2rem;
        padding-top: 1.2rem;
        border-top: 1px solid rgba(244, 239, 229, 0.14);
    }

    .archive-toolbar > *,
    .select-filters label,
    .select-filters select {
        min-width: 0;
    }

    .view-switcher {
        display: flex;
        overflow: hidden;
        border-radius: 0.4rem;
        border: 1px solid rgba(244, 239, 229, 0.25);
    }

    .view-switcher button {
        min-height: 2.85rem;
        padding: 0.7rem 0.9rem;
        color: rgba(244, 239, 229, 0.68);
        border: 0;
        border-left: 1px solid rgba(244, 239, 229, 0.16);
        background: transparent;
        font-size: 0.75rem;
        font-weight: 700;
        cursor: pointer;
    }

    .view-switcher button:first-child {
        border-left: 0;
    }

    .view-switcher button.active {
        color: var(--ink);
        background: var(--paper);
    }

    .archive-search {
        display: flex;
        align-items: center;
        gap: 0.7rem;
        min-height: 2.85rem;
        padding: 0 0.9rem;
        color: var(--paper);
        border: 1px solid rgba(244, 239, 229, 0.25);
        border-radius: 0.38rem;
        background: #171713;
    }

    .archive-search input {
        width: 100%;
        border: 0;
        outline: 0;
        color: var(--paper);
        background: transparent;
        font-size: 0.82rem;
    }

    .archive-search button {
        display: grid;
        place-items: center;
        padding: 0.3rem;
        color: var(--paper);
        border: 0;
        background: transparent;
        cursor: pointer;
    }

    .select-filters {
        grid-column: 1 / -1;
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 1rem;
    }

    .select-filters label > span {
        display: block;
        margin-bottom: 0.4rem;
        color: rgba(244, 239, 229, 0.55);
        font-family: var(--sans);
        font-size: 0.7rem;
        font-weight: 560;
        letter-spacing: 0;
    }

    .select-filters select {
        width: 100%;
        min-height: 2.85rem;
        padding: 0.65rem 2.2rem 0.65rem 0.78rem;
        color: var(--paper);
        border: 1px solid rgba(244, 239, 229, 0.25);
        border-radius: 0.55rem;
        outline: 0;
        background: #171713;
        background-image:
            linear-gradient(45deg, transparent 50%, rgba(244, 239, 229, 0.7) 50%),
            linear-gradient(135deg, rgba(244, 239, 229, 0.7) 50%, transparent 50%);
        background-repeat: no-repeat;
        background-position:
            calc(100% - 1rem) 52%,
            calc(100% - 0.7rem) 52%;
        background-size:
            0.32rem 0.32rem,
            0.32rem 0.32rem;
        appearance: none;
        box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.035);
        font-size: 0.8rem;
    }

    .archive-results {
        padding-top: 1.8rem;
        padding-bottom: clamp(6rem, 10vw, 10rem);
    }

    .archive-results.comparison-active {
        padding-bottom: clamp(11rem, 16vw, 15rem);
    }

    .results-head {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 1.8rem;
    }

    .results-head p {
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.78rem;
    }

    .results-head strong {
        color: var(--madder);
    }

    .results-head > div {
        display: flex;
        gap: 1rem;
        align-items: center;
    }

    .results-head > div > span {
        color: var(--ink-soft);
        font-size: 0.72rem;
        font-weight: 650;
    }

    .archive-grid {
        columns: 3 19rem;
        column-gap: 1px;
        background: var(--line-strong);
        border: 1px solid var(--line-strong);
    }

    .archive-card {
        position: relative;
        display: inline-block;
        min-width: 0;
        width: 100%;
        margin: 0 0 1px;
        overflow: hidden;
        break-inside: avoid;
        background: var(--paper);
    }

    .archive-card.chosen {
        box-shadow: inset 0 0 0 3px var(--indigo);
    }

    .image-button {
        position: relative;
        display: block;
        width: 100%;
        padding: 0;
        overflow: hidden;
        border: 0;
        background: #d3c9b8;
        cursor: zoom-in;
    }

    .image-button img {
        width: 100%;
        height: auto;
        filter: saturate(0.82);
        transition:
            filter 400ms ease,
            transform 500ms ease;
    }

    .image-button:hover img {
        filter: saturate(1);
        transform: scale(1.025);
    }

    .record-type {
        position: absolute;
        top: 0.65rem;
        left: 0.65rem;
        padding: 0.32rem 0.45rem;
        color: var(--paper);
        background: var(--indigo-deep);
        font-family: var(--sans);
        font-size: 0.58rem;
        letter-spacing: 0.07em;
        text-transform: uppercase;
    }

    .record-type.painting {
        background: var(--madder-dark);
    }

    .image-button i {
        position: absolute;
        right: 0.65rem;
        bottom: 0.65rem;
        display: flex;
        gap: 0.35rem;
        align-items: center;
        padding: 0.4rem 0.55rem;
        color: var(--paper);
        background: rgba(23, 23, 17, 0.82);
        font-family: var(--sans);
        font-size: 0.66rem;
        font-style: normal;
        font-weight: 700;
        opacity: 0;
        transform: translateY(0.3rem);
        transition:
            opacity 180ms ease,
            transform 180ms ease;
    }

    .image-button:hover i,
    .image-button:focus-visible i {
        opacity: 1;
        transform: translateY(0);
    }

    .card-copy {
        display: grid;
        grid-template-columns: minmax(0, 1fr) auto;
        gap: 0.8rem;
        align-items: end;
        min-width: 0;
        padding: 1rem;
        overflow: hidden;
        border-top: 1px solid var(--line-strong);
    }

    .card-copy > div {
        min-width: 0;
        overflow: hidden;
    }

    .card-copy p {
        display: -webkit-box;
        margin-bottom: 0.55rem;
        overflow: hidden;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 0.7rem;
        letter-spacing: 0;
        line-height: 1.35;
        overflow-wrap: anywhere;
        -webkit-box-orient: vertical;
        -webkit-line-clamp: 2;
        line-clamp: 2;
    }

    .card-copy h2 {
        margin-bottom: 0.25rem;
        font-family: var(--serif);
        font-size: 1.12rem;
        font-weight: 400;
        line-height: 1.16;
        overflow-wrap: anywhere;
    }

    .card-copy div > span {
        display: block;
        color: var(--ink-soft);
        font-size: 0.72rem;
    }

    .compare-button {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 0.4rem;
        width: auto;
        min-height: 2.25rem;
        padding: 0.55rem 0.75rem;
        color: var(--ink-soft);
        border: 1px solid var(--line);
        background: transparent;
        font-size: 0.7rem;
        font-weight: 700;
        cursor: pointer;
    }

    .compare-button.active {
        color: var(--paper);
        border-color: var(--indigo);
        background: var(--indigo);
    }

    .compare-button.active.slot-b {
        border-color: var(--madder);
        background: var(--madder);
    }

    .compare-button:disabled {
        cursor: not-allowed;
        opacity: 0.42;
    }

    .slot-label {
        display: grid;
        place-items: center;
        width: 1.25rem;
        height: 1.25rem;
        color: var(--indigo);
        background: var(--paper);
        font-size: 0.68rem;
        font-weight: 800;
    }

    .empty-state {
        display: grid;
        place-items: center;
        align-content: center;
        min-height: 32rem;
        color: var(--ink-soft);
        border: 1px solid var(--line);
        text-align: center;
    }

    .empty-state h2 {
        max-width: 17ch;
        margin: 1rem 0 0.5rem;
        color: var(--ink);
        font-family: var(--serif);
        font-size: clamp(2.4rem, 5vw, 4.5rem);
        font-weight: 400;
        line-height: 1;
    }

    .empty-state .button {
        margin-top: 1.5rem;
        cursor: pointer;
    }

    .compare-dock {
        position: fixed;
        z-index: 70;
        right: 0;
        bottom: 0;
        left: 0;
        padding: 0.75rem 0;
        color: var(--paper);
        background: color-mix(in srgb, var(--ink) 95%, transparent);
        border-top: 1px solid rgba(244, 239, 229, 0.22);
        box-shadow: 0 -1rem 3rem rgba(0, 0, 0, 0.24);
        backdrop-filter: blur(16px);
    }

    .compare-dock-inner {
        display: grid;
        grid-template-columns: auto minmax(0, 1fr) auto auto;
        gap: 0.75rem;
        align-items: center;
    }

    .dock-title {
        display: flex;
        gap: 0.55rem;
        align-items: center;
        padding-right: 0.8rem;
    }

    .dock-title strong {
        font-family: var(--serif);
        font-size: 1rem;
        font-weight: 400;
        white-space: nowrap;
    }

    .dock-slots {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 0.55rem;
    }

    .dock-record {
        display: grid;
        grid-template-columns: auto 3rem minmax(0, 1fr) auto;
        gap: 0.55rem;
        align-items: center;
        min-width: 0;
        min-height: 3.5rem;
        padding: 0.35rem 0.45rem;
        background: rgba(244, 239, 229, 0.1);
        border: 1px solid rgba(244, 239, 229, 0.18);
    }

    .dock-record.empty {
        grid-template-columns: auto 1fr;
        background: transparent;
        border-style: dashed;
    }

    .dock-letter,
    .compare-letter {
        display: grid;
        place-items: center;
        width: 1.65rem;
        height: 1.65rem;
        color: var(--ink);
        background: var(--paper);
        font-size: 0.72rem;
        font-weight: 800;
    }

    .dock-record:first-child .dock-letter {
        color: var(--paper);
        background: var(--indigo);
    }

    .dock-record:nth-child(2) .dock-letter {
        color: var(--paper);
        background: var(--madder);
    }

    .dock-record img {
        width: 3rem;
        height: 2.65rem;
        object-fit: cover;
    }

    .dock-record strong {
        overflow: hidden;
        font-size: 0.72rem;
        font-weight: 600;
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    .dock-record button,
    .clear-comparison {
        display: grid;
        place-items: center;
        width: 2rem;
        height: 2rem;
        padding: 0;
        color: rgba(244, 239, 229, 0.72);
        border: 0;
        background: transparent;
        cursor: pointer;
    }

    .open-comparison {
        min-height: 3.5rem;
        padding: 0.7rem 1rem;
        color: var(--ink);
        border: 1px solid var(--paper);
        background: var(--paper);
        font-size: 0.74rem;
        font-weight: 750;
        cursor: pointer;
    }

    .open-comparison:disabled {
        color: rgba(244, 239, 229, 0.5);
        border-color: rgba(244, 239, 229, 0.22);
        background: transparent;
        cursor: default;
    }

    .compare-modal-backdrop {
        position: fixed;
        z-index: 120;
        inset: 0;
        display: grid;
        place-items: center;
        padding: 1rem;
        overflow-y: auto;
        background: rgba(10, 10, 8, 0.88);
        backdrop-filter: blur(10px);
    }

    .compare-modal {
        width: min(78rem, 100%);
        max-height: calc(100svh - 2rem);
        overflow-y: auto;
        color: var(--ink);
        background: var(--paper);
    }

    .compare-modal > header {
        position: sticky;
        z-index: 2;
        top: 0;
        display: flex;
        gap: 2rem;
        align-items: center;
        justify-content: space-between;
        padding: 1.25rem clamp(1rem, 3vw, 2rem);
        background: color-mix(in srgb, var(--paper) 94%, transparent);
        border-bottom: 1px solid var(--line-strong);
        backdrop-filter: blur(14px);
    }

    .compare-modal > header .eyebrow {
        margin-bottom: 0.25rem;
    }

    .compare-modal > header h2 {
        margin: 0;
        font-family: var(--serif);
        font-size: clamp(2rem, 4vw, 3.6rem);
        font-weight: 400;
        letter-spacing: -0.045em;
        line-height: 0.95;
    }

    .compare-modal > header > button {
        display: grid;
        place-items: center;
        width: 2.75rem;
        height: 2.75rem;
        padding: 0;
        color: var(--ink);
        border: 1px solid var(--line-strong);
        background: transparent;
        cursor: pointer;
    }

    .compare-images {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 1px;
        background: var(--line-strong);
        border-bottom: 1px solid var(--line-strong);
    }

    .compare-images article {
        position: relative;
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(13rem, 0.55fr);
        gap: 1.25rem;
        padding: clamp(1rem, 2.5vw, 2rem);
        background: var(--paper-deep);
    }

    .compare-letter {
        position: absolute;
        z-index: 1;
        top: 1.5rem;
        left: 1.5rem;
        color: var(--paper);
        background: var(--indigo);
    }

    .compare-letter.b {
        background: var(--madder);
    }

    .compare-images figure {
        display: grid;
        place-items: center;
        min-height: 18rem;
        margin: 0;
        overflow: hidden;
        background: #d2c8b6;
    }

    .compare-images img {
        width: 100%;
        height: 100%;
        max-height: 25rem;
        object-fit: contain;
    }

    .compare-images article > div:last-child {
        align-self: end;
    }

    .compare-images h3 {
        margin-bottom: 0.4rem;
        font-family: var(--serif);
        font-size: clamp(1.5rem, 2.5vw, 2.5rem);
        font-weight: 400;
        line-height: 1;
    }

    .compare-images p {
        margin: 0;
        color: var(--ink-soft);
        font-size: 0.76rem;
    }

    .comparison-table {
        padding: clamp(1rem, 3vw, 2rem);
        overflow-x: auto;
    }

    .comparison-table table {
        width: 100%;
        min-width: 42rem;
        border-collapse: collapse;
    }

    .comparison-table th,
    .comparison-table td {
        width: 40%;
        padding: 0.75rem 1rem;
        text-align: left;
        vertical-align: top;
        border-bottom: 1px solid var(--line);
        font-size: 0.82rem;
    }

    .comparison-table th:first-child {
        width: 20%;
        color: var(--ink-soft);
        font-size: 0.68rem;
        font-weight: 650;
    }

    .comparison-table thead th {
        color: var(--ink);
        background: var(--paper-deep);
        font-weight: 750;
    }

    dl {
        margin: 1.5rem 0 0;
    }

    dl > div {
        display: grid;
        grid-template-columns: 7rem 1fr;
        gap: 1rem;
        padding: 0.55rem 0;
        border-top: 1px solid var(--line);
    }

    dt {
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.7rem;
        font-weight: 620;
        letter-spacing: 0;
    }

    dd {
        margin: 0;
        font-family: var(--serif);
        font-size: 0.75rem;
    }

    .image-note {
        padding: 3rem 0;
        color: var(--paper);
        background: var(--indigo-deep);
    }

    .image-note > div {
        display: grid;
        grid-template-columns: 12rem 1fr;
        gap: 2rem;
        align-items: center;
    }

    .image-note .eyebrow {
        margin: 0;
        color: var(--saffron);
    }

    .image-note p:last-child {
        max-width: 50rem;
        margin: 0;
        color: rgba(244, 239, 229, 0.72);
        font-family: var(--reading);
        font-size: 1.05rem;
    }

    .modal-backdrop {
        position: fixed;
        z-index: 100;
        inset: 0;
        display: grid;
        place-items: center;
        padding: 1rem;
        overflow-y: auto;
        background: rgba(10, 10, 8, 0.82);
        backdrop-filter: blur(8px);
    }

    .record-modal {
        position: relative;
        display: grid;
        grid-template-columns: minmax(18rem, 1.1fr) minmax(20rem, 0.9fr);
        width: min(70rem, 100%);
        max-height: min(48rem, calc(100svh - 2rem));
        overflow: hidden;
        background: var(--paper);
    }

    .modal-close {
        position: absolute;
        z-index: 1;
        top: 0.7rem;
        right: 0.7rem;
        display: grid;
        place-items: center;
        width: 2.5rem;
        height: 2.5rem;
        color: var(--paper);
        border: 1px solid rgba(255, 255, 255, 0.35);
        background: rgba(23, 23, 17, 0.8);
        cursor: pointer;
    }

    .record-modal > figure {
        display: grid;
        place-items: center;
        min-height: 32rem;
        margin: 0;
        overflow: hidden;
        background: #cfc5b4;
    }

    .record-modal > figure img {
        width: 100%;
        height: 100%;
        object-fit: contain;
    }

    .modal-copy {
        padding: clamp(2rem, 5vw, 4rem);
        overflow-y: auto;
    }

    .modal-copy h2 {
        margin-bottom: 0.4rem;
        font-family: var(--serif);
        font-size: clamp(2.4rem, 4vw, 4.2rem);
        font-weight: 400;
        letter-spacing: -0.05em;
        line-height: 0.95;
    }

    .artist {
        color: var(--ink-soft);
        font-family: var(--serif);
    }

    .modal-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 0.6rem;
        margin-top: 2rem;
    }

    .modal-actions button {
        cursor: pointer;
    }

    .modal-actions button:disabled {
        cursor: not-allowed;
        opacity: 0.45;
    }

    .modal-actions .active {
        color: var(--paper);
        background: var(--indigo);
    }

    @media (max-width: 850px) {
        .archive-toolbar {
            grid-template-columns: 1fr;
        }

        .view-switcher {
            width: 100%;
            overflow-x: auto;
        }

        .view-switcher button {
            flex: 1;
            white-space: nowrap;
        }

        .select-filters {
            grid-template-columns: repeat(2, 1fr);
        }

        .compare-dock-inner {
            grid-template-columns: minmax(0, 1fr) auto auto;
        }

        .dock-title {
            display: none;
        }

        .compare-images article {
            grid-template-columns: 1fr;
        }

        .record-modal {
            grid-template-columns: 1fr;
            max-height: calc(100svh - 2rem);
            overflow-y: auto;
        }

        .record-modal > figure {
            min-height: 25rem;
        }

        .modal-copy {
            overflow: visible;
        }
    }

    @media (max-width: 700px) {
        .archive-grid {
            column-count: 1;
        }
    }

    @media (max-width: 600px) {
        .card-copy {
            grid-template-columns: 1fr;
        }

        .compare-button {
            width: 100%;
        }

        .select-filters {
            grid-template-columns: 1fr 1fr;
        }

        .results-head {
            align-items: flex-start;
        }

        .results-head > div {
            flex-direction: column;
            align-items: flex-end;
        }

        .compare-dock-inner {
            grid-template-columns: minmax(0, 1fr) auto;
        }

        .dock-slots {
            grid-column: 1 / -1;
        }

        .open-comparison {
            min-height: 3rem;
        }

        .compare-images {
            grid-template-columns: 1fr;
        }

        .compare-modal > header {
            align-items: flex-start;
        }

        .image-note > div {
            grid-template-columns: 1fr;
        }

        .record-modal > figure {
            min-height: 19rem;
        }

        .modal-close {
            position: fixed;
            top: 1.6rem;
            right: 1.6rem;
        }
    }
</style>
