<script lang="ts">
    import MultiSelect from "$lib/components/MultiSelect.svelte";
    import ChartDownload from "$lib/components/ChartDownload.svelte";
    import { downloadBlob, downloadImageBoard, exportFilename } from "$lib/utils/download";
    import { imageSize } from "$lib/utils/image-size";
    import { waterfall } from "$lib/utils/waterfall";
    import {
        ArrowUpRight,
        Check,
        GitCompareArrows,
        Info,
        RotateCcw,
        Search,
        X,
    } from "@lucide/svelte";
    import { onMount } from "svelte";
    import { archiveItems, archiveOptions, type ArchiveItem } from "$lib/data/archive";
    import ResearchAppHeader from "$lib/components/ResearchAppHeader.svelte";
    import { assetPath } from "$lib/utils/asset-path";

    const PAGE_SIZE = 24;

    let view = $state<"all" | "named" | "unidentified">("all");
    let query = $state("");
    let textile = $state<string[]>([]);
    let color = $state<string[]>([]);
    let pattern = $state<string[]>([]);
    let process = $state<string[]>([]);
    let fiber = $state<string[]>([]);
    let weave = $state<string[]>([]);
    let modifierMode = $state<"all" | "any">("all");
    let currentPage = $state(1);
    let detailItem = $state<ArchiveItem | null>(null);
    let comparison = $state<ArchiveItem[]>([]);
    let comparisonOpen = $state(false);
    let resultsSection = $state<HTMLElement>();
    let attributesOpen = $state(true);

    const normalize = (input: string) =>
        input
            .toLocaleLowerCase("en")
            .normalize("NFKD")
            .replace(/[^\p{L}\p{N}]+/gu, "")
            .trim();

    function matchesAttribute(input: string, selected: string[]) {
        if (!selected.length) return true;
        const terms = input.split(/[,;]/).map(normalize);
        return selected.some((value) => terms.some((term) => term === normalize(value)));
    }

    function matchesModifiers(item: ArchiveItem) {
        const pairs: [string, string[]][] = [
            [item.primaryColor, color],
            [item.pattern, pattern],
            [item.process, process],
            [item.weave, weave],
            [item.fiber, fiber],
        ];
        const checks = pairs.flatMap(([field, selected]) =>
            selected.map((value) => matchesAttribute(field, [value])),
        );
        return (
            !checks.length ||
            (modifierMode === "all" ? checks.every(Boolean) : checks.some(Boolean))
        );
    }
    function attributeOptions(field: "primaryColor" | "pattern" | "process" | "weave" | "fiber") {
        return [
            ...new Set(
                archiveItems
                    .filter(
                        (item) =>
                            !textile.length ||
                            textile.some((value) => normalize(value) === normalize(item.textile)),
                    )
                    .flatMap((item) =>
                        item[field]
                            .split(/[,;]/)
                            .map((value) => value.trim())
                            .filter(Boolean),
                    ),
            ),
        ].sort((a, b) => a.localeCompare(b));
    }
    const searchTerms = $derived(query.trim().split(/\s+/).filter(Boolean).map(normalize));

    const filtered = $derived.by(() =>
        archiveItems.filter((item) => {
            const terms = normalize(
                [
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
                    item.sourceText,
                    item.additionalInfo,
                    item.quality,
                ].join(" "),
            );

            const isNamed = item.textile !== "No known name";

            return (
                (view === "all" || (view === "named" ? isNamed : !isNamed)) &&
                searchTerms.every((term) => terms.includes(term)) &&
                (!textile.length ||
                    textile.some((value) => normalize(item.textile) === normalize(value))) &&
                matchesModifiers(item)
            );
        }),
    );

    $effect(() => {
        filtered;
        currentPage = 1;
    });

    const pageCount = $derived(Math.max(1, Math.ceil(filtered.length / PAGE_SIZE)));
    const pageItems = $derived(
        filtered.slice((currentPage - 1) * PAGE_SIZE, currentPage * PAGE_SIZE),
    );
    const firstResult = $derived(filtered.length ? (currentPage - 1) * PAGE_SIZE + 1 : 0);
    const lastResult = $derived(Math.min(currentPage * PAGE_SIZE, filtered.length));
    const visiblePages = $derived.by(() => {
        const first = Math.max(1, Math.min(currentPage - 2, pageCount - 4));
        const last = Math.min(pageCount, first + 4);
        return Array.from({ length: last - first + 1 }, (_, index) => first + index);
    });

    const activeFilters = $derived(
        (query ? 1 : 0) +
            [textile, color, pattern, process, weave, fiber].reduce(
                (sum, values) => sum + values.length,
                0,
            ) +
            (view !== "all" ? 1 : 0),
    );

    onMount(() => {
        attributesOpen = window.matchMedia("(min-width: 1000px)").matches;
        const requested = new URL(window.location.href).searchParams.get("textile")?.trim();
        if (!requested) return;

        const normalizedRequest = normalize(requested);
        const matchingOption = archiveOptions.textiles.find((option) => {
            const normalizedOption = normalize(option);
            return (
                normalizedOption === normalizedRequest ||
                normalizedOption.includes(normalizedRequest) ||
                normalizedRequest.includes(normalizedOption)
            );
        });

        if (matchingOption) textile = [matchingOption];
        else query = requested;
    });

    function resetFilters() {
        view = "all";
        query = "";
        textile = [];
        color = [];
        pattern = [];
        process = [];
        fiber = [];
        weave = [];
        modifierMode = "all";
        currentPage = 1;
    }

    function setView(nextView: "all" | "named" | "unidentified") {
        view = nextView;
        currentPage = 1;
    }

    function setPage(page: number) {
        currentPage = Math.max(1, Math.min(page, pageCount));
        requestAnimationFrame(() =>
            resultsSection?.scrollIntoView({
                behavior: window.matchMedia("(prefers-reduced-motion: reduce)").matches
                    ? "instant"
                    : "smooth",
                block: "start",
            }),
        );
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

    function modal(node: HTMLDialogElement) {
        const previousOverflow = document.body.style.overflow;
        const trigger =
            document.activeElement instanceof HTMLElement ? document.activeElement : null;
        node.showModal();
        document.body.style.overflow = "hidden";
        function trapFocus(event: KeyboardEvent) {
            if (event.key !== "Tab") return;
            const controls = Array.from(
                node.querySelectorAll<HTMLElement>(
                    'a[href], button:not([disabled]), input:not([disabled]), select:not([disabled]), textarea:not([disabled]), [tabindex]:not([tabindex="-1"])',
                ),
            ).filter((element) => element.getClientRects().length > 0);
            const first = controls[0];
            const last = controls[controls.length - 1];
            if (!first) {
                event.preventDefault();
                return;
            }
            if (event.shiftKey && document.activeElement === first) {
                event.preventDefault();
                last.focus();
            } else if (!event.shiftKey && document.activeElement === last) {
                event.preventDefault();
                first.focus();
            }
        }
        node.addEventListener("keydown", trapFocus);
        return {
            destroy() {
                node.removeEventListener("keydown", trapFocus);
                node.close();
                document.body.style.overflow = previousOverflow;
                trigger?.focus({ preventScroll: true });
            },
        };
    }

    function downloadRows() {
        const columns: (keyof ArchiveItem)[] = [
            "id",
            "title",
            "textile",
            "primaryColor",
            "pattern",
            "process",
            "weave",
            "fiber",
            "collection",
            "inventory",
            "date",
            "catalogueUrl",
        ];
        const cell = (value: string) => '"' + value.replaceAll('"', '""') + '"';
        const csv = [
            columns.join(","),
            ...filtered.map((item) =>
                columns.map((column) => cell(String(item[column]))).join(","),
            ),
        ].join("\n");
        downloadBlob(
            new Blob(["\uFEFF" + csv], { type: "text/csv;charset=utf-8" }),
            exportFilename(
                "swatches",
                [textile, color, pattern, process, weave, fiber, modifierMode, query],
                "csv",
            ),
        );
    }
    function exportComparison() {
        return downloadImageBoard(
            comparison.map((item) => ({
                image: assetPath(item.fullImage),
                title: item.title,
                details: [item.id, item.date, item.collection, item.inventory].filter(Boolean),
            })),
            exportFilename(
                "swatches",
                comparison.map((item) => item.id),
                "png",
            ),
        );
    }
</script>

<svelte:head>
    <title>Swatch Search — Dutch Textile Trade</title>
    <meta
        name="description"
        content="Explore our growing database of textile samples and swatches by textile name (if you know it) or (if you don’t) search by attributes (modifiers) like color, pattern, process, weave structure, and fiber."
    />
</svelte:head>

<ResearchAppHeader active="swatch-search" />

<div class="archive-workspace page-shell">
    <aside class="archive-app" aria-label="Archive filters">
        <div class="archive-panel">
            <header class="filter-heading">
                <div>
                    <Search size={18} />
                    <h2>Filters</h2>
                </div>
                {#if activeFilters}
                    <button type="button" onclick={resetFilters}
                        ><RotateCcw size={14} /> Reset</button
                    >
                {/if}
            </header>

            <div class="archive-toolbar">
                <div class="view-switcher" role="group" aria-label="Record type">
                    <button
                        class:active={view === "all"}
                        aria-pressed={view === "all"}
                        type="button"
                        onclick={() => setView("all")}
                    >
                        All records
                    </button>
                    <button
                        class:active={view === "named"}
                        aria-pressed={view === "named"}
                        type="button"
                        onclick={() => setView("named")}
                    >
                        Named textiles
                    </button>
                    <button
                        class:active={view === "unidentified"}
                        aria-pressed={view === "unidentified"}
                        type="button"
                        onclick={() => setView("unidentified")}
                    >
                        Unidentified textiles
                    </button>
                </div>

                <label class="archive-search">
                    <Search size={17} strokeWidth={1.5} />
                    <span class="sr-only">Search archive</span>
                    <input
                        type="search"
                        placeholder="Search records…"
                        bind:value={query}
                        oninput={() => (currentPage = 1)}
                    />
                    {#if query}
                        <button
                            type="button"
                            aria-label="Clear search"
                            onclick={() => {
                                query = "";
                                currentPage = 1;
                            }}
                        >
                            <X size={15} />
                        </button>
                    {/if}
                </label>

                <details class="attribute-filters" bind:open={attributesOpen}>
                    <summary>Attributes</summary>
                    <div class="modifier-mode" role="group" aria-label="Match modifiers">
                        <button
                            type="button"
                            aria-pressed={modifierMode === "all"}
                            onclick={() => (modifierMode = "all")}>Match all</button
                        ><button
                            type="button"
                            aria-pressed={modifierMode === "any"}
                            onclick={() => (modifierMode = "any")}>Match any</button
                        >
                    </div>
                    <div class="select-filters">
                        <div class="field">
                            <MultiSelect
                                label="Textile name"
                                options={archiveOptions.textiles}
                                bind:value={textile}
                            />
                        </div>
                        <div class="field">
                            <MultiSelect
                                label="Color"
                                options={attributeOptions("primaryColor")}
                                bind:value={color}
                            />
                        </div>
                        <div class="field">
                            <MultiSelect
                                label="Pattern"
                                options={attributeOptions("pattern")}
                                bind:value={pattern}
                            />
                        </div>
                        <div class="field">
                            <MultiSelect
                                label="Process"
                                options={attributeOptions("process")}
                                bind:value={process}
                            />
                        </div>
                        <div class="field">
                            <MultiSelect
                                label="Weave"
                                options={attributeOptions("weave")}
                                bind:value={weave}
                            />
                        </div>
                        <div class="field">
                            <MultiSelect
                                label="Fiber"
                                options={attributeOptions("fiber")}
                                bind:value={fiber}
                            />
                        </div>
                    </div>
                </details>
            </div>
        </div>
    </aside>

    <section
        bind:this={resultsSection}
        class:comparison-active={comparison.length > 0}
        class="archive-results"
    >
        <div class="archive-export">
            <button
                class="button secondary"
                type="button"
                onclick={downloadRows}
                disabled={!filtered.length}>Download data</button
            >
        </div>
        <div class="results-head">
            <p role="status" aria-live="polite" aria-atomic="true">
                <strong>{filtered.length}</strong>
                {filtered.length === 1 ? "record" : "records"}
                {#if filtered.length > PAGE_SIZE}
                    <span> · {firstResult}–{lastResult}</span>
                {/if}
            </p>
            <div>
                {#if comparison.length}<span>{comparison.length}/2 selected</span>{/if}
            </div>
        </div>

        {#if filtered.length}
            <div class="archive-grid" use:waterfall>
                {#each pageItems as item, index (item.id)}
                    <article class:chosen={isCompared(item)} class="archive-card">
                        <button
                            class="image-button"
                            type="button"
                            aria-label={item.title}
                            aria-haspopup="dialog"
                            onclick={() => (detailItem = item)}
                        >
                            <img
                                {...imageSize(item.image)}
                                src={assetPath(item.image)}
                                alt={item.title}
                                loading={index > 5 ? "lazy" : "eager"}
                                decoding="async"
                            />
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
                                aria-pressed={isCompared(item)}
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
            {#if pageCount > 1}
                <nav class="pagination" aria-label="Swatch result pages">
                    <button
                        type="button"
                        disabled={currentPage === 1}
                        onclick={() => setPage(currentPage - 1)}
                    >
                        Previous
                    </button>
                    <div>
                        {#each visiblePages as page}
                            <button
                                class:active={page === currentPage}
                                type="button"
                                aria-current={page === currentPage ? "page" : undefined}
                                aria-label={`Page ${page}`}
                                onclick={() => setPage(page)}
                            >
                                {page}
                            </button>
                        {/each}
                    </div>
                    <span class="page-position">{currentPage} / {pageCount}</span>
                    <button
                        type="button"
                        disabled={currentPage === pageCount}
                        onclick={() => setPage(currentPage + 1)}
                    >
                        Next
                    </button>
                </nav>
            {/if}
        {:else}
            <div class="empty-state">
                <Search size={25} strokeWidth={1.3} />
                <h2>No results</h2>
                <button class="button" type="button" onclick={resetFilters}>Show all records</button
                >
            </div>
        {/if}
    </section>
</div>

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
                aria-haspopup="dialog"
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
    <dialog
        use:modal
        class="compare-modal-backdrop"
        aria-labelledby="comparison-title"
        oncancel={() => (comparisonOpen = false)}
        onclick={(event) => {
            if (event.currentTarget === event.target) comparisonOpen = false;
        }}
    >
        <div class="compare-modal" tabindex="-1">
            <header>
                <div>
                    <h2 id="comparison-title">Comparison Tool</h2>
                    <ChartDownload action={exportComparison} />
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
                        <figure><img src={assetPath(item.fullImage)} alt={item.title} /></figure>
                        <div>
                            <h3>{item.title}</h3>
                            {#if item.artist || item.date}<p>
                                    {[item.artist, item.date].filter(Boolean).join(", ")}
                                </p>{/if}
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
    </dialog>
{/if}

{#if detailItem}
    <dialog
        use:modal
        class="modal-backdrop"
        aria-labelledby="record-title"
        oncancel={() => (detailItem = null)}
        onclick={(event) => {
            if (event.currentTarget === event.target) detailItem = null;
        }}
    >
        <div class="record-modal">
            <button
                class="modal-close"
                type="button"
                aria-label="Close record"
                onclick={() => (detailItem = null)}
            >
                <X size={19} />
            </button>
            <figure>
                <img src={assetPath(detailItem.fullImage)} alt={detailItem.title} />
            </figure>
            <div class="modal-copy">
                <p class="eyebrow">{detailItem.id}</p>
                <h2 id="record-title">{detailItem.title}</h2>
                {#if detailItem.artist || detailItem.date}<p class="artist">
                        {[detailItem.artist, detailItem.date].filter(Boolean).join(", ")}
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
                    {#if detailItem.sourceText}
                        <div>
                            <dt>Text from source</dt>
                            <dd>{detailItem.sourceText}</dd>
                        </div>
                    {/if}
                    {#if detailItem.quality}
                        <div>
                            <dt>Quality</dt>
                            <dd>{detailItem.quality}</dd>
                        </div>
                    {/if}
                    {#if detailItem.additionalInfo}
                        <div>
                            <dt>Additional information</dt>
                            <dd>{detailItem.additionalInfo}</dd>
                        </div>
                    {/if}
                </dl>
                <div class="modal-actions">
                    <a
                        class="button secondary"
                        href={assetPath(detailItem.fullImage)}
                        download={exportFilename(
                            "swatch",
                            [detailItem.id, detailItem.title],
                            detailItem.fullImage.split(".").at(-1) || "jpg",
                        )}>Download image</a
                    >
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
    </dialog>
{/if}

<style>
    .modifier-mode {
        display: flex;
        gap: 0.4rem;
        padding: 0.5rem 0 1rem;
    }
    .modifier-mode button {
        padding: 0.5rem 0.65rem;
        border: 1px solid var(--line);
        background: transparent;
        font: var(--type-label);
    }
    .modifier-mode button[aria-pressed="true"] {
        color: var(--paper);
        background: var(--ink);
        border-color: var(--ink);
    }
    .archive-export {
        display: flex;
        justify-content: flex-end;
        padding-bottom: 1rem;
    }
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
    .archive-workspace {
        display: grid;
        grid-template-columns: 13rem minmax(0, 1fr);
        gap: clamp(2.5rem, 5vw, 6rem);
        align-items: start;
        padding-top: 3rem;
        padding-bottom: 8rem;
    }
    .archive-app {
        position: sticky;
        top: 7rem;
        min-width: 0;
        color: var(--ink);
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
        font-family: var(--display-font, var(--sans));
        font-size: 0.8rem;
        font-weight: var(--display-weight, 600);
        letter-spacing: 0;
    }
    .filter-heading button {
        display: inline-flex;
        gap: 0.35rem;
        align-items: center;
        min-height: 2.5rem;
        padding: 0;
        color: var(--ink-soft);
        border: 0;
        background: none;
        font: 500 0.75rem var(--sans);
        cursor: pointer;
    }
    .archive-toolbar {
        display: flex;
        flex-direction: column;
        gap: 1.5rem;
        margin-top: 1.5rem;
    }
    .archive-search {
        order: -1;
        display: flex;
        align-items: center;
        gap: 0.6rem;
        min-height: 3rem;
        padding: 0 0 0.2rem;
        border-bottom: 1px solid var(--ink);
    }
    .archive-search input {
        width: 100%;
        min-width: 0;
        padding: 0.5rem 0;
        border: 0;
        outline: 0;
        color: var(--ink);
        background: transparent;
        font: 0.82rem var(--sans);
    }
    .archive-search input::placeholder {
        color: var(--ink-soft);
        opacity: 1;
    }
    .archive-search button {
        display: grid;
        place-items: center;
        padding: 0.4rem;
        color: var(--ink);
        border: 0;
        background: transparent;
        cursor: pointer;
    }
    .archive-search:focus-within {
        border-color: var(--madder);
        box-shadow: 0 1px var(--madder);
    }
    .view-switcher {
        display: flex;
        flex-direction: column;
        align-items: flex-start;
        gap: 0.35rem;
    }
    .view-switcher button {
        position: relative;
        min-height: 2.5rem;
        padding: 0.5rem 0 0.5rem 1.1rem;
        color: var(--ink-soft);
        border: 0;
        background: none;
        font: 500 0.77rem var(--sans);
        text-align: left;
        cursor: pointer;
    }
    .view-switcher button::before {
        position: absolute;
        left: 0;
        top: calc(50% - 3px);
        width: 6px;
        height: 6px;
        content: "";
        border: 1px solid currentColor;
        border-radius: 50%;
    }
    .view-switcher button.active {
        color: var(--madder);
    }
    .view-switcher button.active::before {
        background: currentColor;
    }
    .attribute-filters {
        border-top: 1px solid var(--line);
    }
    .attribute-filters summary {
        display: flex;
        justify-content: space-between;
        gap: 1rem;
        padding: 1rem 0;
        font: 600 0.75rem var(--sans);
        list-style: none;
        cursor: pointer;
    }
    .attribute-filters summary::-webkit-details-marker {
        display: none;
    }
    .attribute-filters summary::after {
        content: "+";
        font-weight: 400;
    }
    .attribute-filters[open] summary::after {
        content: "−";
    }
    .select-filters {
        display: grid;
        gap: 1.25rem;
        padding-bottom: 1rem;
    }
    .archive-results {
        min-width: 0;
        scroll-margin-top: 6rem;
    }
    .archive-results.comparison-active {
        padding-bottom: 5rem;
    }
    .results-head {
        display: flex;
        gap: 1rem;
        align-items: center;
        justify-content: space-between;
        min-height: 2.5rem;
        margin-bottom: 2rem;
    }
    .results-head p {
        margin: 0;
        color: var(--ink-soft);
        font: 0.75rem var(--sans);
    }
    .results-head strong {
        color: var(--ink);
        font-weight: 600;
    }
    .results-head > div {
        color: var(--madder);
        font: 0.75rem var(--sans);
    }
    .archive-grid {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(0, 1fr);
        gap: 4.5rem clamp(1.5rem, 3vw, 3.5rem);
        align-items: start;
        row-gap: 0;
    }
    .archive-card {
        min-width: 0;
        padding-bottom: 3.5rem;
    }
    .image-button {
        position: relative;
        display: block;
        place-items: center;
        width: 100%;
        height: auto;
        padding: 0;
        overflow: hidden;
        border: 0;
        background: var(--paper-deep);
        cursor: zoom-in;
    }
    .image-button img {
        min-width: 0;
        min-height: 0;
        width: 100%;
        height: auto;
        object-fit: contain;
        transition: transform 300ms ease;
    }
    .image-button:hover img {
        transform: scale(1.025);
    }
    .image-button i {
        position: absolute;
        right: 0.8rem;
        bottom: 0.8rem;
        display: flex;
        gap: 0.4rem;
        align-items: center;
        padding: 0.55rem 0.7rem;
        color: var(--paper);
        background: var(--ink);
        font: 500 0.75rem var(--sans);
        font-style: normal;
        opacity: 0;
        transform: translateY(0.3rem);
        transition:
            opacity 160ms ease,
            transform 160ms ease;
    }
    .image-button:hover i,
    .image-button:focus-visible i {
        opacity: 1;
        transform: none;
    }
    .chosen .image-button {
        outline: 2px solid var(--madder);
        outline-offset: 6px;
    }
    .card-copy {
        display: grid;
        grid-template-columns: minmax(0, 1fr) auto;
        gap: 1rem;
        align-items: start;
        padding-top: 1rem;
    }
    .card-copy > div {
        min-width: 0;
    }
    .card-copy p {
        margin: 0 0 0.6rem;
        color: var(--ink-soft);
        font: 0.75rem/1.5 var(--sans);
        overflow-wrap: anywhere;
    }
    .card-copy h2 {
        margin: 0;
        font-family: var(--editorial-font);
        font-size: clamp(1.5rem, 2vw, 2.1rem);
        font-weight: var(--display-weight, 400);
        letter-spacing: var(--display-tracking, -0.025em);
        line-height: 1.14;
        overflow-wrap: anywhere;
    }
    .card-copy div > span {
        display: block;
        margin-top: 0.4rem;
        color: var(--ink-soft);
        font: 0.75rem var(--sans);
    }
    .compare-button {
        display: flex;
        flex-wrap: wrap;
        gap: 0.3rem;
        align-items: center;
        justify-content: flex-end;
        max-width: 6rem;
        min-height: 2.75rem;
        padding: 0;
        color: var(--ink-soft);
        border: 0;
        background: none;
        font: 500 0.75rem var(--sans);
        cursor: pointer;
    }
    .compare-button.active {
        color: var(--madder);
    }
    .compare-button:disabled {
        opacity: 0.35;
        cursor: default;
    }
    .slot-label {
        display: grid;
        place-items: center;
        width: 1.2rem;
        height: 1.2rem;
        color: var(--paper);
        background: var(--madder);
        font: 600 0.75rem var(--sans);
    }
    .pagination {
        display: flex;
        gap: 1rem;
        align-items: center;
        justify-content: space-between;
        margin-top: 5rem;
        padding-top: 1.5rem;
        border-top: 1px solid var(--line-strong);
    }
    .pagination > div {
        display: flex;
        gap: 0.3rem;
    }
    .pagination button {
        min-width: 2.75rem;
        min-height: 2.75rem;
        padding: 0.5rem;
        color: var(--ink-soft);
        border: 0;
        background: transparent;
        font: 500 0.75rem var(--sans);
        cursor: pointer;
    }
    .pagination button.active {
        color: var(--paper);
        background: var(--ink);
    }
    .pagination button:hover:not(:disabled) {
        color: var(--madder);
    }
    .pagination button.active:hover {
        color: var(--paper);
    }
    .pagination button:disabled {
        opacity: 0.3;
        cursor: default;
    }
    .page-position {
        display: none;
        font: 0.8rem var(--sans);
        font-variant-numeric: tabular-nums;
    }
    .empty-state {
        display: grid;
        place-items: start;
        align-content: center;
        gap: 1.5rem;
        min-height: 26rem;
    }
    .empty-state h2 {
        margin: 0;
        font: 400 clamp(3rem, 6vw, 6rem)/1 var(--editorial-font);
        letter-spacing: var(--display-tracking, -0.03em);
    }
    .image-note {
        padding: 3rem 0;
        border-top: 1px solid var(--line-strong);
    }
    .image-note > div {
        display: grid;
        grid-template-columns: 13rem minmax(0, 42rem);
        gap: clamp(2.5rem, 5vw, 6rem);
    }
    .image-note .eyebrow {
        margin: 0;
        color: var(--ink);
        font-size: 0.75rem;
        letter-spacing: 0;
        text-transform: none;
    }
    .image-note p:last-child {
        margin: 0;
        color: var(--ink-soft);
        font: 1.15rem/1.6 var(--reading);
    }
    .compare-dock {
        position: fixed;
        z-index: 70;
        right: 0;
        bottom: 0;
        left: 0;
        padding: 0.75rem 0;
        color: var(--ink);
        background: var(--paper);
        border-top: 1px solid var(--ink);
        box-shadow: 0 -1rem 3rem #17171110;
    }
    .compare-dock-inner {
        display: grid;
        grid-template-columns: auto minmax(0, 1fr) auto auto;
        gap: 1.5rem;
        align-items: center;
    }
    .dock-title {
        display: flex;
        align-items: center;
        gap: 0.65rem;
    }
    .dock-title strong {
        font: 400 1.25rem var(--editorial-font);
    }
    .dock-slots {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 1rem;
    }
    .dock-record {
        display: grid;
        grid-template-columns: auto 2.5rem minmax(0, 1fr) auto;
        gap: 0.55rem;
        align-items: center;
        min-width: 0;
        min-height: 3rem;
    }
    .dock-record.empty {
        grid-template-columns: auto 1fr;
        opacity: 0.3;
    }
    .dock-letter,
    .compare-letter {
        display: grid;
        place-items: center;
        width: 1.5rem;
        height: 1.5rem;
        font: 500 0.75rem var(--sans);
    }
    .dock-letter {
        color: var(--madder);
        border: 1px solid var(--madder);
    }
    .dock-record img {
        width: 2.5rem;
        height: 3rem;
        object-fit: contain;
    }
    .dock-record strong {
        overflow: hidden;
        font: 500 0.75rem var(--sans);
        text-overflow: ellipsis;
        white-space: nowrap;
    }
    .dock-record button,
    .clear-comparison {
        display: grid;
        place-items: center;
        width: 2.75rem;
        height: 2.75rem;
        padding: 0;
        color: var(--ink);
        border: 0;
        background: transparent;
        cursor: pointer;
    }
    .open-comparison {
        min-height: 3rem;
        padding: 0.8rem 1.15rem;
        color: var(--paper);
        border: 1px solid var(--ink);
        background: var(--ink);
        font: 500 0.75rem var(--sans);
        cursor: pointer;
    }
    .open-comparison:disabled {
        color: var(--ink-soft);
        border-color: var(--line);
        background: transparent;
        cursor: default;
    }
    dialog {
        position: fixed;
        inset: 0;
        display: grid;
        place-items: center;
        width: 100%;
        height: 100%;
        max-width: none;
        max-height: none;
        margin: 0;
        padding: 1.5rem;
        overflow-y: auto;
        border: 0;
        background: #10110fed;
    }
    dialog::backdrop {
        background: transparent;
    }
    .record-modal {
        position: relative;
        display: grid;
        grid-template-columns: minmax(0, 1.4fr) minmax(21rem, 0.75fr);
        width: min(94rem, 100%);
        height: min(54rem, calc(100svh - 3rem));
        overflow: hidden;
        color: #f7f5ef;
        background: #1c1e1a;
    }
    .modal-close {
        position: absolute;
        z-index: 2;
        top: 0.8rem;
        right: 0.8rem;
        display: grid;
        place-items: center;
        width: 2.75rem;
        height: 2.75rem;
        color: #f7f5ef;
        border: 1px solid #f7f5ef60;
        background: #171711;
        cursor: pointer;
    }
    .record-modal > figure {
        display: grid;
        place-items: center;
        min-height: 0;
        margin: 0;
        padding: 1.5rem;
        overflow: hidden;
        background: #0e100d;
    }
    .record-modal > figure img {
        min-width: 0;
        min-height: 0;
        width: 100%;
        height: 100%;
        max-height: calc(100svh - 6rem);
        object-fit: contain;
    }
    .modal-copy {
        padding: 4rem clamp(1.5rem, 3vw, 3rem) 2rem;
        overflow-y: auto;
    }
    .modal-copy .eyebrow {
        color: #d88976;
    }
    .modal-copy h2 {
        margin: 0 0 0.7rem;
        font: 400 clamp(2.2rem, 3.5vw, 4.5rem)/1 var(--editorial-font);
        letter-spacing: -0.03em;
    }
    .artist {
        margin: 0;
        color: #bdbfb8;
        font: 0.88rem var(--sans);
    }
    dl {
        margin: 2rem 0 0;
    }
    dl > div {
        display: grid;
        grid-template-columns: 6rem minmax(0, 1fr);
        gap: 1rem;
        padding: 0.85rem 0;
        border-top: 1px solid #f7f5ef26;
    }
    dt {
        color: #a7aaa2;
        font: 0.75rem/1.5 var(--sans);
    }
    dd {
        margin: 0;
        color: #f7f5ef;
        font: 1.05rem/1.55 var(--reading);
        overflow-wrap: anywhere;
    }
    .modal-actions {
        display: flex;
        flex-wrap: wrap;
        gap: 0.6rem;
        margin-top: 2rem;
    }
    .modal-actions :global(.button) {
        min-height: 2.8rem;
        padding: 0.7rem 0.85rem;
        color: #f7f5ef;
        border: 1px solid #f7f5ef60;
        background: none;
        font-size: 0.75rem;
    }
    .modal-actions button {
        cursor: pointer;
    }
    .modal-actions button:disabled {
        opacity: 0.4;
        cursor: default;
    }
    .modal-actions :global(.active) {
        color: #171711;
        background: #f7f5ef;
    }
    .compare-modal {
        width: min(80rem, 100%);
        max-height: calc(100svh - 3rem);
        overflow-y: auto;
        color: #f7f5ef;
        background: #1c1e1a;
    }
    .compare-modal > header {
        position: sticky;
        z-index: 2;
        top: 0;
        display: flex;
        gap: 2rem;
        align-items: center;
        justify-content: space-between;
        padding: 1.25rem 2rem;
        background: #1c1e1a;
        border-bottom: 1px solid #f7f5ef26;
    }
    .compare-modal > header h2 {
        margin: 0;
        font: 400 clamp(2rem, 4vw, 3.8rem)/1 var(--editorial-font);
        letter-spacing: var(--display-tracking, -0.03em);
    }
    .compare-modal > header > button {
        flex: 0 0 auto;
        display: grid;
        place-items: center;
        width: 2.75rem;
        height: 2.75rem;
        padding: 0;
        color: #f7f5ef;
        border: 1px solid #f7f5ef60;
        background: none;
        cursor: pointer;
    }
    .compare-images {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 2rem;
        padding: 2rem;
    }
    .compare-images article {
        position: relative;
        min-width: 0;
    }
    .compare-letter {
        position: absolute;
        z-index: 1;
        top: 0.7rem;
        left: 0.7rem;
        color: #171711;
        background: #f7f5ef;
    }
    .compare-images figure {
        display: grid;
        place-items: center;
        height: 24rem;
        margin: 0 0 1rem;
        padding: 1rem;
        background: #0e100d;
    }
    .compare-images img {
        min-width: 0;
        min-height: 0;
        width: 100%;
        height: 100%;
        object-fit: contain;
    }
    .compare-images h3 {
        margin: 0 0 0.4rem;
        font: 400 clamp(1.4rem, 2.4vw, 2.5rem)/1.14 var(--editorial-font);
        overflow-wrap: anywhere;
    }
    .compare-images p {
        margin: 0;
        color: #bdbfb8;
        font: 0.78rem var(--sans);
    }
    .comparison-table {
        padding: 0 2rem 2rem;
    }
    .comparison-table table {
        width: 100%;
        table-layout: fixed;
        border-collapse: collapse;
        overflow-wrap: anywhere;
    }
    .comparison-table th,
    .comparison-table td {
        width: 37%;
        padding: 0.9rem 0.8rem;
        text-align: left;
        vertical-align: top;
        border-bottom: 1px solid #f7f5ef26;
        font: 1rem/1.5 var(--reading);
    }
    .comparison-table th:first-child {
        width: 26%;
        color: #a7aaa2;
        font: 0.75rem/1.5 var(--sans);
    }
    .comparison-table thead th {
        font: 600 0.75rem var(--sans);
    }
    @media (max-width: 1100px) {
        .archive-workspace {
            grid-template-columns: 11rem minmax(0, 1fr);
            gap: 2.5rem;
        }
        .compare-dock-inner {
            grid-template-columns: minmax(0, 1fr) auto auto;
            gap: 0.8rem;
        }
        .dock-title {
            display: none;
        }
        .card-copy {
            grid-template-columns: 1fr;
            gap: 0.4rem;
        }
        .compare-button {
            justify-content: flex-start;
        }
    }
    @media (max-width: 999px) {
        .archive-workspace {
            display: block;
            padding-top: 1.5rem;
        }
        .archive-app {
            position: static;
            padding-bottom: 2rem;
        }
        .filter-heading {
            display: flex;
            justify-content: flex-end;
        }

        .filter-heading > div {
            display: none;
        }
        .archive-toolbar {
            display: grid;
            grid-template-columns: minmax(15rem, 1fr) auto;
            gap: 1rem 2rem;
            margin: 0;
        }
        .archive-search {
            order: 0;
            grid-column: 1;
            grid-row: 1;
        }
        .view-switcher {
            flex-direction: row;
            flex-wrap: wrap;
            gap: 0.6rem 1.5rem;
            grid-column: 1;
        }
        .attribute-filters {
            grid-column: 2;
            grid-row: 1 / 3;
            min-width: 13rem;
        }
        .select-filters {
            grid-template-columns: 1fr 1fr;
            gap: 1rem;
        }
        .attribute-filters[open] {
            grid-column: 1 / -1;
            grid-row: 3;
        }
        .results-head {
            border-top: 1px solid var(--line);
            padding-top: 1.5rem;
        }
        .image-button {
            height: auto;
        }
        .record-modal {
            display: block;
            height: auto;
            max-height: calc(100svh - 3rem);
            overflow-y: auto;
        }
        .record-modal > figure img {
            height: auto;
            max-height: 65svh;
        }
        .modal-copy {
            overflow: visible;
            padding-top: 2rem;
        }
        .modal-close {
            position: fixed;
            top: 2.3rem;
            right: 2.3rem;
        }
        .compare-images figure {
            height: 18rem;
        }
    }
    @media (max-width: 600px) {
        .archive-workspace {
            padding-bottom: 4rem;
        }
        .archive-toolbar {
            grid-template-columns: 1fr auto;
            gap: 0.75rem;
        }
        .view-switcher {
            gap: 0.2rem 1rem;
            grid-column: 1 / -1;
            grid-row: 2;
        }
        .view-switcher button {
            min-height: 2rem;
            padding-left: 0.8rem;
            font-size: 0.75rem;
        }
        .attribute-filters {
            grid-row: 1;
            min-width: 5rem;
            border: 0;
        }
        .attribute-filters summary {
            min-height: 3rem;
            padding: 0.85rem 0;
            font-size: 0.75rem;
        }
        .attribute-filters[open] {
            border-top: 1px solid var(--line);
        }
        .select-filters {
            grid-template-columns: repeat(2, minmax(0, 1fr));
        }
        .results-head {
            align-items: flex-start;
            margin-bottom: 1.5rem;
        }
        .results-head p {
            font-size: 0.75rem;
        }
        .archive-grid {
            gap: 3rem 1.25rem;
            row-gap: 0;
        }
        .image-button {
            height: auto;
        }
        .card-copy h2 {
            font-size: 1.45rem;
        }
        .card-copy p {
            font-size: 0.75rem;
        }
        .compare-button {
            font-size: 0.75rem;
        }
        .image-button i {
            right: 0.3rem;
            bottom: 0.3rem;
            padding: 0.4rem;
            font-size: 0.75rem;
        }
        .image-button i :global(svg) {
            display: none;
        }
        .pagination {
            margin-top: 3rem;
        }
        .pagination > div {
            display: none;
        }
        .page-position {
            display: inline;
        }
        .image-note > div {
            grid-template-columns: 1fr;
            gap: 1rem;
        }
        .compare-dock-inner {
            grid-template-columns: 1fr auto;
            gap: 0.5rem;
        }
        .dock-slots {
            grid-column: 1 / -1;
            gap: 0.5rem;
        }
        .dock-record {
            grid-template-columns: auto 2rem minmax(0, 1fr) auto;
            gap: 0.3rem;
        }
        .dock-record img {
            width: 2rem;
        }
        .dock-record strong {
            font-size: 0.75rem;
        }
        .dock-record button {
            width: 1.8rem;
        }
        dialog {
            padding: 0.75rem;
        }
        .record-modal,
        .compare-modal {
            max-height: calc(100svh - 1.5rem);
        }
        .record-modal > figure {
            padding: 0.75rem;
        }
        .modal-close {
            top: 1.5rem;
            right: 1.5rem;
        }
        .modal-copy {
            padding: 2rem 1.25rem;
        }
        .compare-modal > header {
            padding: 1rem;
            gap: 1rem;
        }
        .compare-modal > header h2 {
            font-size: 2rem;
        }
        .compare-images {
            gap: 0.75rem;
            padding: 0.75rem;
        }
        .compare-images figure {
            height: 13rem;
            padding: 0.3rem;
        }
        .compare-images h3 {
            font-size: 1.35rem;
        }
        .compare-images p {
            font-size: 0.75rem;
        }
        .comparison-table {
            padding: 0.5rem 0.5rem 1rem;
        }
        .comparison-table th,
        .comparison-table td {
            padding: 0.7rem 0.4rem;
            font-size: 0.9rem;
        }
        .comparison-table th:first-child {
            font-size: 0.75rem;
        }
    }
    @media (max-width: 360px) {
        .archive-grid {
            grid-template-columns: 1fr;
            row-gap: 0;
        }
        .image-button {
            height: auto;
        }
    }
    @media (hover: none) {
        .image-button i {
            opacity: 1;
            transform: none;
        }
    }
    @media (prefers-reduced-motion: reduce) {
        .image-button img,
        .image-button i {
            transition: none;
        }
    }
</style>
