<script lang="ts">
    import MultiSelect from "$lib/components/MultiSelect.svelte";
    import ChartDownload from "$lib/components/ChartDownload.svelte";
    import { downloadBlob, downloadChart, exportFilename } from "$lib/utils/download";

    import {
        ArrowDownToLine,
        BarChart3,
        Database,
        Filter,
        RotateCcw,
        Route,
        Rows3,
        Search,
        X,
    } from "@lucide/svelte";
    import { onMount, tick } from "svelte";
    import ResearchAppHeader from "$lib/components/ResearchAppHeader.svelte";
    import type { TradeRecord } from "$lib/server/trade";
    import type { PageData } from "./$types";

    let { data }: { data: PageData } = $props();

    // This page is prerendered and its load data does not change after initialization.
    // svelte-ignore state_referenced_locally
    const initialData = $state.snapshot(data);
    const firstYear = initialData.options.years[0] ?? 1700;
    const lastYear = initialData.options.years.at(-1) ?? 1724;

    let company = $state("");
    let textile = $state<string[]>([]);
    let origin = $state<string[]>([]);
    let destination = $state<string[]>([]);
    let color = $state<string[]>([]);
    let pattern = $state<string[]>([]);
    let fiber = $state<string[]>([]);
    let yearFrom = $state(firstYear);
    let yearTo = $state(lastYear);
    let search = $state("");
    let chartDimension = $state<"textile" | "destination" | "origin" | "year">("textile");
    let chartMetric = $state<"records" | "value">("records");
    let activeTab = $state<"chart" | "routes" | "records">("chart");
    let filtersOpen = $state(false);
    let filterPanel = $state<HTMLElement>();
    let filterTrigger = $state<HTMLButtonElement>();

    async function openFilters() {
        filtersOpen = true;
        await tick();
        await new Promise<void>((resolve) => requestAnimationFrame(() => resolve()));
        if (filtersOpen) {
            filterPanel?.querySelector<HTMLInputElement>("input")?.focus({ preventScroll: true });
        }
    }

    function closeFilters() {
        filtersOpen = false;
        filterTrigger?.focus({ preventScroll: true });
    }

    function handleFilterKeydown(event: KeyboardEvent) {
        if (!filtersOpen || !window.matchMedia("(max-width: 900px)").matches) return;
        if (event.key === "Escape") {
            event.preventDefault();
            closeFilters();
        }
        if (event.key !== "Tab" || !filterPanel) return;
        const controls = [
            ...filterPanel.querySelectorAll<HTMLElement>(
                "button:not([disabled]), input, select, summary",
            ),
        ].filter((element) => element.checkVisibility());
        const first = controls[0];
        const last = controls.at(-1);
        if (event.shiftKey && document.activeElement === first) {
            event.preventDefault();
            last?.focus();
        } else if (!event.shiftKey && document.activeElement === last) {
            event.preventDefault();
            first?.focus();
        }
    }
    let recordPage = $state(1);
    const pageSize = 100;

    onMount(() => {
        textile = new URL(window.location.href).searchParams.getAll("textile");
    });

    const normalize = (value: string) => value.toLocaleLowerCase("en").trim();

    const filtered = $derived.by(() =>
        data.records.filter((record) => {
            const textileMatch =
                !textile.length ||
                textile.some((value) => normalize(record.textile) === normalize(value));
            const searchText = [
                record.textile,
                record.origin,
                record.destination,
                record.color,
                record.inferredColor,
                record.pattern,
                record.process,
                record.fiber,
                record.quality,
                record.geography,
                record.other,
            ]
                .join(" ")
                .toLocaleLowerCase("en");

            return (
                (!company || record.company === company) &&
                textileMatch &&
                (!origin.length || origin.includes(record.origin)) &&
                (!destination.length || destination.includes(record.destination)) &&
                (!color.length || color.includes(record.color)) &&
                (!pattern.length || pattern.includes(record.pattern)) &&
                (!fiber.length || fiber.includes(record.fiber)) &&
                (record.year === null || (record.year >= yearFrom && record.year <= yearTo)) &&
                (!search || searchText.includes(search.toLocaleLowerCase("en").trim()))
            );
        }),
    );

    const pageCount = $derived(Math.max(1, Math.ceil(filtered.length / pageSize)));
    const visibleRecords = $derived(
        filtered.slice((recordPage - 1) * pageSize, recordPage * pageSize),
    );

    $effect(() => {
        filtered;
        recordPage = 1;
    });

    const summary = $derived.by(() => {
        const textiles = new Set<string>();
        const origins = new Set<string>();
        const destinations = new Set<string>();
        const routes = new Set<string>();
        let value = 0;
        let valuedRecords = 0;

        for (const record of filtered) {
            if (record.textile) textiles.add(record.textile);
            if (record.origin) origins.add(record.origin);
            if (record.destination) destinations.add(record.destination);
            if (record.origin && record.destination)
                routes.add(`${record.origin}→${record.destination}`);
            if (record.value !== null) {
                value += record.value;
                valuedRecords += 1;
            }
        }

        return {
            textiles: textiles.size,
            places: new Set([...origins, ...destinations]).size,
            routes: routes.size,
            value,
            valuedRecords,
        };
    });

    type ChartItem = { name: string; records: number; value: number };

    const chartData = $derived.by(() => {
        const groups = new Map<string, ChartItem>();

        for (const record of filtered) {
            const raw =
                chartDimension === "year"
                    ? record.year?.toString() || "Unknown year"
                    : record[chartDimension] || "Not recorded";
            const current = groups.get(raw) ?? { name: raw, records: 0, value: 0 };
            current.records += 1;
            current.value += record.value ?? 0;
            groups.set(raw, current);
        }

        const result = [...groups.values()].sort((a, b) =>
            chartDimension === "year"
                ? a.name.localeCompare(b.name, undefined, { numeric: true })
                : b[chartMetric] - a[chartMetric],
        );

        return chartDimension === "year" ? result : result.slice(0, 12);
    });

    const chartMax = $derived(Math.max(...chartData.map((item) => item[chartMetric]), 1));

    const routeData = $derived.by(() => {
        const routes = new Map<
            string,
            { origin: string; destination: string; records: number; textiles: Set<string> }
        >();

        for (const record of filtered) {
            if (!record.origin || !record.destination) continue;
            const key = `${record.origin}→${record.destination}`;
            const current = routes.get(key) ?? {
                origin: record.origin,
                destination: record.destination,
                records: 0,
                textiles: new Set<string>(),
            };
            current.records += 1;
            if (record.textile) current.textiles.add(record.textile);
            routes.set(key, current);
        }

        return [...routes.values()]
            .sort((a, b) => b.records - a.records)
            .slice(0, 18)
            .map((route) => ({ ...route, textiles: route.textiles.size }));
    });

    const activeFilters = $derived(
        (company ? 1 : 0) +
            [textile, origin, destination, color, pattern, fiber].reduce(
                (sum, values) => sum + values.length,
                0,
            ) +
            (yearFrom !== firstYear || yearTo !== lastYear ? 1 : 0) +
            (search ? 1 : 0),
    );

    function resetFilters() {
        company = "";
        textile = [];
        origin = [];
        destination = [];
        color = [];
        pattern = [];
        fiber = [];
        yearFrom = firstYear;
        yearTo = lastYear;
        search = "";
    }

    function formatValue(value: number) {
        return new Intl.NumberFormat("en", {
            notation: value > 999_999 ? "compact" : "standard",
            maximumFractionDigits: value > 999 ? 1 : 0,
        }).format(value);
    }

    function csvCell(value: string | number | null) {
        const string = value === null ? "" : String(value);
        return `"${string.replaceAll('"', '""')}"`;
    }

    function downloadFiltered() {
        const columns: (keyof TradeRecord)[] = [
            "company",
            "exchangeNumber",
            "source",
            "year",
            "origin",
            "destination",
            "textile",
            "quantity",
            "unit",
            "originalUnit",
            "value",
            "color",
            "inferredColor",
            "pattern",
            "process",
            "fiber",
            "quality",
            "geography",
            "other",
        ];
        const rows = [
            columns.join(","),
            ...filtered.map((record) => columns.map((column) => csvCell(record[column])).join(",")),
        ];
        const blob = new Blob(["\uFEFF", rows.join("\n")], { type: "text/csv;charset=utf-8" });
        downloadBlob(
            blob,
            exportFilename(
                "records",
                [textile, company, origin, destination, color, pattern, fiber, yearFrom, yearTo],
                "csv",
            ),
        );
    }

    function exportImage() {
        const filters: [string, string[]][] = [
            ["Textile", textile],
            ["Company", company ? [company] : []],
            ["Origin", origin],
            ["Destination", destination],
            ["Color", color],
            ["Pattern", pattern],
            ["Fiber", fiber],
            ["Search", search ? [search] : []],
        ];
        const routeView = activeTab === "routes";
        const measure = !routeView && chartMetric === "value" ? "Recorded value" : "Record count";
        return downloadChart({
            title: "Trade Data Explorer",
            context: [
                "Years: " + yearFrom + "–" + yearTo,
                "Measure: " + measure,
                "Compare by: " + (routeView ? "Route" : chartDimension),
                ...filters
                    .filter(([, values]) => values.length)
                    .map(([label, values]) => label + ": " + values.join(", ")),
            ],
            series: [{ label: measure, color: "#171711" }],
            rows: (routeView
                ? routeData.map((row) => ({
                      name: row.origin + " → " + row.destination,
                      records: row.records,
                      value: 0,
                  }))
                : chartData
            ).map((row) => ({
                label: row.name,
                values: [row[routeView ? "records" : chartMetric]],
                display: [
                    routeView || chartMetric === "records"
                        ? row.records.toLocaleString()
                        : formatValue(row.value) + " ƒ",
                ],
            })),
            filename: exportFilename(
                routeView ? "routes" : "explore",
                [
                    textile,
                    company,
                    origin,
                    destination,
                    color,
                    pattern,
                    fiber,
                    search,
                    yearFrom,
                    yearTo,
                    activeTab === "routes" ? "routes" : chartDimension,
                    activeTab === "routes" ? "records" : chartMetric,
                ],
                "png",
            ),
        });
    }
</script>

<svelte:head>
    <title>Trade Data Explorer — Dutch Textile Trade</title>
    <meta
        name="description"
        content="These dynamic apps allow users to explore the project data by geography, date, company, textile name and/or descriptors of the textiles found in archival sources (modifiers), and value of textiles, producing data visualizations."
    />
</svelte:head>

<svelte:window onkeydown={handleFilterKeydown} />

<ResearchAppHeader active="trade-explorer" />

{#if filtersOpen}<button
        class="filter-backdrop"
        type="button"
        aria-label="Close filters"
        onclick={closeFilters}
    ></button>{/if}

<div class="explorer-shell page-shell">
    <aside id="explorer-filters" bind:this={filterPanel} class:open={filtersOpen} class="filters">
        <div class="filter-heading">
            <div>
                <Filter size={17} strokeWidth={1.5} />
                <h2>Refine the records</h2>
            </div>
            <button class="close-filters" type="button" onclick={closeFilters}>
                <X size={18} />
                <span>Close filters</span>
            </button>
        </div>

        <label class="field search-field">
            <span>Search across fields</span>
            <div>
                <Search size={15} />
                <input type="search" placeholder="Textile, place, modifier…" bind:value={search} />
            </div>
        </label>

        <div class="field">
            <span>Company</span>
            <div class="segmented">
                <button class:active={!company} type="button" onclick={() => (company = "")}
                    >Both</button
                >
                <button
                    class:active={company === "VOC"}
                    type="button"
                    onclick={() => (company = "VOC")}>VOC</button
                >
                <button
                    class:active={company === "WIC"}
                    type="button"
                    onclick={() => (company = "WIC")}>WIC</button
                >
            </div>
        </div>

        <div class="field">
            <MultiSelect
                label="Textile name"
                options={data.options.textiles}
                bind:value={textile}
            />
        </div>

        <div class="field">
            <MultiSelect label="Origin region" options={data.options.origins} bind:value={origin} />
        </div>

        <div class="field">
            <MultiSelect
                label="Destination region"
                options={data.options.destinations}
                bind:value={destination}
            />
        </div>

        <div class="year-field field">
            <span>Years</span>
            <div>
                <label>
                    <span>From</span>
                    <input type="number" min={firstYear} max={yearTo} bind:value={yearFrom} />
                </label>
                <i></i>
                <label>
                    <span>To</span>
                    <input type="number" min={yearFrom} max={lastYear} bind:value={yearTo} />
                </label>
            </div>
        </div>

        <details>
            <summary>Archival modifiers</summary>
            <div class="modifier-fields">
                <div class="field">
                    <MultiSelect label="Color" options={data.options.colors} bind:value={color} />
                </div>
                <div class="field">
                    <MultiSelect
                        label="Pattern"
                        options={data.options.patterns}
                        bind:value={pattern}
                    />
                </div>
                <div class="field">
                    <MultiSelect label="Fiber" options={data.options.fibers} bind:value={fiber} />
                </div>
            </div>
        </details>

        <button class="reset-button" type="button" onclick={resetFilters} disabled={!activeFilters}>
            <RotateCcw size={14} />
            Reset filters {#if activeFilters}<span>({activeFilters})</span>{/if}
        </button>
    </aside>

    <section class="explorer-content">
        <div class="mobile-toolbar">
            <button
                bind:this={filterTrigger}
                type="button"
                aria-controls="explorer-filters"
                aria-expanded={filtersOpen}
                onclick={openFilters}
            >
                <Filter size={15} /> Filters {#if activeFilters}<span>{activeFilters}</span>{/if}
            </button>
            <button type="button" onclick={downloadFiltered}>
                <ArrowDownToLine size={15} /> Download
            </button>
        </div>

        <div class="result-summary">
            <div class="result-count">
                <span>Current view</span>
                <strong>{filtered.length.toLocaleString()}</strong>
                <p>of {data.records.length.toLocaleString()} records</p>
            </div>
            <div>
                <strong>{summary.textiles.toLocaleString()}</strong>
                <span>textiles</span>
            </div>
            <div>
                <strong>{summary.routes.toLocaleString()}</strong>
                <span>routes</span>
            </div>
            <div>
                <strong>{summary.places.toLocaleString()}</strong>
                <span>places</span>
            </div>
            <button type="button" onclick={downloadFiltered} disabled={!filtered.length}>
                <ArrowDownToLine size={15} /> Download view
            </button>
        </div>

        <div class="view-tabs" role="tablist" aria-label="Data views">
            <button
                class:active={activeTab === "chart"}
                type="button"
                role="tab"
                aria-selected={activeTab === "chart"}
                onclick={() => (activeTab = "chart")}
            >
                <BarChart3 size={15} /> Chart
            </button>
            <button
                class:active={activeTab === "routes"}
                type="button"
                role="tab"
                aria-selected={activeTab === "routes"}
                onclick={() => (activeTab = "routes")}
            >
                <Route size={15} /> Routes
            </button>
            <button
                class:active={activeTab === "records"}
                type="button"
                role="tab"
                aria-selected={activeTab === "records"}
                onclick={() => (activeTab = "records")}
            >
                <Rows3 size={15} /> Records
            </button>
        </div>

        {#if activeTab !== "records"}<div class="export-toolbar">
                <ChartDownload action={exportImage} disabled={!filtered.length} />
            </div>{/if}
        {#if activeTab === "chart"}
            <div class="chart-panel" role="tabpanel">
                <div class="panel-heading">
                    <div>
                        <span>Compare by</span>
                        <select bind:value={chartDimension}>
                            <option value="textile">Textile name</option>
                            <option value="destination">Destination region</option>
                            <option value="origin">Origin region</option>
                            <option value="year">Year</option>
                        </select>
                    </div>
                    <div>
                        <span>Measure</span>
                        <select bind:value={chartMetric}>
                            <option value="records">Record count</option>
                            <option value="value">Recorded value</option>
                        </select>
                    </div>
                </div>

                {#if chartData.length}
                    <div class:timeline={chartDimension === "year"} class="bar-chart">
                        {#each chartData as item, index}
                            <div class="bar-row">
                                <span class="bar-rank">{String(index + 1).padStart(2, "0")}</span>
                                <span class="bar-name" title={item.name}>{item.name}</span>
                                <div class="bar-track">
                                    <i style={`--bar: ${(item[chartMetric] / chartMax) * 100}%`}
                                    ></i>
                                </div>
                                <strong>
                                    {chartMetric === "value"
                                        ? `ƒ${formatValue(item.value)}`
                                        : item.records.toLocaleString()}
                                </strong>
                            </div>
                        {/each}
                    </div>
                    {#if chartMetric === "value"}
                        <p class="chart-note">
                            Some VOC accounts use Indian (Indisch) guilders, which are approximately
                            7/10ths of a Dutch guilder; these have been normalized to Dutch guilders
                            in the dataset.
                        </p>
                    {/if}
                {:else}
                    <div class="empty-panel">
                        <Database size={26} strokeWidth={1.3} />
                        <h2>No matching records.</h2>
                        <button type="button" onclick={resetFilters}>Clear filters</button>
                    </div>
                {/if}
            </div>
        {:else if activeTab === "routes"}
            <div class="routes-panel" role="tabpanel">
                <div class="route-heading">
                    <div>
                        <span>Origin</span>
                        <span>Destination</span>
                    </div>
                    <p>Record count</p>
                </div>
                {#if routeData.length}
                    <div class="route-list">
                        {#each routeData as route, index}
                            <article>
                                <span class="route-index">{String(index + 1).padStart(2, "0")}</span
                                >
                                <div class="route-points">
                                    <strong>{route.origin}</strong>
                                    <i><b></b></i>
                                    <strong>{route.destination}</strong>
                                </div>
                                <div class="route-meta">
                                    <span>{route.records.toLocaleString()} records</span>
                                    <span>{route.textiles.toLocaleString()} textile names</span>
                                </div>
                            </article>
                        {/each}
                    </div>
                {:else}
                    <div class="empty-panel">
                        <Route size={26} strokeWidth={1.3} />
                        <h2>No matching routes.</h2>
                        <button type="button" onclick={resetFilters}>Clear filters</button>
                    </div>
                {/if}
            </div>
        {:else}
            <div class="records-panel" role="tabpanel">
                {#if filtered.length > pageSize}
                    <nav class="record-pagination" aria-label="Record pages">
                        <button
                            type="button"
                            disabled={recordPage <= 1}
                            onclick={() => (recordPage -= 1)}>Previous</button
                        >
                        <span aria-live="polite">Page {recordPage} / {pageCount}</span>
                        <button
                            type="button"
                            disabled={recordPage >= pageCount}
                            onclick={() => (recordPage += 1)}>Next</button
                        >
                    </nav>
                {/if}
                <div class="table-scroll">
                    <table>
                        <thead>
                            <tr>
                                <th>Company</th>
                                <th>Year</th>
                                <th>Textile</th>
                                <th>Origin</th>
                                <th>Destination</th>
                                <th>Quantity</th>
                                <th>Value</th>
                            </tr>
                        </thead>
                        <tbody>
                            {#each visibleRecords as record}
                                <tr>
                                    <td
                                        ><span class={`company ${record.company.toLowerCase()}`}
                                            >{record.company}</span
                                        ></td
                                    >
                                    <td>{record.year ?? "—"}</td>
                                    <td><strong>{record.textile || "Not recorded"}</strong></td>
                                    <td>{record.origin || "Not recorded"}</td>
                                    <td>{record.destination || "Not recorded"}</td>
                                    <td>
                                        {record.quantity !== null
                                            ? record.quantity.toLocaleString()
                                            : "—"}
                                        <small>{record.unit}</small>
                                    </td>
                                    <td
                                        >{record.value !== null
                                            ? `ƒ${formatValue(record.value)}`
                                            : "—"}</td
                                    >
                                </tr>
                            {:else}
                                <tr><td colspan="7">No matching records.</td></tr>
                            {/each}
                        </tbody>
                    </table>
                </div>
            </div>
        {/if}
    </section>
</div>

<aside class="data-caveat">
    <div class="page-shell">
        <p class="eyebrow">Our Data</p>
        <p>
            A data-focused project brings with it the expectation of objective, clear, numeric data,
            but the hard facts remain elusive in this project, as they do in most data-driven
            endeavors. Humanistic data is subject to change over time, interpretation of compilers
            and researchers in the past and present.
        </p>
        <a href="/data/">Data methodology</a>
    </div>
</aside>

<style>
    .export-toolbar {
        display: flex;
        justify-content: flex-end;
        padding: 1rem 0;
    }
    .explorer-shell {
        font-variant-numeric: tabular-nums lining-nums;
        display: grid;
        grid-template-columns: 15rem minmax(0, 1fr);
        gap: 2.5rem;
        align-items: start;
        padding-top: 2rem;
        padding-bottom: 5rem;
        color: var(--ink);
        font-family: var(--sans);
    }
    .filters {
        position: sticky;
        top: 6rem;
        min-width: 0;
        max-height: calc(100svh - 7rem);
        overflow-y: auto;
        padding: 0 1.8rem 1rem 0;
        border-right: 1px solid var(--line-strong);
        scrollbar-width: thin;
    }
    .filter-heading {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.75rem;
        padding-bottom: 1.25rem;
        margin-bottom: 1.35rem;
        border-bottom: 1px solid var(--line);
    }
    .filter-heading > div {
        display: flex;
        align-items: center;
        gap: 0.5rem;
    }
    .filter-heading h2 {
        margin: 0;
        font: 700 0.95rem var(--sans);
        letter-spacing: var(--display-tracking, -0.015em);
    }
    .close-filters,
    .filter-backdrop {
        display: none;
    }
    .field {
        display: block;
        margin-bottom: 1.25rem;
    }
    .field > span,
    .year-field label span,
    .panel-heading span {
        display: block;
        margin-bottom: 0.5rem;
        color: var(--ink-soft);
        font-size: 0.8125rem;
        font-weight: 600;
    }
    select,
    .field input {
        width: 100%;
        min-height: 2.8rem;
        padding: 0.55rem 0.65rem;
        border: 1px solid var(--line-strong);
        border-radius: 0;
        color: var(--ink);
        background: transparent;
        font-size: 0.8125rem;
    }
    button:focus-visible,
    input:focus-visible,
    select:focus-visible {
        outline: 2px solid var(--accent-fill);
        outline-offset: 3px;
    }
    .search-field > div {
        display: flex;
        align-items: center;
        gap: 0.5rem;
        padding-left: 0.7rem;
        border: 1px solid var(--line-strong);
    }
    .search-field input {
        min-width: 0;
        border: 0;
        padding-left: 0;
    }
    .segmented {
        display: flex;
        border-bottom: 1px solid var(--line-strong);
    }
    .segmented button {
        flex: 1;
        min-height: 2.8rem;
        padding: 0.4rem;
        border: 0;
        background: none;
        color: inherit;
        font-size: 0.8125rem;
        cursor: pointer;
    }
    .segmented button.active {
        color: var(--paper);
        background: var(--ink);
    }
    .year-field > div {
        display: grid;
        grid-template-columns: 1fr 0.5rem 1fr;
        gap: 0.3rem;
        align-items: end;
    }
    .year-field i {
        height: 1px;
        margin-bottom: 1.4rem;
        background: var(--line-strong);
    }
    details {
        margin: 1.5rem 0;
        padding: 1rem 0;
        border-block: 1px solid var(--line);
    }
    summary {
        font-size: 0.85rem;
        cursor: pointer;
    }
    .modifier-fields {
        padding-top: 1.25rem;
    }
    .modifier-fields .field:last-child {
        margin-bottom: 0;
    }
    .reset-button {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 0.5rem;
        width: 100%;
        min-height: 2.8rem;
        border: 1px solid var(--line-strong);
        background: none;
        color: inherit;
        font-size: 0.8125rem;
        cursor: pointer;
    }
    .reset-button:disabled {
        opacity: 0.4;
        cursor: default;
    }
    .explorer-content {
        min-width: 0;
    }
    .mobile-toolbar {
        display: none;
    }
    .result-summary {
        display: grid;
        grid-template-columns: minmax(9rem, 1.7fr) repeat(3, 1fr);
        column-gap: 1.5rem;
        row-gap: 1rem;
        align-items: end;
        padding-bottom: 2rem;
    }
    .result-summary > div {
        min-width: 0;
    }
    .result-summary strong {
        display: block;
        font: 500 clamp(1.8rem, 3.8vw, 3.8rem)/1 var(--sans);
        letter-spacing: -0.035em;
        font-variant-numeric: tabular-nums lining-nums;
    }
    .result-count strong {
        margin-top: 0.6rem;
        font-size: clamp(2.5rem, 5.6vw, 5.5rem);
    }
    .result-summary span {
        display: block;
        margin-top: 0.5rem;
        font-size: 0.8125rem;
        color: var(--ink-soft);
    }
    .result-count p {
        margin: 0.55rem 0 0;
        color: var(--ink-soft);
        font-size: 0.8125rem;
    }
    .result-summary > button {
        grid-column: 1 / -1;
        justify-self: end;
        display: flex;
        align-items: center;
        gap: 0.45rem;
        min-height: 2.8rem;
        padding: 0.6rem 1rem;
        border: 0;
        color: white;
        background: var(--accent-fill);
        font-size: 0.8125rem;
        cursor: pointer;
    }
    .result-summary > button:disabled {
        opacity: 0.4;
    }
    .view-tabs {
        display: flex;
        gap: 2rem;
        border-bottom: 1px solid var(--line-strong);
    }
    .view-tabs button {
        position: relative;
        display: flex;
        align-items: center;
        gap: 0.5rem;
        min-height: 3.6rem;
        padding: 0.75rem 0;
        border: 0;
        background: none;
        color: var(--ink-soft);
        font-size: 0.9rem;
        cursor: pointer;
    }
    .view-tabs button.active {
        color: var(--ink);
        font-weight: 600;
    }
    .view-tabs button.active::after {
        position: absolute;
        content: "";
        bottom: -1px;
        left: 0;
        right: 0;
        height: 3px;
        background: var(--accent-fill);
    }
    .chart-panel,
    .routes-panel,
    .records-panel {
        min-height: 34rem;
        padding-top: 1.6rem;
    }
    .panel-heading {
        display: flex;
        justify-content: flex-end;
        gap: 1rem;
        margin-bottom: 2rem;
    }
    .panel-heading > div {
        min-width: 10rem;
    }
    .panel-heading select {
        min-height: 2.6rem;
    }
    .bar-chart {
        display: grid;
    }
    .bar-row {
        display: grid;
        grid-template-columns: 1.6rem minmax(7rem, 0.65fr) minmax(5rem, 1fr) 5.5rem;
        gap: 1rem;
        align-items: center;
        min-height: 3.5rem;
        border-bottom: 1px solid var(--line);
    }
    .bar-row:hover {
        background: color-mix(in srgb, var(--ink) 2%, transparent);
    }
    .bar-rank {
        color: var(--ink-soft);
        font-size: 0.8125rem;
        font-variant-numeric: tabular-nums lining-nums;
    }
    .bar-name {
        overflow: hidden;
        white-space: nowrap;
        text-overflow: ellipsis;
        font-size: 0.86rem;
        font-weight: 500;
    }
    .bar-track {
        height: 1.1rem;
        background: color-mix(in srgb, var(--ink) 3%, transparent);
    }
    .bar-track i {
        display: block;
        width: var(--bar);
        min-width: 1px;
        height: 100%;
        background: var(--accent-fill);
        transition: width 240ms ease;
    }
    .bar-row strong {
        font-size: 0.8125rem;
        font-weight: 500;
        font-variant-numeric: tabular-nums lining-nums;
        text-align: right;
    }
    .chart-note {
        max-width: 68ch;
        margin: 2rem 0 0;
        font-size: 0.8125rem;
        line-height: 1.6;
        color: var(--ink-soft);
    }
    .route-heading {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 1rem;
        color: var(--ink-soft);
        font-size: 0.8125rem;
    }
    .route-heading > div {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 3rem;
        width: 70%;
        padding-left: 2.6rem;
    }
    .route-heading p {
        margin: 0;
    }
    .route-list article {
        display: grid;
        grid-template-columns: 1.6rem minmax(0, 1fr) auto;
        align-items: center;
        gap: 1rem;
        min-height: 5.2rem;
        border-top: 1px solid var(--line);
    }
    .route-index {
        color: var(--ink-soft);
        font-size: 0.8125rem;
    }
    .route-points {
        display: grid;
        grid-template-columns: 1fr 2rem 1fr;
        gap: 1rem;
        align-items: center;
    }
    .route-points strong {
        min-width: 0;
        font: 500 0.92rem var(--sans);
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
    }
    .route-points i {
        height: 1px;
        background: var(--accent-fill);
        position: relative;
    }
    .route-points i::after {
        content: "";
        position: absolute;
        width: 5px;
        height: 5px;
        right: 0;
        top: -2px;
        background: var(--accent-fill);
        border-radius: 50%;
    }
    .route-meta {
        display: grid;
        gap: 0.25rem;
        min-width: 7rem;
        font-size: 0.8125rem;
        color: var(--ink-soft);
        text-align: right;
    }
    .record-pagination {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 1rem;
        margin-bottom: 1rem;
        font-size: 0.8125rem;
        font-variant-numeric: tabular-nums lining-nums;
    }
    .record-pagination button {
        min-height: 2.75rem;
        padding: 0.5rem 0.8rem;
        border: 1px solid var(--line-strong);
        background: none;
        cursor: pointer;
    }
    .record-pagination button:disabled {
        opacity: 0.4;
        cursor: default;
    }
    .table-scroll {
        overflow-x: auto;
        border-top: 1px solid var(--line-strong);
    }
    table {
        width: 100%;
        min-width: 52rem;
        border-collapse: collapse;
        font-size: 0.875rem;
        font-variant-numeric: tabular-nums lining-nums;
    }
    th {
        padding: 0.8rem 0.65rem;
        border-bottom: 1px solid var(--line-strong);
        color: var(--ink-soft);
        font-size: 0.8125rem;
        font-weight: 500;
        text-align: left;
    }
    td {
        max-width: 14rem;
        padding: 0.9rem 0.65rem;
        overflow: hidden;
        border-bottom: 1px solid var(--line);
        text-overflow: ellipsis;
        white-space: nowrap;
    }
    td strong {
        font-weight: 550;
    }
    td small {
        color: var(--ink-soft);
    }
    .company {
        display: inline-block;
        padding: 0.2rem 0.35rem;
        border: 1px solid currentColor;
        color: var(--ink);
        font-size: 0.8125rem;
        font-weight: 600;
    }
    .company.wic {
        color: var(--accent-fill);
    }
    .empty-panel {
        display: grid;
        place-items: center;
        align-content: center;
        min-height: 25rem;
        color: var(--ink-soft);
    }
    .empty-panel h2 {
        margin: 1rem 0;
        color: var(--ink);
        font: 500 2rem var(--sans);
        letter-spacing: var(--display-tracking, -0.04em);
    }
    .empty-panel button {
        min-height: 2.75rem;
        padding: 0.5rem 0;
        border: 0;
        background: none;
        text-decoration: underline;
        text-underline-offset: 3px;
        font-size: 0.8125rem;
        cursor: pointer;
    }
    .data-caveat {
        border-top: 1px solid var(--line-strong);
        padding: 4rem 0;
    }
    .data-caveat > div {
        display: grid;
        grid-template-columns: 1fr 2fr auto;
        gap: 2rem;
        align-items: start;
    }
    .data-caveat p {
        margin: 0;
    }
    .data-caveat .eyebrow {
        font: 500 1.5rem var(--sans);
        letter-spacing: -0.03em;
        color: var(--ink);
        text-transform: none;
    }
    .data-caveat > div > p:not(.eyebrow) {
        max-width: 63ch;
        font-size: 0.95rem;
        line-height: 1.65;
        color: var(--ink-soft);
    }
    .data-caveat a {
        font-size: 0.8125rem;
        text-underline-offset: 4px;
    }
    @media (max-width: 1100px) {
        .explorer-shell {
            grid-template-columns: 13rem minmax(0, 1fr);
            gap: 1.5rem;
        }
        .filters {
            padding-right: 1rem;
        }
        .result-summary {
            column-gap: 1rem;
        }
        .bar-row {
            gap: 0.6rem;
            grid-template-columns: 1.2rem minmax(6rem, 0.6fr) minmax(4rem, 1fr) 4.5rem;
        }
    }
    @media (max-width: 900px) {
        select,
        .field input {
            font-size: 1rem;
        }
        .explorer-shell {
            display: block;
            padding-top: 1rem;
        }
        .filters {
            position: fixed;
            z-index: 90;
            inset: 0 auto 0 0;
            width: min(90vw, 23rem);
            max-height: none;
            padding: 1.5rem;
            background: var(--paper);
            visibility: hidden;
            transform: translateX(-105%);
            transition: transform 220ms ease;
        }
        .filters.open {
            visibility: visible;
            transform: translateX(0);
        }
        .filter-backdrop {
            display: block;
            position: fixed;
            z-index: 85;
            inset: 0;
            border: 0;
            background: #17171180;
            backdrop-filter: blur(3px);
        }
        .close-filters {
            display: grid;
            place-items: center;
            width: 2.75rem;
            height: 2.75rem;
            border: 0;
            background: none;
            cursor: pointer;
        }
        .close-filters span {
            position: absolute;
            width: 1px;
            height: 1px;
            overflow: hidden;
            clip: rect(0, 0, 0, 0);
        }
        .mobile-toolbar {
            display: flex;
            justify-content: space-between;
            padding-bottom: 1.5rem;
        }
        .mobile-toolbar button {
            display: flex;
            gap: 0.5rem;
            align-items: center;
            min-height: 2.8rem;
            padding: 0.5rem 0.75rem;
            border: 1px solid var(--line-strong);
            background: none;
            font-size: 0.8125rem;
        }
        .mobile-toolbar span {
            color: var(--accent-fill);
        }
        .result-summary > button {
            display: none;
        }
        .result-summary {
            margin-bottom: 1rem;
        }
        .data-caveat > div {
            grid-template-columns: 1fr;
            gap: 1.5rem;
        }
    }
    @media (max-width: 550px) {
        .result-summary {
            grid-template-columns: repeat(3, 1fr);
            gap: 1.5rem 1rem;
        }
        .result-count {
            grid-column: 1/-1;
        }
        .result-count strong {
            font-size: 4.5rem;
        }
        .result-summary strong {
            font-size: 2.4rem;
        }
        .result-count strong {
            font-size: 4.5rem;
        }
        .view-tabs {
            gap: 1.8rem;
        }
        .panel-heading {
            display: grid;
            grid-template-columns: 1fr 1fr;
        }
        .panel-heading > div {
            min-width: 0;
        }
        .bar-row {
            grid-template-columns: 1rem minmax(5rem, 0.7fr) minmax(2rem, 1fr) 3.7rem;
            gap: 0.5rem;
        }
        .bar-name {
            font-size: 0.8125rem;
        }
        .bar-row strong {
            font-size: 0.8125rem;
        }
        .bar-track {
            height: 0.9rem;
        }
        .route-heading {
            display: none;
        }
        .route-list article {
            grid-template-columns: 1.2rem 1fr;
            padding: 0.9rem 0;
            gap: 0.5rem;
        }
        .route-points {
            grid-template-columns: 1fr 1.5rem 1fr;
            gap: 0.6rem;
        }
        .route-points strong {
            font-size: 0.83rem;
        }
        .route-meta {
            grid-column: 2;
            display: flex;
            gap: 1rem;
            text-align: left;
        }
    }
</style>
