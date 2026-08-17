<script lang="ts">
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
    import { onMount } from "svelte";
    import type { TradeRecord } from "$lib/server/trade";
    import type { PageData } from "./$types";

    let { data }: { data: PageData } = $props();

    // This page is prerendered and its load data does not change after initialization.
    // svelte-ignore state_referenced_locally
    const initialData = $state.snapshot(data);
    const firstYear = initialData.options.years[0] ?? 1700;
    const lastYear = initialData.options.years.at(-1) ?? 1724;

    let company = $state("");
    let textile = $state("");
    let origin = $state("");
    let destination = $state("");
    let color = $state("");
    let pattern = $state("");
    let fiber = $state("");
    let yearFrom = $state(firstYear);
    let yearTo = $state(lastYear);
    let search = $state("");
    let chartDimension = $state<"textile" | "destination" | "origin" | "year">("textile");
    let chartMetric = $state<"records" | "value">("records");
    let activeTab = $state<"chart" | "routes" | "records">("chart");
    let filtersOpen = $state(false);

    onMount(() => {
        textile = new URL(window.location.href).searchParams.get("textile") ?? "";
    });

    const normalize = (value: string) => value.toLocaleLowerCase("en").trim();

    const filtered = $derived.by(() =>
        data.records.filter((record) => {
            const textileMatch =
                !textile ||
                normalize(record.textile) === normalize(textile) ||
                normalize(record.textile).includes(normalize(textile));
            const searchText = [
                record.textile,
                record.origin,
                record.destination,
                record.color,
                record.pattern,
                record.process,
                record.fiber,
            ]
                .join(" ")
                .toLocaleLowerCase("en");

            return (
                (!company || record.company === company) &&
                textileMatch &&
                (!origin || record.origin === origin) &&
                (!destination || record.destination === destination) &&
                (!color || record.color === color) &&
                (!pattern || record.pattern === pattern) &&
                (!fiber || record.fiber === fiber) &&
                (record.year === null || (record.year >= yearFrom && record.year <= yearTo)) &&
                (!search || searchText.includes(search.toLocaleLowerCase("en").trim()))
            );
        }),
    );

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
            current.textiles.add(record.textile);
            routes.set(key, current);
        }

        return [...routes.values()]
            .sort((a, b) => b.records - a.records)
            .slice(0, 18)
            .map((route) => ({ ...route, textiles: route.textiles.size }));
    });

    const activeFilters = $derived(
        [company, textile, origin, destination, color, pattern, fiber].filter(Boolean).length +
            (yearFrom !== firstYear || yearTo !== lastYear ? 1 : 0) +
            (search ? 1 : 0),
    );

    function resetFilters() {
        company = "";
        textile = "";
        origin = "";
        destination = "";
        color = "";
        pattern = "";
        fiber = "";
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
            "year",
            "origin",
            "destination",
            "textile",
            "quantity",
            "unit",
            "value",
            "color",
            "pattern",
            "process",
            "fiber",
            "quality",
        ];
        const rows = [
            columns.join(","),
            ...filtered.map((record) => columns.map((column) => csvCell(record[column])).join(",")),
        ];
        const blob = new Blob([rows.join("\n")], { type: "text/csv;charset=utf-8" });
        const url = URL.createObjectURL(blob);
        const anchor = document.createElement("a");
        anchor.href = url;
        anchor.download = "dutch-textile-trade-filtered.csv";
        anchor.click();
        URL.revokeObjectURL(url);
    }
</script>

<svelte:head>
    <title>Data Table — Dutch Textile Trade</title>
    <meta
        name="description"
        content="These dynamic apps allow users to explore the project data by geography, date, company, textile name and/or descriptors of the textiles found in archival sources (modifiers), and value of textiles, producing data visualizations."
    />
</svelte:head>

<div class="explorer-head page-shell">
    <div>
        <p class="eyebrow">Data Visualization</p>
        <h1>Data Table</h1>
    </div>
    <p>
        These dynamic apps allow users to explore the project data by geography, date, company,
        textile name and/or descriptors of the textiles found in archival sources (modifiers), and
        value of textiles, producing data visualizations.
    </p>
</div>

<div class="explorer-shell page-shell">
    <aside class:open={filtersOpen} class="filters">
        <div class="filter-heading">
            <div>
                <Filter size={17} strokeWidth={1.5} />
                <h2>Refine the records</h2>
            </div>
            <button class="close-filters" type="button" onclick={() => (filtersOpen = false)}>
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

        <label class="field">
            <span>Textile name</span>
            <select bind:value={textile}>
                <option value="">All textile names</option>
                {#each data.options.textiles as item}
                    <option value={item}>{item}</option>
                {/each}
            </select>
        </label>

        <label class="field">
            <span>Origin region</span>
            <select bind:value={origin}>
                <option value="">All origins</option>
                {#each data.options.origins as item}
                    <option value={item}>{item}</option>
                {/each}
            </select>
        </label>

        <label class="field">
            <span>Destination region</span>
            <select bind:value={destination}>
                <option value="">All destinations</option>
                {#each data.options.destinations as item}
                    <option value={item}>{item}</option>
                {/each}
            </select>
        </label>

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
                <label class="field">
                    <span>Color</span>
                    <select bind:value={color}>
                        <option value="">All colors</option>
                        {#each data.options.colors as item}
                            <option value={item}>{item}</option>
                        {/each}
                    </select>
                </label>
                <label class="field">
                    <span>Pattern</span>
                    <select bind:value={pattern}>
                        <option value="">All patterns</option>
                        {#each data.options.patterns as item}
                            <option value={item}>{item}</option>
                        {/each}
                    </select>
                </label>
                <label class="field">
                    <span>Fiber</span>
                    <select bind:value={fiber}>
                        <option value="">All fibers</option>
                        {#each data.options.fibers as item}
                            <option value={item}>{item}</option>
                        {/each}
                    </select>
                </label>
            </div>
        </details>

        <button class="reset-button" type="button" onclick={resetFilters} disabled={!activeFilters}>
            <RotateCcw size={14} />
            Reset {activeFilters
                ? `${activeFilters} active ${activeFilters === 1 ? "filter" : "filters"}`
                : "filters"}
        </button>
    </aside>

    <section class="explorer-content">
        <div class="mobile-toolbar">
            <button type="button" onclick={() => (filtersOpen = true)}>
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
                        <h2>No records match this view.</h2>
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
                    <p>Top routes by matching record count</p>
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
                        <h2>No complete routes match this view.</h2>
                        <button type="button" onclick={resetFilters}>Clear filters</button>
                    </div>
                {/if}
            </div>
        {:else}
            <div class="records-panel" role="tabpanel">
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
                            {#each filtered.slice(0, 100) as record}
                                <tr>
                                    <td
                                        ><span class={`company ${record.company.toLowerCase()}`}
                                            >{record.company}</span
                                        ></td
                                    >
                                    <td>{record.year ?? "—"}</td>
                                    <td><strong>{record.textile}</strong></td>
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
                            {/each}
                        </tbody>
                    </table>
                </div>
                {#if filtered.length > 100}
                    <p class="table-note">
                        Showing 100 of {filtered.length.toLocaleString()} records. Download for all results.
                    </p>
                {/if}
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
        <a href="/data/">Read the full data methodology</a>
    </div>
</aside>

<style>
    .explorer-head {
        display: grid;
        grid-template-columns: 1fr minmax(18rem, 0.38fr);
        gap: 3rem;
        align-items: end;
        padding-top: clamp(3.5rem, 8vw, 7rem);
        padding-bottom: clamp(2.5rem, 5vw, 4rem);
    }

    .explorer-head h1 {
        margin-bottom: 0;
        font-family: var(--serif);
        font-size: clamp(4rem, 8vw, 8rem);
        font-weight: 400;
        letter-spacing: -0.065em;
        line-height: 0.88;
    }

    .explorer-head > p {
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--serif);
        font-size: 1.05rem;
    }

    .explorer-shell {
        display: grid;
        grid-template-columns: 18.5rem minmax(0, 1fr);
        align-items: start;
        padding-bottom: clamp(5rem, 10vw, 10rem);
    }

    .filters {
        position: sticky;
        top: 7rem;
        max-height: calc(100svh - 8.5rem);
        padding: 1.2rem;
        overflow-y: auto;
        border: 1px solid var(--line-strong);
        background: var(--paper-deep);
    }

    .filter-heading {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 1.5rem;
        padding-bottom: 1rem;
        border-bottom: 1px solid var(--line);
    }

    .filter-heading > div {
        display: flex;
        align-items: center;
        gap: 0.6rem;
    }

    .filter-heading h2 {
        margin: 0;
        font-family: var(--serif);
        font-size: 1.08rem;
        font-weight: 400;
    }

    .close-filters {
        display: none;
    }

    .field {
        display: block;
        margin-bottom: 1rem;
    }

    .field > span,
    .panel-heading span,
    .year-field label span {
        display: block;
        margin-bottom: 0.4rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.52rem;
        letter-spacing: 0.07em;
        text-transform: uppercase;
    }

    select,
    .field input {
        width: 100%;
        min-height: 2.65rem;
        padding: 0.55rem 0.7rem;
        color: var(--ink);
        border: 1px solid var(--line-strong);
        border-radius: 0;
        outline: 0;
        background: var(--cream);
        font-size: 0.72rem;
    }

    select:focus,
    .field input:focus {
        border-color: var(--madder);
    }

    .search-field > div {
        display: flex;
        align-items: center;
        gap: 0.55rem;
        min-height: 2.65rem;
        padding: 0 0.7rem;
        border: 1px solid var(--line-strong);
        background: var(--cream);
    }

    .search-field input {
        min-height: 2.5rem;
        padding: 0;
        border: 0;
        background: transparent;
    }

    .segmented {
        display: grid;
        grid-template-columns: repeat(3, 1fr);
        border: 1px solid var(--line-strong);
    }

    .segmented button {
        min-height: 2.65rem;
        padding: 0.5rem;
        color: var(--ink-soft);
        border: 0;
        border-left: 1px solid var(--line);
        background: transparent;
        font-family: var(--sans);
        font-size: 0.58rem;
        cursor: pointer;
    }

    .segmented button:first-child {
        border-left: 0;
    }

    .segmented button.active {
        color: var(--cream);
        background: var(--ink);
    }

    .year-field > div {
        display: grid;
        grid-template-columns: 1fr 1rem 1fr;
        gap: 0.5rem;
        align-items: end;
    }

    .year-field label span {
        font-size: 0.48rem;
    }

    .year-field i {
        height: 1px;
        margin-bottom: 1.3rem;
        background: var(--line-strong);
    }

    details {
        margin: 1.2rem 0;
        padding: 0.8rem 0;
        border-top: 1px solid var(--line);
        border-bottom: 1px solid var(--line);
    }

    summary {
        font-family: var(--serif);
        font-size: 0.82rem;
        cursor: pointer;
    }

    .modifier-fields {
        padding-top: 1rem;
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
        color: var(--ink);
        border: 1px solid var(--line-strong);
        background: transparent;
        font-size: 0.64rem;
        font-weight: 700;
        cursor: pointer;
    }

    .reset-button:disabled {
        cursor: default;
        opacity: 0.4;
    }

    .explorer-content {
        min-width: 0;
        border-top: 1px solid var(--line-strong);
        border-right: 1px solid var(--line-strong);
        border-bottom: 1px solid var(--line-strong);
    }

    .mobile-toolbar {
        display: none;
    }

    .result-summary {
        display: grid;
        grid-template-columns: 1.2fr repeat(3, 0.65fr) auto;
        min-height: 7.4rem;
        border-bottom: 1px solid var(--line-strong);
    }

    .result-summary > div {
        display: flex;
        flex-direction: column;
        justify-content: center;
        min-width: 0;
        padding: 1.1rem;
        border-right: 1px solid var(--line);
    }

    .result-count {
        display: grid !important;
        grid-template-columns: auto 1fr;
        grid-template-rows: auto auto;
        column-gap: 0.75rem;
        align-content: center;
    }

    .result-count > span {
        grid-column: 1 / -1;
    }

    .result-count strong {
        font-size: clamp(2rem, 4vw, 3.4rem) !important;
    }

    .result-count p {
        align-self: end;
        margin: 0 0 0.4rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.55rem;
    }

    .result-summary strong {
        overflow: hidden;
        font-family: var(--serif);
        font-size: clamp(1.5rem, 2.6vw, 2.4rem);
        font-weight: 400;
        line-height: 1;
        text-overflow: ellipsis;
    }

    .result-summary span {
        margin-top: 0.35rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.5rem;
        letter-spacing: 0.07em;
        text-transform: uppercase;
    }

    .result-summary > button {
        display: flex;
        align-items: center;
        justify-content: center;
        gap: 0.45rem;
        padding: 0 1.2rem;
        color: var(--paper);
        border: 0;
        background: var(--ink);
        font-size: 0.62rem;
        font-weight: 700;
        cursor: pointer;
    }

    .result-summary > button:disabled {
        cursor: default;
        opacity: 0.45;
    }

    .view-tabs {
        display: flex;
        padding: 1rem 1.2rem 0;
        border-bottom: 1px solid var(--line-strong);
    }

    .view-tabs button {
        display: flex;
        align-items: center;
        gap: 0.4rem;
        min-height: 2.8rem;
        padding: 0.6rem 1rem;
        color: var(--ink-soft);
        border: 1px solid transparent;
        border-bottom: 0;
        background: transparent;
        font-size: 0.67rem;
        font-weight: 700;
        cursor: pointer;
    }

    .view-tabs button.active {
        position: relative;
        bottom: -1px;
        color: var(--ink);
        border-color: var(--line-strong);
        background: var(--paper);
    }

    .chart-panel,
    .routes-panel,
    .records-panel {
        min-height: 39rem;
        padding: clamp(1.2rem, 3vw, 2.5rem);
    }

    .panel-heading {
        display: flex;
        gap: 1rem;
        justify-content: flex-end;
        margin-bottom: 2.5rem;
    }

    .panel-heading > div {
        min-width: 11rem;
    }

    .panel-heading select {
        min-height: 2.5rem;
    }

    .bar-chart {
        display: grid;
        gap: 0.68rem;
    }

    .bar-row {
        display: grid;
        grid-template-columns: 2rem minmax(8rem, 0.55fr) minmax(10rem, 1fr) 5.5rem;
        gap: 0.8rem;
        align-items: center;
        min-height: 2rem;
    }

    .bar-rank {
        color: var(--madder);
        font-family: var(--sans);
        font-size: 0.52rem;
    }

    .bar-name {
        overflow: hidden;
        font-family: var(--serif);
        font-size: 0.78rem;
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    .bar-track {
        height: 0.7rem;
        background: var(--paper-deep);
    }

    .bar-track i {
        display: block;
        width: var(--bar);
        min-width: 2px;
        height: 100%;
        background: var(--indigo);
        transform-origin: left;
    }

    .bar-row strong {
        font-family: var(--sans);
        font-size: 0.57rem;
        font-weight: 500;
        text-align: right;
    }

    .chart-note,
    .table-note {
        margin: 1.5rem 0 0;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.52rem;
    }

    .route-heading {
        display: flex;
        align-items: center;
        justify-content: space-between;
        margin-bottom: 1.5rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.52rem;
        letter-spacing: 0.07em;
        text-transform: uppercase;
    }

    .route-heading > div {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 4rem;
        width: 65%;
    }

    .route-heading p {
        margin: 0;
    }

    .route-list {
        border-top: 1px solid var(--line-strong);
    }

    .route-list article {
        display: grid;
        grid-template-columns: 2rem minmax(0, 1fr) auto;
        gap: 1rem;
        align-items: center;
        min-height: 4.5rem;
        border-bottom: 1px solid var(--line);
    }

    .route-index {
        color: var(--madder);
        font-family: var(--sans);
        font-size: 0.5rem;
    }

    .route-points {
        display: grid;
        grid-template-columns: minmax(5rem, 1fr) minmax(3rem, 0.35fr) minmax(5rem, 1fr);
        gap: 1rem;
        align-items: center;
    }

    .route-points strong {
        overflow: hidden;
        font-family: var(--serif);
        font-size: 0.76rem;
        font-weight: 400;
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    .route-points i {
        position: relative;
        height: 1px;
        background: var(--line-strong);
    }

    .route-points i::before,
    .route-points i::after {
        position: absolute;
        top: 50%;
        width: 0.35rem;
        height: 0.35rem;
        content: "";
        border-radius: 50%;
        background: var(--indigo);
        transform: translateY(-50%);
    }

    .route-points i::after {
        right: 0;
        background: var(--madder);
    }

    .route-meta {
        display: grid;
        min-width: 8rem;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.49rem;
        text-align: right;
    }

    .table-scroll {
        overflow-x: auto;
        border: 1px solid var(--line-strong);
    }

    table {
        width: 100%;
        min-width: 58rem;
        border-collapse: collapse;
        font-size: 0.68rem;
    }

    th {
        padding: 0.7rem;
        color: var(--ink-soft);
        border-bottom: 1px solid var(--line-strong);
        background: var(--paper-deep);
        font-family: var(--sans);
        font-size: 0.49rem;
        font-weight: 600;
        letter-spacing: 0.06em;
        text-align: left;
        text-transform: uppercase;
    }

    td {
        max-width: 14rem;
        padding: 0.72rem;
        overflow: hidden;
        border-bottom: 1px solid var(--line);
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    td strong {
        font-family: var(--serif);
        font-weight: 600;
    }

    td small {
        color: var(--ink-soft);
    }

    .company {
        padding: 0.25rem 0.35rem;
        color: var(--paper);
        background: var(--indigo);
        font-family: var(--sans);
        font-size: 0.48rem;
    }

    .company.wic {
        background: var(--madder);
    }

    .empty-panel {
        display: grid;
        place-items: center;
        align-content: center;
        min-height: 27rem;
        color: var(--ink-soft);
        text-align: center;
    }

    .empty-panel h2 {
        max-width: 16ch;
        margin: 1rem 0;
        color: var(--ink);
        font-family: var(--serif);
        font-size: 2rem;
        font-weight: 400;
    }

    .empty-panel button {
        padding: 0;
        color: var(--madder);
        border: 0;
        border-bottom: 1px solid currentColor;
        background: transparent;
        font-size: 0.7rem;
        cursor: pointer;
    }

    .data-caveat {
        padding: clamp(3.5rem, 7vw, 6rem) 0;
        color: var(--paper);
        background: var(--madder-dark);
    }

    .data-caveat > div {
        display: grid;
        grid-template-columns: 12rem 1fr auto;
        gap: 3rem;
        align-items: center;
    }

    .data-caveat .eyebrow {
        margin: 0;
        color: var(--saffron);
    }

    .data-caveat > div > p:not(.eyebrow) {
        max-width: 52rem;
        margin: 0;
        color: rgba(244, 239, 229, 0.76);
        font-family: var(--serif);
        font-size: clamp(1rem, 1.6vw, 1.25rem);
    }

    .data-caveat a {
        color: var(--paper);
        font-size: 0.7rem;
        font-weight: 700;
        text-underline-offset: 0.3rem;
    }

    @media (max-width: 1000px) {
        .explorer-shell {
            grid-template-columns: 15rem minmax(0, 1fr);
        }

        .result-summary {
            grid-template-columns: 1.1fr repeat(3, 0.6fr);
        }

        .result-summary > button {
            display: none;
        }

        .bar-row {
            grid-template-columns: 1.5rem minmax(7rem, 0.55fr) minmax(7rem, 1fr) 4.5rem;
        }
    }

    @media (max-width: 800px) {
        .explorer-head {
            grid-template-columns: 1fr;
        }

        .explorer-shell {
            display: block;
            padding-right: 0;
            padding-left: 0;
        }

        .filters {
            position: fixed;
            z-index: 80;
            top: 0;
            bottom: 0;
            left: 0;
            width: min(90vw, 23rem);
            max-height: none;
            padding: 1.5rem;
            visibility: hidden;
            box-shadow: 1.5rem 0 3rem rgba(0, 0, 0, 0.22);
            transform: translateX(-105%);
            transition:
                visibility 220ms,
                transform 220ms ease;
        }

        .filters.open {
            visibility: visible;
            transform: translateX(0);
        }

        .close-filters {
            display: grid;
            place-items: center;
            padding: 0.3rem;
            border: 0;
            background: transparent;
            cursor: pointer;
        }

        .close-filters span {
            position: absolute;
            width: 1px;
            height: 1px;
            overflow: hidden;
            clip: rect(0, 0, 0, 0);
        }

        .explorer-content {
            border-right: 0;
            border-left: 0;
        }

        .mobile-toolbar {
            display: flex;
            justify-content: space-between;
            padding: 0.75rem var(--page-pad);
            border-bottom: 1px solid var(--line-strong);
        }

        .mobile-toolbar button {
            display: flex;
            align-items: center;
            gap: 0.4rem;
            padding: 0.5rem;
            border: 0;
            background: transparent;
            font-size: 0.65rem;
            font-weight: 700;
        }

        .mobile-toolbar span {
            display: grid;
            place-items: center;
            width: 1.2rem;
            height: 1.2rem;
            color: var(--paper);
            border-radius: 50%;
            background: var(--madder);
            font-family: var(--sans);
            font-size: 0.48rem;
        }

        .data-caveat > div {
            grid-template-columns: 1fr;
            gap: 1.5rem;
        }
    }

    @media (max-width: 620px) {
        .result-summary {
            grid-template-columns: 1fr 1fr 1fr;
        }

        .result-count {
            grid-column: 1 / -1;
            border-bottom: 1px solid var(--line);
        }

        .result-summary > div:nth-child(4) {
            border-right: 0;
        }

        .view-tabs {
            padding-right: 0.5rem;
            padding-left: 0.5rem;
        }

        .view-tabs button {
            flex: 1;
            justify-content: center;
            padding: 0.5rem;
        }

        .panel-heading {
            display: grid;
            grid-template-columns: 1fr 1fr;
        }

        .panel-heading > div {
            min-width: 0;
        }

        .bar-row {
            grid-template-columns: 1.2rem minmax(6rem, 0.8fr) 1fr 3.5rem;
            gap: 0.45rem;
        }

        .bar-name {
            font-size: 0.68rem;
        }

        .bar-row strong {
            font-size: 0.49rem;
        }

        .route-list article {
            grid-template-columns: 1.5rem 1fr;
            padding: 0.8rem 0;
        }

        .route-meta {
            grid-column: 2;
            display: flex;
            gap: 1rem;
            text-align: left;
        }

        .route-heading {
            display: none;
        }
    }
</style>
