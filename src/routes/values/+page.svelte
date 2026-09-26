<script lang="ts">
    import MultiSelect from "$lib/components/MultiSelect.svelte";
    import ChartDownload from "$lib/components/ChartDownload.svelte";
    import { downloadBlob, downloadChart, exportFilename } from "$lib/utils/download";

    import { ArrowDownToLine, BarChart3, Check, Database, RotateCcw, Scale } from "@lucide/svelte";
    import { onMount } from "svelte";
    import type { TradeRecord } from "$lib/server/trade";
    import type { PageData } from "./$types";
    import CompanyMark from "$lib/components/CompanyMark.svelte";
    import ResearchAppHeader from "$lib/components/ResearchAppHeader.svelte";

    let { data }: { data: PageData } = $props();

    // This static route receives an immutable build-time snapshot of the research data.
    // svelte-ignore state_referenced_locally
    const initialData = $state.snapshot(data);
    const firstYear = initialData.options.years[0] ?? 1700;
    const lastYear = initialData.options.years.at(-1) ?? 1724;

    type ModifierField =
        | "company"
        | "color"
        | "inferredColor"
        | "geography"
        | "other"
        | "pattern"
        | "process"
        | "fiber"
        | "quality"
        | "origin"
        | "destination";
    type Metric = "records" | "value" | "quantity" | "pricePerUnit";
    type Dimension = "year" | "origin" | "destination" | "textile";

    const modifierFields: { value: ModifierField; label: string }[] = [
        { value: "company", label: "Company network" },
        { value: "color", label: "Archival color" },
        { value: "inferredColor", label: "Inferred color" },
        { value: "geography", label: "Archival geography" },
        { value: "other", label: "Other" },
        { value: "pattern", label: "Pattern" },
        { value: "process", label: "Process" },
        { value: "fiber", label: "Fiber" },
        { value: "quality", label: "Quality" },
        { value: "origin", label: "Origin region" },
        { value: "destination", label: "Destination region" },
    ];

    let textile = $state<string[]>([]);
    let yearFrom = $state(firstYear);
    let yearTo = $state(lastYear);
    let metric = $state<Metric>("value");
    let dimension = $state<Dimension>("year");
    let scale = $state<"cohort" | "absolute">("absolute");
    let unit = $state("");
    let fieldA = $state<ModifierField>("company");
    let valueA = $state<string[]>(["VOC"]);
    let fieldB = $state<ModifierField>("company");
    let valueB = $state<string[]>(["WIC"]);
    let downloadReady = $state(false);
    let controlsOpen = $state(true);

    onMount(() => {
        controlsOpen = !window.matchMedia("(max-width: 900px)").matches;
        const params = new URL(window.location.href).searchParams;
        const requestedTextile = params.get("textile") ?? params.get("name");
        if (requestedTextile)
            textile = params.getAll("textile").length
                ? params.getAll("textile")
                : [requestedTextile];
    });

    const normalize = (value: string) => value.toLocaleLowerCase("en").trim();
    const baseRecords = $derived.by(() =>
        data.records.filter(
            (record) =>
                (!textile.length ||
                    textile.some((value) => normalize(record.textile) === normalize(value))) &&
                (record.year === null || (record.year >= yearFrom && record.year <= yearTo)),
        ),
    );

    const unitOptions = $derived.by(() => {
        const counts = new Map<string, number>();
        for (const record of baseRecords) {
            const recordUnit = metric === "pricePerUnit" ? record.priceUnit : record.unit;
            const amount = metric === "pricePerUnit" ? record.pricePerUnit : record.quantity;
            if (recordUnit && amount !== null) {
                counts.set(recordUnit, (counts.get(recordUnit) ?? 0) + 1);
            }
        }
        return [...counts]
            .map(([name, count]) => ({ name, count }))
            .sort((a, b) => b.count - a.count || a.name.localeCompare(b.name));
    });
    const activeUnit = $derived(unit || unitOptions[0]?.name || "");

    function modifierValue(record: TradeRecord, field: ModifierField) {
        return record[field];
    }

    function modifierLabel(field: ModifierField) {
        return modifierFields.find((item) => item.value === field)?.label ?? field;
    }

    function modifierOptions(field: ModifierField) {
        return [
            ...new Set(
                baseRecords
                    .map((record) => modifierValue(record, field))
                    .filter((value): value is string => Boolean(value)),
            ),
        ].sort((a, b) => a.localeCompare(b));
    }

    $effect(() => {
        const optionsA = modifierOptions(fieldA);
        const optionsB = modifierOptions(fieldB);
        if (valueA.some((value) => !optionsA.includes(value)))
            valueA = valueA.filter((value) => optionsA.includes(value));
        if (valueB.some((value) => !optionsB.includes(value)))
            valueB = valueB.filter((value) => optionsB.includes(value));
        if (unit && !unitOptions.some((option) => option.name === unit)) unit = "";
    });

    function matchesCohort(record: TradeRecord, field: ModifierField, value: string[]) {
        return !value.length || value.includes(modifierValue(record, field));
    }

    const cohortA = $derived(baseRecords.filter((record) => matchesCohort(record, fieldA, valueA)));
    const cohortB = $derived(baseRecords.filter((record) => matchesCohort(record, fieldB, valueB)));
    const labelA = $derived(
        valueA.length ? valueA.join(" + ") : "All " + modifierLabel(fieldA).toLocaleLowerCase("en"),
    );
    const labelB = $derived(
        valueB.length ? valueB.join(" + ") : "All " + modifierLabel(fieldB).toLocaleLowerCase("en"),
    );
    const companyA = $derived(
        fieldA === "company" && valueA.length === 1 && (valueA[0] === "VOC" || valueA[0] === "WIC")
            ? (valueA[0] as "VOC" | "WIC")
            : null,
    );
    const companyB = $derived(
        fieldB === "company" && valueB.length === 1 && (valueB[0] === "VOC" || valueB[0] === "WIC")
            ? (valueB[0] as "VOC" | "WIC")
            : null,
    );

    function recordMetric(record: TradeRecord): number | null {
        if (metric === "records") return 1;
        if (metric === "value") return record.value;
        if (metric === "quantity") return record.unit === activeUnit ? record.quantity : null;
        return record.priceUnit === activeUnit ? record.pricePerUnit : null;
    }

    function dimensionValue(record: TradeRecord) {
        if (dimension === "year") return record.year?.toString() || "Unknown year";
        return record[dimension] || "Not recorded";
    }

    type ChartRow = {
        name: string;
        a: number;
        b: number;
        aObservations: number;
        bObservations: number;
    };

    const chartData = $derived.by(() => {
        const groups = new Map<
            string,
            { aTotal: number; bTotal: number; aCount: number; bCount: number }
        >();

        function addRecords(records: TradeRecord[], cohort: "a" | "b") {
            for (const record of records) {
                const amount = recordMetric(record);
                if (amount === null || !Number.isFinite(amount)) continue;
                const name = dimensionValue(record);
                const current = groups.get(name) ?? { aTotal: 0, bTotal: 0, aCount: 0, bCount: 0 };
                if (cohort === "a") {
                    current.aTotal += amount;
                    current.aCount += 1;
                } else {
                    current.bTotal += amount;
                    current.bCount += 1;
                }
                groups.set(name, current);
            }
        }

        addRecords(cohortA, "a");
        addRecords(cohortB, "b");

        const rows: ChartRow[] = [...groups].map(([name, item]) => ({
            name,
            a: metric === "pricePerUnit" && item.aCount ? item.aTotal / item.aCount : item.aTotal,
            b: metric === "pricePerUnit" && item.bCount ? item.bTotal / item.bCount : item.bTotal,
            aObservations: item.aCount,
            bObservations: item.bCount,
        }));

        rows.sort((a, b) =>
            dimension === "year"
                ? a.name.localeCompare(b.name, undefined, { numeric: true })
                : Math.max(b.a, b.b) - Math.max(a.a, a.b) || a.name.localeCompare(b.name),
        );
        return dimension === "year" ? rows : rows.slice(0, 16);
    });

    const totals = $derived({
        a: cohortA.reduce((sum, record) => sum + (recordMetric(record) ?? 0), 0),
        b: cohortB.reduce((sum, record) => sum + (recordMetric(record) ?? 0), 0),
    });
    const chartMax = $derived(Math.max(...chartData.flatMap((item) => [item.a, item.b]), 1));

    $effect(() => {
        if (metric === "pricePerUnit") scale = "absolute";
    });

    function barWidth(value: number, cohort: "a" | "b") {
        if (scale === "absolute" || metric === "pricePerUnit") return (value / chartMax) * 100;
        const total = totals[cohort];
        return total ? (value / total) * 100 : 0;
    }

    function summarize(records: TradeRecord[]) {
        let totalValue = 0;
        let valued = 0;
        let quantity = 0;
        let quantities = 0;
        let price = 0;
        let priced = 0;

        for (const record of records) {
            if (record.value !== null) {
                totalValue += record.value;
                valued += 1;
            }
            if (record.unit === activeUnit && record.quantity !== null) {
                quantity += record.quantity;
                quantities += 1;
            }
            if (record.priceUnit === activeUnit && record.pricePerUnit !== null) {
                price += record.pricePerUnit;
                priced += 1;
            }
        }

        return {
            records: records.length,
            totalValue,
            valued,
            quantity,
            quantities,
            averagePrice: priced ? price / priced : null,
            priced,
        };
    }

    const summaryA = $derived(summarize(cohortA));
    const summaryB = $derived(summarize(cohortB));
    const metricTitle = $derived(
        metric === "records"
            ? "record count"
            : metric === "value"
              ? "recorded value"
              : metric === "quantity"
                ? "quantity"
                : "average unit price",
    );

    function formatNumber(value: number, maximumFractionDigits = 1) {
        return new Intl.NumberFormat("en", {
            notation: Math.abs(value) > 999_999 ? "compact" : "standard",
            maximumFractionDigits,
        }).format(value);
    }

    function formatMetric(value: number) {
        if (metric === "records") return formatNumber(value, 0);
        if (metric === "value") return `${formatNumber(value)} ƒ`;
        if (metric === "quantity") return `${formatNumber(value)} ${activeUnit}`;
        return `${formatNumber(value, 2)} ƒ / ${activeUnit}`;
    }

    function swapCohorts() {
        const nextFieldA = fieldB;
        const nextValueA = valueB;
        fieldB = fieldA;
        valueB = valueA;
        fieldA = nextFieldA;
        valueA = nextValueA;
    }

    function resetComparison() {
        textile = [];
        yearFrom = firstYear;
        yearTo = lastYear;
        metric = "value";
        dimension = "year";
        scale = "absolute";
        unit = "";
        fieldA = "company";
        valueA = ["VOC"];
        fieldB = "company";
        valueB = ["WIC"];
    }

    function csvCell(value: string | number | null) {
        const string = value === null ? "" : String(value);
        return `"${string.replaceAll('"', '""')}"`;
    }

    function downloadComparison() {
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
            "pricePerUnit",
            "priceUnit",
            "color",
            "inferredColor",
            "pattern",
            "process",
            "fiber",
            "quality",
            "geography",
            "other",
        ];
        const selected = baseRecords.filter(
            (record) =>
                matchesCohort(record, fieldA, valueA) || matchesCohort(record, fieldB, valueB),
        );
        const rows = [
            ["cohort", ...columns].join(","),
            ...selected.map((record) => {
                const inA = matchesCohort(record, fieldA, valueA);
                const inB = matchesCohort(record, fieldB, valueB);
                const membership = inA && inB ? `${labelA} + ${labelB}` : inA ? labelA : labelB;
                return [
                    csvCell(membership),
                    ...columns.map((column) => csvCell(record[column])),
                ].join(",");
            }),
        ];
        const blob = new Blob(["\uFEFF", rows.join("\n")], { type: "text/csv;charset=utf-8" });
        downloadBlob(
            blob,
            exportFilename(
                "comparison",
                [textile, fieldA, valueA, fieldB, valueB, yearFrom, yearTo, metric],
                "csv",
            ),
        );
        downloadReady = true;
        window.setTimeout(() => (downloadReady = false), 1600);
    }

    function exportImage() {
        return downloadChart({
            title: "Textiles, Modifiers, and Values",
            context: [
                "Years: " + yearFrom + "–" + yearTo,
                "Textile: " + (textile.join(", ") || "All"),
                "Measure: " +
                    metricTitle +
                    (metric === "quantity" || metric === "pricePerUnit"
                        ? " · " + activeUnit
                        : metric === "value"
                          ? " · ƒ"
                          : ""),
                "Compare across: " +
                    dimension +
                    " · " +
                    (scale === "cohort" ? "Within-cohort share" : "Absolute scale"),
                "A · " + modifierLabel(fieldA) + ": " + labelA,
                "B · " + modifierLabel(fieldB) + ": " + labelB,
            ],
            series: [
                { label: labelA, color: "#171711" },
                { label: labelB, color: "#b9402b" },
            ],
            rows: chartData.map((row) => ({
                label: row.name,
                values: [row.a, row.b],
                display: [formatMetric(row.a), formatMetric(row.b)],
                widths: [barWidth(row.a, "a"), barWidth(row.b, "b")],
            })),
            filename: exportFilename(
                "comparison",
                [
                    textile,
                    fieldA,
                    valueA,
                    fieldB,
                    valueB,
                    yearFrom,
                    yearTo,
                    dimension,
                    metric,
                    scale,
                ],
                "png",
            ),
        });
    }
</script>

<svelte:head>
    <title>Textiles, Modifiers, and Values — Dutch Textile Trade</title>
    <meta
        name="description"
        content="Explore specific textiles in greater detail, like the quantities, total values, or per-piece values of imported or exported textiles over time or across geographies."
    />
</svelte:head>

<ResearchAppHeader active="textiles-modifiers-and-values" />

<section class="comparison-app">
    <div class="page-shell comparison-shell">
        <header class="builder-head">
            <button
                class="filter-toggle"
                type="button"
                aria-expanded={controlsOpen}
                onclick={() => (controlsOpen = !controlsOpen)}><Scale size={17} /> Filters</button
            >
            <button type="button" onclick={resetComparison}>
                <RotateCcw size={14} /> Reset
            </button>
        </header>

        {#if controlsOpen}
            <div class="base-controls">
                <div class="field">
                    <MultiSelect
                        label="Textile"
                        options={data.options.textiles}
                        bind:value={textile}
                    />
                </div>
                <div class="year-control">
                    <label>
                        <span>From year</span>
                        <input type="number" min={firstYear} max={yearTo} bind:value={yearFrom} />
                    </label>
                    <label>
                        <span>To year</span>
                        <input type="number" min={yearFrom} max={lastYear} bind:value={yearTo} />
                    </label>
                </div>
                <label>
                    <span>Compare across</span>
                    <select bind:value={dimension}>
                        <option value="year">Year</option>
                        <option value="origin">Origin region</option>
                        <option value="destination">Destination region</option>
                        <option value="textile">Textile name</option>
                    </select>
                </label>
                <label>
                    <span>Measure</span>
                    <select bind:value={metric}>
                        <option value="value">Recorded value</option>
                        <option value="records">Record count</option>
                        <option value="quantity">Quantity</option>
                        <option value="pricePerUnit">Average unit price</option>
                    </select>
                </label>
                {#if metric === "quantity" || metric === "pricePerUnit"}
                    <label>
                        <span>Unit</span>
                        <select bind:value={unit}>
                            {#each unitOptions as option}
                                <option value={option.name}
                                    >{option.name} · {option.count} rows</option
                                >
                            {/each}
                        </select>
                    </label>
                {/if}
            </div>
        {/if}
    </div>

    <div class="page-shell results-shell">
        <div class="cohort-builder">
            <article class="cohort-card cohort-a">
                <div class="cohort-title">
                    <div class="cohort-identity">
                        {#if companyA}
                            <CompanyMark company={companyA} showLabel={false} />
                        {:else}
                            <i aria-hidden="true"></i>
                        {/if}
                        <h3>{labelA}</h3>
                    </div>
                    <strong>{formatNumber(cohortA.length, 0)} rows</strong>
                </div>
                <div class="cohort-fields">
                    <label>
                        <span>Define by</span>
                        <select bind:value={fieldA}>
                            {#each modifierFields as option}
                                <option value={option.value}>{option.label}</option>
                            {/each}
                        </select>
                    </label>
                    <div class="field">
                        <MultiSelect
                            label="Matching values"
                            options={modifierOptions(fieldA)}
                            bind:value={valueA}
                        />
                    </div>
                </div>
            </article>

            <button class="swap" type="button" onclick={swapCohorts} aria-label="Swap cohorts">
                Swap cohorts
            </button>

            <article class="cohort-card cohort-b">
                <div class="cohort-title">
                    <div class="cohort-identity">
                        {#if companyB}
                            <CompanyMark company={companyB} showLabel={false} />
                        {:else}
                            <i aria-hidden="true"></i>
                        {/if}
                        <h3>{labelB}</h3>
                    </div>
                    <strong>{formatNumber(cohortB.length, 0)} rows</strong>
                </div>
                <div class="cohort-fields">
                    <label>
                        <span>Define by</span>
                        <select bind:value={fieldB}>
                            {#each modifierFields as option}
                                <option value={option.value}>{option.label}</option>
                            {/each}
                        </select>
                    </label>
                    <div class="field">
                        <MultiSelect
                            label="Matching values"
                            options={modifierOptions(fieldB)}
                            bind:value={valueB}
                        />
                    </div>
                </div>
            </article>
        </div>
        <div class="result-toolbar">
            <div>
                <h2>{metricTitle}</h2>
            </div>
            <div class="scale-control" aria-label="Chart scale">
                {#if metric !== "pricePerUnit"}
                    <button
                        class:active={scale === "cohort"}
                        type="button"
                        onclick={() => (scale = "cohort")}
                    >
                        Within-cohort share
                    </button>
                {/if}
                <button
                    class:active={scale === "absolute"}
                    type="button"
                    onclick={() => (scale = "absolute")}>Absolute scale</button
                >
            </div>
        </div>

        <div class="summary-cards">
            <article class="summary-a">
                <div class="summary-name">
                    {#if companyA}<CompanyMark company={companyA} showLabel={false} />{/if}
                    <span>{labelA}</span>
                </div>
                <strong>{formatNumber(summaryA.totalValue)} ƒ</strong>
                <p>
                    {formatNumber(summaryA.valued, 0)} valued rows
                </p>
                <small>
                    {formatNumber(summaryA.quantity)}
                    {activeUnit || "units"} ·
                    {summaryA.averagePrice === null
                        ? "price unavailable"
                        : `${formatNumber(summaryA.averagePrice, 2)} ƒ average`}
                </small>
            </article>
            <article class="summary-b">
                <div class="summary-name">
                    {#if companyB}<CompanyMark company={companyB} showLabel={false} />{/if}
                    <span>{labelB}</span>
                </div>
                <strong>{formatNumber(summaryB.totalValue)} ƒ</strong>
                <p>
                    {formatNumber(summaryB.valued, 0)} valued rows
                </p>
                <small>
                    {formatNumber(summaryB.quantity)}
                    {activeUnit || "units"} ·
                    {summaryB.averagePrice === null
                        ? "price unavailable"
                        : `${formatNumber(summaryB.averagePrice, 2)} ƒ average`}
                </small>
            </article>
        </div>

        <div class="chart-panel">
            <div class="export-toolbar">
                <ChartDownload action={exportImage} disabled={!chartData.length} />
            </div>
            <div class="chart-key">
                <span
                    ><i class="key-a"></i>{#if companyA}<CompanyMark
                            company={companyA}
                            showLabel={false}
                        />{/if}{labelA}</span
                >
                <span
                    ><i class="key-b"></i>{#if companyB}<CompanyMark
                            company={companyB}
                            showLabel={false}
                        />{/if}{labelB}</span
                >
            </div>

            {#if chartData.length}
                <div class="comparison-chart" role="img" aria-label="Comparison chart">
                    {#each chartData as item}
                        <div class="chart-row">
                            <div class="row-label" title={item.name}>{item.name}</div>
                            <div class="bar-stack">
                                <div class="bar-line">
                                    <span
                                        class="bar bar-a"
                                        style={`--bar-width: ${barWidth(item.a, "a")}%`}
                                        aria-label={`${labelA}: ${formatMetric(item.a)}`}
                                    ></span>
                                    <strong>{formatMetric(item.a)}</strong>
                                </div>
                                <div class="bar-line">
                                    <span
                                        class="bar bar-b"
                                        style={`--bar-width: ${barWidth(item.b, "b")}%`}
                                        aria-label={`${labelB}: ${formatMetric(item.b)}`}
                                    ></span>
                                    <strong>{formatMetric(item.b)}</strong>
                                </div>
                            </div>
                        </div>
                    {/each}
                </div>
            {:else}
                <div class="empty-chart">
                    <BarChart3 size={27} />
                    <p>No matching values.</p>
                </div>
            {/if}
        </div>

        <div class="table-head">
            <div>
                <Database size={18} />
                <h2>Exact chart values</h2>
            </div>
            <button type="button" onclick={downloadComparison}>
                {#if downloadReady}
                    <Check size={15} /> Downloaded
                {:else}
                    <ArrowDownToLine size={15} /> Download cohort rows
                {/if}
            </button>
        </div>
        <div class="result-table">
            <table>
                <thead>
                    <tr>
                        <th>{dimension}</th>
                        <th>{labelA}</th>
                        <th>{labelB}</th>
                        <th>{labelA} observations</th>
                        <th>{labelB} observations</th>
                    </tr>
                </thead>
                <tbody>
                    {#each chartData as item}
                        <tr>
                            <th>{item.name}</th>
                            <td>{formatMetric(item.a)}</td>
                            <td>{formatMetric(item.b)}</td>
                            <td>{formatNumber(item.aObservations, 0)}</td>
                            <td>{formatNumber(item.bObservations, 0)}</td>
                        </tr>
                    {/each}
                </tbody>
            </table>
        </div>
    </div>
</section>

<style>
    .export-toolbar {
        display: flex;
        justify-content: flex-end;
        padding-block: 1rem;
    }
    .comparison-app {
        font-variant-numeric: tabular-nums lining-nums;
        display: grid;
        grid-template-columns: 15rem minmax(0, 1fr);
        gap: 2.5rem;
        align-items: start;
        padding: 2rem var(--page-pad) 5rem;
        background: var(--paper);
        color: var(--ink);
        font-family: var(--sans);
    }
    .comparison-shell,
    .results-shell {
        width: 100%;
        max-width: none;
        margin: 0;
        padding: 0;
        min-width: 0;
    }
    .comparison-shell {
        position: sticky;
        top: 6rem;
        padding-right: 1.8rem;
        border-right: 1px solid var(--line-strong);
    }
    .builder-head,
    .table-head,
    .table-head > div {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.6rem;
    }
    .builder-head {
        padding-bottom: 1.1rem;
        border-bottom: 1px solid var(--line);
    }
    .builder-head button,
    .table-head button {
        display: inline-flex;
        align-items: center;
        gap: 0.4rem;
        min-height: 2rem;
        padding: 0;
        border: 0;
        background: none;
        color: inherit;
        font-size: 0.8125rem;
        cursor: pointer;
    }
    .builder-head .filter-toggle {
        font-size: 0.95rem;
        font-weight: 700;
    }
    .base-controls {
        display: grid;
        gap: 1.35rem;
        margin-top: 1.35rem;
    }
    label {
        min-width: 0;
    }
    label > span {
        display: block;
        margin-bottom: 0.5rem;
        font-size: 0.8125rem;
        color: var(--ink-soft);
        font-weight: 600;
    }
    select,
    input {
        width: 100%;
        min-height: 2.8rem;
        padding: 0.55rem 0.65rem;
        border: 1px solid var(--line-strong);
        border-radius: 0;
        color: inherit;
        background: transparent;
        font-size: 0.8125rem;
    }
    button:focus-visible,
    input:focus-visible,
    select:focus-visible {
        outline: 2px solid var(--accent-fill);
        outline-offset: 3px;
    }
    .year-control {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 0.65rem;
    }
    .cohort-builder {
        position: relative;
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 2.5rem;
        padding-bottom: 1.5rem;
        margin-bottom: 2.5rem;
        border-bottom: 1px solid var(--line-strong);
    }
    .cohort-card {
        min-width: 0;
        border-top: 3px solid var(--ink);
        padding-top: 1rem;
    }
    .cohort-b {
        border-color: var(--accent-fill);
    }
    .cohort-title {
        display: flex;
        justify-content: space-between;
        align-items: center;
        gap: 0.75rem;
    }
    .cohort-identity {
        display: flex;
        min-width: 0;
        align-items: center;
        gap: 0.5rem;
    }
    .cohort-identity > i {
        width: 0.65rem;
        height: 0.65rem;
        background: currentColor;
        border-radius: 50%;
    }
    .cohort-title h3 {
        min-width: 0;
        margin: 0;
        font: 600 1.15rem/1.2 var(--sans);
        overflow-wrap: anywhere;
        letter-spacing: var(--display-tracking, -0.025em);
    }
    .cohort-title strong {
        flex-shrink: 0;
        font-size: 0.8125rem;
        font-weight: 400;
        color: var(--ink-soft);
        font-variant-numeric: tabular-nums lining-nums;
    }
    .cohort-fields {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 0.65rem;
        margin-top: 1rem;
    }
    .swap {
        grid-column: 1 / -1;
        grid-row: 2;
        justify-self: end;
        min-height: 2rem;
        margin-top: -1.25rem;
        padding: 0;
        border: 0;
        background: none;
        color: var(--ink-soft);
        font-size: 0.8125rem;
        text-decoration: underline;
        text-underline-offset: 3px;
        cursor: pointer;
    }
    .swap:hover {
        color: var(--accent-fill);
    }
    .result-toolbar {
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        justify-content: space-between;
        gap: 1rem;
    }
    .result-toolbar h2 {
        margin: 0;
        font: 500 clamp(1.7rem, 3vw, 3rem)/1.1 var(--sans);
        letter-spacing: var(--display-tracking, -0.045em);
        text-transform: capitalize;
    }
    .scale-control {
        display: flex;
        border-bottom: 1px solid var(--line-strong);
    }
    .scale-control button {
        min-height: 2.8rem;
        padding: 0.6rem 0.8rem;
        border: 0;
        background: none;
        color: var(--ink-soft);
        font-size: 0.8125rem;
        cursor: pointer;
    }
    .scale-control button.active {
        color: var(--paper);
        background: var(--ink);
    }
    .summary-cards {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 2.5rem;
        margin: 2.5rem 0;
    }
    .summary-cards > article {
        min-width: 0;
    }
    .summary-name {
        display: flex;
        align-items: center;
        gap: 0.5rem;
        font-size: 0.8125rem;
    }
    .summary-name > span {
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
    }
    .summary-cards strong {
        display: block;
        margin: 0.5rem 0;
        font: 500 clamp(2rem, 4.3vw, 4.5rem)/1 var(--sans);
        letter-spacing: -0.035em;
        font-variant-numeric: tabular-nums lining-nums;
    }
    .summary-b strong {
        color: var(--accent-fill);
    }
    .summary-cards p,
    .summary-cards small {
        display: block;
        margin: 0.3rem 0 0;
        color: var(--ink-soft);
        font-size: 0.8125rem;
        line-height: 1.5;
    }
    .chart-panel {
        min-width: 0;
    }
    .chart-key {
        display: flex;
        gap: 1.5rem;
        align-items: center;
        flex-wrap: wrap;
        padding-bottom: 1rem;
        border-bottom: 1px solid var(--line-strong);
        font-size: 0.8125rem;
    }
    .chart-key span {
        display: flex;
        align-items: center;
        gap: 0.4rem;
    }
    .chart-key i {
        width: 1.2rem;
        height: 0.3rem;
    }
    .key-a,
    .bar-a {
        background: var(--ink);
    }
    .key-b,
    .bar-b {
        background: var(--accent-fill);
    }
    .chart-row {
        display: grid;
        grid-template-columns: minmax(5rem, 0.18fr) 1fr;
        gap: 1.5rem;
        align-items: center;
        min-height: 4.1rem;
        border-bottom: 1px solid var(--line);
    }
    .chart-row:hover {
        background: color-mix(in srgb, var(--ink) 2%, transparent);
    }
    .row-label {
        min-width: 0;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        font-size: 0.84rem;
        font-weight: 500;
    }
    .bar-stack {
        display: grid;
        gap: 0.35rem;
    }
    .bar-line {
        display: grid;
        grid-template-columns: minmax(0, 1fr) 7.5rem;
        align-items: center;
        gap: 1rem;
    }
    .bar {
        display: block;
        width: max(1px, var(--bar-width));
        height: 0.85rem;
        transition: width 240ms ease;
    }
    .bar-line strong {
        text-align: right;
        font-size: 0.8125rem;
        font-weight: 500;
        font-variant-numeric: tabular-nums lining-nums;
    }
    .empty-chart {
        display: grid;
        place-items: center;
        align-content: center;
        min-height: 25rem;
        color: var(--ink-soft);
        font-size: 0.9rem;
    }
    .table-head {
        margin-top: 3rem;
        padding-bottom: 1rem;
    }
    .table-head h2 {
        margin: 0;
        font: 600 1.1rem var(--sans);
        letter-spacing: var(--display-tracking, -0.025em);
    }
    .table-head button {
        min-height: 2.8rem;
        padding: 0.5rem 0.8rem;
        background: var(--accent-fill);
        color: white;
    }
    .result-table {
        overflow-x: auto;
        border-top: 1px solid var(--line-strong);
    }
    table {
        min-width: 42rem;
        width: 100%;
        border-collapse: collapse;
        font-size: 0.875rem;
        font-variant-numeric: tabular-nums lining-nums;
    }
    th,
    td {
        padding: 0.9rem 0.65rem;
        border-bottom: 1px solid var(--line);
        text-align: left;
    }
    thead th {
        color: var(--ink-soft);
        font-size: 0.8125rem;
        font-weight: 500;
    }
    tbody th {
        max-width: 15rem;
        overflow: hidden;
        text-overflow: ellipsis;
        white-space: nowrap;
        font-weight: 500;
    }
    @media (max-width: 1150px) {
        .comparison-app {
            grid-template-columns: 13rem minmax(0, 1fr);
            gap: 1.5rem;
        }
        .comparison-shell {
            padding-right: 1rem;
        }
        .cohort-builder {
            gap: 1.5rem;
        }
        .cohort-fields {
            grid-template-columns: 1fr;
        }
    }
    @media (max-width: 900px) {
        select,
        input {
            font-size: 1rem;
        }
        .comparison-app {
            display: block;
            padding-top: 1rem;
        }
        .comparison-shell {
            position: static;
            padding: 0 0 1.5rem;
            border-right: 0;
        }
        .base-controls {
            grid-template-columns: 1fr 1fr;
        }
        .cohort-fields {
            grid-template-columns: 1fr 1fr;
        }
        .cohort-builder {
            margin-top: 1rem;
        }
    }
    @media (max-width: 600px) {
        .cohort-builder,
        .summary-cards {
            gap: 1rem;
        }
        .cohort-title {
            align-items: start;
            flex-direction: column;
        }
        .cohort-fields {
            grid-template-columns: 1fr;
        }
        .cohort-title h3 {
            font-size: 1rem;
        }
        .summary-cards strong {
            font-size: clamp(1.7rem, 7vw, 2.7rem);
        }
        .chart-row {
            grid-template-columns: 1fr;
            gap: 0.45rem;
            padding: 0.8rem 0;
        }
        .bar-line {
            grid-template-columns: minmax(0, 1fr) 6.5rem;
            gap: 0.5rem;
        }
        .result-toolbar h2 {
            font-size: 2rem;
        }
        .table-head {
            align-items: start;
            gap: 1rem;
            flex-direction: column;
        }
    }
</style>
