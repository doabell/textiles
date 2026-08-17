<script lang="ts">
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
        { value: "pattern", label: "Pattern" },
        { value: "process", label: "Process" },
        { value: "fiber", label: "Fiber" },
        { value: "quality", label: "Quality" },
        { value: "origin", label: "Origin region" },
        { value: "destination", label: "Destination region" },
    ];

    let textile = $state("");
    let yearFrom = $state(firstYear);
    let yearTo = $state(lastYear);
    let metric = $state<Metric>("value");
    let dimension = $state<Dimension>("year");
    let scale = $state<"cohort" | "absolute">("cohort");
    let unit = $state("");
    let fieldA = $state<ModifierField>("company");
    let valueA = $state("VOC");
    let fieldB = $state<ModifierField>("company");
    let valueB = $state("WIC");
    let downloadReady = $state(false);

    onMount(() => {
        const requestedTextile = new URL(window.location.href).searchParams.get("textile");
        if (requestedTextile) textile = requestedTextile;
    });

    const normalize = (value: string) => value.toLocaleLowerCase("en").trim();
    const baseRecords = $derived.by(() =>
        data.records.filter(
            (record) =>
                (!textile || normalize(record.textile).includes(normalize(textile))) &&
                (record.year === null || (record.year >= yearFrom && record.year <= yearTo)),
        ),
    );

    const unitOptions = $derived.by(() => {
        const counts = new Map<string, number>();
        for (const record of baseRecords) {
            if (record.unit && record.quantity !== null) {
                counts.set(record.unit, (counts.get(record.unit) ?? 0) + 1);
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
        if (valueA && !optionsA.includes(valueA)) valueA = "";
        if (valueB && !optionsB.includes(valueB)) valueB = "";
        if (unit && !unitOptions.some((option) => option.name === unit)) unit = "";
    });

    function matchesCohort(record: TradeRecord, field: ModifierField, value: string) {
        return !value || modifierValue(record, field) === value;
    }

    const cohortA = $derived(baseRecords.filter((record) => matchesCohort(record, fieldA, valueA)));
    const cohortB = $derived(baseRecords.filter((record) => matchesCohort(record, fieldB, valueB)));
    const labelA = $derived(valueA || `All ${modifierLabel(fieldA).toLocaleLowerCase("en")}`);
    const labelB = $derived(valueB || `All ${modifierLabel(fieldB).toLocaleLowerCase("en")}`);
    const companyA = $derived(
        fieldA === "company" && (valueA === "VOC" || valueA === "WIC") ? valueA : null,
    );
    const companyB = $derived(
        fieldB === "company" && (valueB === "VOC" || valueB === "WIC") ? valueB : null,
    );

    function recordMetric(record: TradeRecord): number | null {
        if (metric === "records") return 1;
        if (metric === "value") return record.value;
        if (record.unit !== activeUnit) return null;
        if (metric === "quantity") return record.quantity;
        return record.pricePerUnit;
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
        a: chartData.reduce((sum, item) => sum + item.a, 0),
        b: chartData.reduce((sum, item) => sum + item.b, 0),
    });
    const chartMax = $derived(Math.max(...chartData.flatMap((item) => [item.a, item.b]), 1));

    function barWidth(value: number, cohort: "a" | "b") {
        if (scale === "absolute") return (value / chartMax) * 100;
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
            if (record.unit === activeUnit && record.pricePerUnit !== null) {
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
                ? `quantity in ${activeUnit || "one unit"}`
                : `average price per ${activeUnit || "unit"}`,
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
        textile = "";
        yearFrom = firstYear;
        yearTo = lastYear;
        metric = "value";
        dimension = "year";
        scale = "cohort";
        unit = "";
        fieldA = "company";
        valueA = "VOC";
        fieldB = "company";
        valueB = "WIC";
    }

    function csvCell(value: string | number | null) {
        const string = value === null ? "" : String(value);
        return `"${string.replaceAll('"', '""')}"`;
    }

    function downloadComparison() {
        const columns: (keyof TradeRecord)[] = [
            "company",
            "year",
            "origin",
            "destination",
            "textile",
            "quantity",
            "unit",
            "value",
            "pricePerUnit",
            "color",
            "pattern",
            "process",
            "fiber",
            "quality",
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
        const blob = new Blob([rows.join("\n")], { type: "text/csv;charset=utf-8" });
        const url = URL.createObjectURL(blob);
        const anchor = document.createElement("a");
        anchor.href = url;
        anchor.download = "dutch-textile-trade-comparison.csv";
        anchor.click();
        URL.revokeObjectURL(url);
        downloadReady = true;
        window.setTimeout(() => (downloadReady = false), 1600);
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
            <div>
                <Scale size={19} />
                <h2>Filters</h2>
            </div>
            <button type="button" onclick={resetComparison}>
                <RotateCcw size={14} /> Reset
            </button>
        </header>

        <div class="base-controls">
            <label>
                <span>Textile</span>
                <select bind:value={textile}>
                    <option value="">All textile names</option>
                    {#each data.options.textiles as option}
                        <option value={option}>{option}</option>
                    {/each}
                </select>
            </label>
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
                    <option value="pricePerUnit">Average price per unit</option>
                </select>
            </label>
            {#if metric === "quantity" || metric === "pricePerUnit"}
                <label>
                    <span>Unit</span>
                    <select bind:value={unit}>
                        {#each unitOptions as option}
                            <option value={option.name}>{option.name} · {option.count} rows</option>
                        {/each}
                    </select>
                </label>
            {/if}
        </div>

        <div class="cohort-builder">
            <article class="cohort-card cohort-a">
                <div class="cohort-title">
                    <div class="cohort-identity">
                        {#if companyA}
                            <CompanyMark company={companyA} showLabel={false} inverted />
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
                    <label>
                        <span>Matching value</span>
                        <select bind:value={valueA}>
                            <option value="">All available values</option>
                            {#each modifierOptions(fieldA) as option}
                                <option value={option}>{option}</option>
                            {/each}
                        </select>
                    </label>
                </div>
            </article>

            <button
                class="swap"
                type="button"
                onclick={swapCohorts}
                aria-label="Swap the two cohorts"
            >
                Swap cohorts
            </button>

            <article class="cohort-card cohort-b">
                <div class="cohort-title">
                    <div class="cohort-identity">
                        {#if companyB}
                            <CompanyMark company={companyB} showLabel={false} inverted />
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
                    <label>
                        <span>Matching value</span>
                        <select bind:value={valueB}>
                            <option value="">All available values</option>
                            {#each modifierOptions(fieldB) as option}
                                <option value={option}>{option}</option>
                            {/each}
                        </select>
                    </label>
                </div>
            </article>
        </div>
    </div>

    <div class="page-shell results-shell">
        <div class="result-toolbar">
            <div>
                <p class="eyebrow">Comparison result</p>
                <h2>{metricTitle} by {dimension}</h2>
            </div>
            <div class="scale-control" aria-label="Chart scale">
                <button
                    class:active={scale === "cohort"}
                    type="button"
                    onclick={() => (scale = "cohort")}
                >
                    Within-cohort share
                </button>
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
                    {formatNumber(summaryA.valued, 0)} valued rows of {formatNumber(
                        summaryA.records,
                        0,
                    )}
                </p>
                <small>
                    {formatNumber(summaryA.quantity)}
                    {activeUnit || "units"} ·
                    {summaryA.averagePrice === null
                        ? "no comparable unit price"
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
                    {formatNumber(summaryB.valued, 0)} valued rows of {formatNumber(
                        summaryB.records,
                        0,
                    )}
                </p>
                <small>
                    {formatNumber(summaryB.quantity)}
                    {activeUnit || "units"} ·
                    {summaryB.averagePrice === null
                        ? "no comparable unit price"
                        : `${formatNumber(summaryB.averagePrice, 2)} ƒ average`}
                </small>
            </article>
        </div>

        <div class="chart-panel">
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
                <div
                    class="comparison-chart"
                    role="img"
                    aria-label={`${metricTitle} comparison chart`}
                >
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
                    <p>No values. Change measure or broaden either cohort.</p>
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
    .comparison-app {
        padding: 1.25rem 0 clamp(6rem, 9vw, 9rem);
        color: var(--paper);
        background: var(--ink);
    }

    .comparison-shell {
        padding-top: 1.2rem;
        padding-bottom: 1.2rem;
        background: #22221d;
        border: 1px solid rgba(244, 239, 229, 0.14);
    }

    .builder-head,
    .builder-head > div,
    .table-head,
    .table-head > div {
        display: flex;
        gap: 0.55rem;
        align-items: center;
        justify-content: space-between;
    }

    .builder-head > div,
    .table-head > div {
        justify-content: flex-start;
    }

    .builder-head h2,
    .table-head h2 {
        margin: 0;
        font-family: var(--sans);
        font-size: 0.96rem;
        font-weight: 650;
        letter-spacing: -0.015em;
    }

    .builder-head button,
    .table-head button {
        display: inline-flex;
        gap: 0.35rem;
        align-items: center;
        color: rgba(244, 239, 229, 0.66);
        background: none;
        border: 0;
        font-size: 0.67rem;
        cursor: pointer;
    }

    .base-controls {
        display: grid;
        grid-template-columns:
            minmax(0, 1.3fr) minmax(0, 1fr) minmax(0, 1fr) minmax(0, 1fr)
            minmax(0, 0.8fr);
        gap: 0.75rem;
        margin-top: 1.2rem;
        padding-top: 1.2rem;
        border-top: 1px solid rgba(244, 239, 229, 0.14);
    }

    label > span,
    .cohort-fields label > span {
        display: block;
        margin-bottom: 0.4rem;
        color: rgba(244, 239, 229, 0.5);
        font-family: var(--sans);
        font-size: 0.7rem;
        font-weight: 560;
        letter-spacing: 0;
    }

    .base-controls label,
    .cohort-fields label {
        min-width: 0;
    }

    select,
    input {
        width: 100%;
        min-height: 2.85rem;
        padding: 0.65rem 0.78rem;
        color: var(--paper);
        background: #171713;
        border: 1px solid rgba(244, 239, 229, 0.22);
        border-radius: 0.55rem;
        font-size: 0.8rem;
        box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.035);
    }

    select {
        padding-right: 2.2rem;
        appearance: none;
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
    }

    .year-control {
        display: grid;
        grid-template-columns: 1fr 1fr;
        gap: 0.5rem;
    }

    .cohort-builder {
        display: grid;
        grid-template-columns: 1fr auto 1fr;
        gap: 1rem;
        align-items: stretch;
        margin-top: 1.2rem;
    }

    .cohort-card {
        min-width: 0;
        padding: 1.15rem;
        border: 1px solid rgba(244, 239, 229, 0.2);
        border-radius: 0.65rem;
    }

    .cohort-a {
        background: rgba(31, 70, 84, 0.3);
    }

    .cohort-b {
        background: rgba(166, 65, 47, 0.23);
    }

    .cohort-title {
        display: grid;
        grid-template-columns: minmax(0, 1fr) auto;
        gap: 1rem;
        align-items: center;
    }

    .cohort-identity,
    .cohort-title h3 {
        display: flex;
        min-width: 0;
        gap: 0.55rem;
        align-items: center;
        margin: 0;
    }

    .cohort-identity > i {
        flex: 0 0 auto;
        width: 0.75rem;
        height: 0.75rem;
        border-radius: 999px;
        background: var(--saffron);
    }

    .cohort-title h3 {
        font-family: var(--sans);
        font-size: 1.08rem;
        font-weight: 650;
        line-height: 1.2;
        overflow-wrap: anywhere;
    }

    .cohort-title strong {
        color: rgba(244, 239, 229, 0.58);
        font-family: var(--sans);
        font-size: 0.72rem;
        font-variant-numeric: tabular-nums;
        white-space: nowrap;
    }

    .cohort-fields {
        display: grid;
        grid-template-columns: minmax(0, 0.8fr) minmax(0, 1.2fr);
        gap: 0.65rem;
        margin-top: 1rem;
    }

    .swap {
        align-self: center;
        min-height: 2.85rem;
        padding: 0.65rem 0.85rem;
        color: var(--paper);
        background: #171713;
        border: 1px solid rgba(244, 239, 229, 0.3);
        border-radius: 0.55rem;
        font-size: 0.74rem;
        font-weight: 650;
        white-space: nowrap;
        cursor: pointer;
    }

    .swap:hover {
        color: var(--ink);
        background: var(--saffron);
    }

    .results-shell {
        margin-top: 1.25rem;
        padding-top: 2rem;
        padding-bottom: 2rem;
        color: var(--ink);
        background: var(--paper);
    }

    .result-toolbar {
        display: flex;
        gap: 2rem;
        align-items: end;
        justify-content: space-between;
    }

    .result-toolbar h2 {
        margin: 0;
        font-family: var(--serif);
        font-size: clamp(2rem, 4vw, 3.5rem);
        font-weight: 400;
        text-transform: capitalize;
    }

    .scale-control {
        display: flex;
    }

    .scale-control button {
        min-height: 2.65rem;
        padding: 0.5rem 0.8rem;
        color: var(--ink-soft);
        background: transparent;
        border: 1px solid var(--line-strong);
        font-size: 0.72rem;
        cursor: pointer;
    }

    .scale-control button + button {
        border-left: 0;
    }

    .scale-control button.active {
        color: var(--paper);
        background: var(--ink);
    }

    .summary-cards {
        display: grid;
        grid-template-columns: 1fr 1fr;
        margin-top: 2rem;
        border: 1px solid var(--line-strong);
    }

    .summary-cards > * {
        min-width: 0;
        padding: 1rem;
        border-left: 1px solid var(--line-strong);
    }

    .summary-cards > *:first-child {
        border-left: 0;
    }

    .summary-name {
        display: flex;
        min-width: 0;
        gap: 0.5rem;
        align-items: center;
    }

    .summary-name > span {
        display: block;
        overflow: hidden;
        color: var(--ink-soft);
        font-family: var(--sans);
        font-size: 0.74rem;
        font-weight: 650;
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    .summary-cards strong {
        display: block;
        margin: 0.5rem 0;
        font-family: var(--serif);
        font-size: clamp(1.8rem, 3vw, 2.8rem);
        font-weight: 400;
    }

    .summary-cards p,
    .summary-cards small {
        display: block;
        margin: 0;
        color: var(--ink-soft);
        font-size: 0.75rem;
        line-height: 1.5;
    }

    .summary-a {
        box-shadow: inset 0 4px var(--indigo);
    }

    .summary-b {
        box-shadow: inset 0 4px var(--madder);
    }

    .chart-panel {
        margin-top: 1.5rem;
        padding: 1.2rem;
        border: 1px solid var(--line-strong);
    }

    .chart-key {
        display: flex;
        flex-wrap: wrap;
        gap: 0.75rem 1.2rem;
        align-items: center;
        padding-bottom: 1rem;
        border-bottom: 1px solid var(--line);
        font-family: var(--sans);
        font-size: 0.72rem;
    }

    .chart-key span {
        display: flex;
        gap: 0.35rem;
        align-items: center;
    }

    .chart-key i {
        width: 1.2rem;
        height: 0.32rem;
    }

    .key-a {
        background: var(--indigo);
    }

    .key-b {
        background: var(--madder);
    }

    .comparison-chart {
        margin-top: 1rem;
    }

    .chart-row {
        display: grid;
        grid-template-columns: minmax(8rem, 0.25fr) 1fr;
        gap: 1rem;
        align-items: center;
        min-height: 3.7rem;
        border-top: 1px solid var(--line);
    }

    .chart-row:first-child {
        border-top: 0;
    }

    .row-label {
        overflow: hidden;
        font-family: var(--sans);
        font-size: 0.82rem;
        font-weight: 620;
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    .bar-stack {
        display: grid;
        gap: 0.3rem;
    }

    .bar-line {
        display: grid;
        grid-template-columns: minmax(0, 1fr) 7.5rem;
        gap: 0.6rem;
        align-items: center;
    }

    .bar {
        display: block;
        width: max(1px, var(--bar-width));
        height: 0.62rem;
        transition: width 260ms ease;
    }

    .bar-a {
        background: var(--indigo);
    }

    .bar-b {
        background: var(--madder);
    }

    .bar-line strong {
        font-family: var(--sans);
        font-size: 0.7rem;
        font-weight: 400;
        text-align: right;
    }

    .empty-chart {
        padding: 5rem 1rem;
        color: var(--ink-soft);
        text-align: center;
    }

    .empty-chart :global(svg) {
        margin-bottom: 1rem;
    }

    .table-head {
        margin-top: 2rem;
        padding-bottom: 0.8rem;
    }

    .table-head button {
        color: var(--indigo);
        font-weight: 700;
    }

    .result-table {
        overflow-x: auto;
        border: 1px solid var(--line-strong);
    }

    table {
        width: 100%;
        min-width: 48rem;
        border-collapse: collapse;
        font-size: 0.7rem;
    }

    th,
    td {
        padding: 0.7rem 0.8rem;
        text-align: left;
        border-bottom: 1px solid var(--line);
    }

    thead th {
        color: var(--ink-soft);
        background: var(--paper-deep);
        font-family: var(--sans);
        font-size: 0.7rem;
        letter-spacing: 0;
    }

    tbody th {
        max-width: 18rem;
        overflow: hidden;
        font-family: var(--sans);
        font-weight: 620;
        text-overflow: ellipsis;
        white-space: nowrap;
    }

    tbody tr:last-child th,
    tbody tr:last-child td {
        border-bottom: 0;
    }

    @media (max-width: 980px) {
        .base-controls {
            grid-template-columns: repeat(2, 1fr);
        }

        .summary-cards {
            grid-template-columns: 1fr 1fr;
        }
    }

    @media (max-width: 780px) {
        .cohort-builder {
            grid-template-columns: 1fr;
        }

        .swap {
            justify-self: center;
        }

        .result-toolbar {
            display: block;
        }

        .scale-control {
            margin-top: 1rem;
        }

        .chart-row {
            grid-template-columns: 1fr;
            gap: 0.25rem;
            padding: 0.6rem 0;
        }
    }

    @media (max-width: 580px) {
        .base-controls,
        .cohort-fields,
        .summary-cards {
            grid-template-columns: 1fr;
        }

        .summary-cards > * {
            border-top: 1px solid var(--line-strong);
            border-left: 0;
        }

        .summary-cards > *:first-child {
            border-top: 0;
        }

        .bar-line {
            grid-template-columns: minmax(0, 1fr) 6rem;
        }
    }
</style>
