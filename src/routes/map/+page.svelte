<script lang="ts">
    import {
        ArrowDownToLine,
        ArrowRight,
        Compass,
        MapPin,
        RotateCcw,
        Route,
        Search,
    } from "@lucide/svelte";
    import { geoInterpolate } from "d3-geo";
    import type {
        Canvas as LeafletCanvas,
        LayerGroup as LeafletLayerGroup,
        Map as LeafletMap,
        Polyline as LeafletPolyline,
    } from "leaflet";
    import "leaflet/dist/leaflet.css";
    import { onMount } from "svelte";
    import CompanyMark from "$lib/components/CompanyMark.svelte";
    import ResearchAppHeader from "$lib/components/ResearchAppHeader.svelte";
    import type { TradeRecord } from "$lib/server/trade";
    import type { PageData } from "./$types";

    let { data }: { data: PageData } = $props();

    // The page is prerendered and receives one immutable snapshot of the source dataset.
    // svelte-ignore state_referenced_locally
    const initialData = $state.snapshot(data);
    const firstYear = initialData.options.years[0] ?? 1700;
    const lastYear = initialData.options.years.at(-1) ?? 1724;
    const places = [
        ...new Set([...initialData.options.origins, ...initialData.options.destinations]),
    ].sort((a, b) => a.localeCompare(b));

    let company = $state("");
    let textile = $state("");
    let modifier = $state("");
    let place = $state("");
    let yearFrom = $state(firstYear);
    let yearTo = $state(lastYear);
    let metric = $state<"records" | "value">("records");
    let selectedRouteKey = $state("");
    let mapHost = $state<HTMLDivElement>();
    let mapContainer = $state<HTMLDivElement>();
    let mapWidth = $state(1000);
    let mapStatus = $state<"loading" | "ready" | "error">("loading");
    let interactiveMap: LeafletMap | null = null;
    let leaflet: typeof import("leaflet") | null = null;
    let routeLayer: LeafletLayerGroup | null = null;
    let pointLayer: LeafletLayerGroup | null = null;
    let routeRenderer: LeafletCanvas | null = null;
    const routeLines = new Map<string, LeafletPolyline>();
    const routeStyles = new Map<string, { color: string; weight: number }>();
    let destroyed = false;

    const mapHeight = $derived(Math.max(540, Math.min(820, mapWidth * 0.68)));
    const normalize = (value: string) => value.toLocaleLowerCase("en").trim();
    const modifiers = [
        ...new Set(
            initialData.records.flatMap((record) => [
                record.color,
                record.pattern,
                record.process,
                record.fiber,
                record.quality,
            ]),
        ),
    ]
        .filter(Boolean)
        .sort((a, b) => a.localeCompare(b));

    const filtered = $derived.by(() =>
        data.records.filter(
            (record) =>
                (!company || record.company === company) &&
                (!textile || normalize(record.textile).includes(normalize(textile))) &&
                (!modifier ||
                    [
                        record.color,
                        record.pattern,
                        record.process,
                        record.fiber,
                        record.quality,
                    ].some((value) => normalize(value).includes(normalize(modifier)))) &&
                (!place || record.origin === place || record.destination === place) &&
                (record.year === null || (record.year >= yearFrom && record.year <= yearTo)),
        ),
    );

    type RouteSummary = {
        key: string;
        origin: string;
        destination: string;
        originPort: string;
        destinationPort: string;
        originLat: number;
        originLong: number;
        destinationLat: number;
        destinationLong: number;
        records: number;
        value: number;
        textiles: Set<string>;
        companies: Set<string>;
    };

    const routes = $derived.by(() => {
        const grouped = new Map<string, RouteSummary>();

        for (const record of filtered) {
            if (
                record.originLat === null ||
                record.originLong === null ||
                record.destinationLat === null ||
                record.destinationLong === null
            ) {
                continue;
            }

            const key = [
                record.origin,
                record.destination,
                record.originLat.toFixed(4),
                record.originLong.toFixed(4),
                record.destinationLat.toFixed(4),
                record.destinationLong.toFixed(4),
            ].join("|");
            const current = grouped.get(key) ?? {
                key,
                origin: record.origin || record.originPort || "Unrecorded origin",
                destination:
                    record.destination || record.destinationPort || "Unrecorded destination",
                originPort: record.originPort,
                destinationPort: record.destinationPort,
                originLat: record.originLat,
                originLong: record.originLong,
                destinationLat: record.destinationLat,
                destinationLong: record.destinationLong,
                records: 0,
                value: 0,
                textiles: new Set<string>(),
                companies: new Set<string>(),
            };

            current.records += 1;
            current.value += record.value ?? 0;
            current.textiles.add(record.textile);
            current.companies.add(record.company);
            grouped.set(key, current);
        }

        return [...grouped.values()].sort((a, b) => b[metric] - a[metric]);
    });

    const mappedRecords = $derived(routes.reduce((sum, item) => sum + item.records, 0));
    const routeMax = $derived(Math.max(...routes.map((route) => route[metric]), 1));
    const selectedRoute = $derived(routes.find((route) => route.key === selectedRouteKey) ?? null);
    const topRoutes = $derived(routes.slice(0, 10));
    const destinationCount = $derived(
        new Set(filtered.map((record) => record.destination).filter(Boolean)).size,
    );
    const originCount = $derived(
        new Set(filtered.map((record) => record.origin).filter(Boolean)).size,
    );
    const textileCount = $derived(new Set(filtered.map((record) => record.textile)).size);

    onMount(() => {
        textile = new URL(window.location.href).searchParams.get("textile") ?? "";
        if (!mapHost) return;
        const observer = new ResizeObserver(([entry]) => {
            mapWidth = Math.max(320, Math.floor(entry.contentRect.width));
            interactiveMap?.invalidateSize({ pan: false });
        });
        observer.observe(mapHost);

        destroyed = false;
        void initializeMap();

        return () => {
            destroyed = true;
            observer.disconnect();
            interactiveMap?.remove();
            interactiveMap = null;
            routeLayer = null;
            pointLayer = null;
            routeRenderer = null;
            routeLines.clear();
            routeStyles.clear();
        };
    });

    $effect(() => {
        updateInteractiveRoutes(routes, routeMax, metric);
    });

    $effect(() => {
        updateSelectedRoute(selectedRouteKey);
    });

    $effect(() => {
        if (selectedRouteKey && !routes.some((route) => route.key === selectedRouteKey)) {
            selectedRouteKey = "";
        }
    });

    async function initializeMap() {
        if (!mapContainer || destroyed) return;

        interactiveMap?.remove();
        interactiveMap = null;
        routeLayer = null;
        pointLayer = null;
        routeRenderer = null;
        routeLines.clear();
        routeStyles.clear();
        mapStatus = "loading";

        try {
            const L = await import("leaflet");
            if (destroyed || !mapContainer) return;

            leaflet = L;
            const renderer = L.canvas({ padding: 0.35, tolerance: 8 });
            const map = L.map(mapContainer, {
                center: [14, 38],
                zoom: 2,
                minZoom: 1,
                maxZoom: 5,
                maxBounds: [
                    [-85, -180],
                    [85, 180],
                ],
                maxBoundsViscosity: 1,
                preferCanvas: true,
                renderer,
                worldCopyJump: false,
            });
            interactiveMap = map;
            routeRenderer = renderer;

            L.tileLayer("https://tiles.openfreemap.org/natural_earth/ne2sr/{z}/{x}/{y}.png", {
                minZoom: 1,
                maxZoom: 5,
                maxNativeZoom: 5,
                noWrap: true,
                detectRetina: false,
                attribution:
                    '<a href="https://openfreemap.org/">OpenFreeMap</a> · <a href="https://www.naturalearthdata.com/">Natural Earth</a>',
            }).addTo(map);

            routeLayer = L.layerGroup().addTo(map);
            pointLayer = L.layerGroup().addTo(map);
            mapStatus = "ready";
            updateInteractiveRoutes(routes, routeMax, metric);
            updateSelectedRoute(selectedRouteKey);
        } catch {
            mapStatus = "error";
        }
    }

    function updateInteractiveRoutes(
        mapRoutes: RouteSummary[],
        maxValue: number,
        activeMetric: "records" | "value",
    ) {
        if (!leaflet || !routeLayer || !pointLayer || !routeRenderer) return;

        routeLayer.clearLayers();
        pointLayer.clearLayers();
        routeLines.clear();
        routeStyles.clear();
        const points = new Map<
            string,
            { coordinates: [number, number]; kind: "origin" | "destination" }
        >();

        for (const routeItem of mapRoutes) {
            const interpolate = geoInterpolate(
                [routeItem.originLong, routeItem.originLat],
                [routeItem.destinationLong, routeItem.destinationLat],
            );
            const coordinates = Array.from({ length: 17 }, (_, index) => {
                const [longitude, latitude] = interpolate(index / 16);
                return [latitude, longitude] as [number, number];
            });
            const companyName =
                routeItem.companies.size > 1
                    ? "Mixed"
                    : routeItem.companies.has("WIC")
                      ? "WIC"
                      : "VOC";
            const color =
                companyName === "WIC" ? "#a6412f" : companyName === "Mixed" ? "#c6953f" : "#1f4654";
            const weight = 1 + Math.sqrt(routeItem[activeMetric] / Math.max(maxValue, 1)) * 6;
            const line = leaflet.polyline(coordinates, {
                color,
                opacity: 0.76,
                weight,
                lineCap: "round",
                lineJoin: "round",
                renderer: routeRenderer,
                interactive: true,
            });
            line.on("click", () => selectRoute(routeItem));
            line.addTo(routeLayer);
            routeLines.set(routeItem.key, line);
            routeStyles.set(routeItem.key, { color, weight });

            points.set(`${routeItem.originLat}|${routeItem.originLong}`, {
                coordinates: [routeItem.originLat, routeItem.originLong],
                kind: "origin",
            });
            points.set(`${routeItem.destinationLat}|${routeItem.destinationLong}`, {
                coordinates: [routeItem.destinationLat, routeItem.destinationLong],
                kind: "destination",
            });
        }

        for (const point of points.values()) {
            leaflet
                .circleMarker(point.coordinates, {
                    radius: 4,
                    color: "#fbf8f1",
                    weight: 1.2,
                    fillColor: point.kind === "origin" ? "#f3c96b" : "#c65f4a",
                    fillOpacity: 1,
                    renderer: routeRenderer,
                    interactive: false,
                })
                .addTo(pointLayer);
        }

        updateSelectedRoute(selectedRouteKey);
    }

    function updateSelectedRoute(activeKey: string) {
        for (const [key, line] of routeLines) {
            const baseStyle = routeStyles.get(key);
            if (key === activeKey) {
                line.setStyle({ color: "#f3c96b", opacity: 1, weight: 5 });
                line.bringToFront();
            } else if (baseStyle) {
                line.setStyle({ ...baseStyle, opacity: 0.76 });
            }
        }
    }

    function selectRoute(routeItem: RouteSummary) {
        selectedRouteKey = routeItem.key;
        if (!interactiveMap || mapStatus !== "ready") return;
        interactiveMap.fitBounds(
            [
                [routeItem.originLat, routeItem.originLong],
                [routeItem.destinationLat, routeItem.destinationLong],
            ],
            { padding: [72, 72], animate: true, duration: 0.75, maxZoom: 4 },
        );
    }

    function resetFilters() {
        company = "";
        textile = "";
        modifier = "";
        place = "";
        yearFrom = firstYear;
        yearTo = lastYear;
        selectedRouteKey = "";
    }

    function formatNumber(value: number) {
        return new Intl.NumberFormat("en", {
            notation: value > 999_999 ? "compact" : "standard",
            maximumFractionDigits: value > 999 ? 1 : 0,
        }).format(value);
    }

    function csvCell(value: string | number | null) {
        const string = value === null ? "" : String(value);
        return `"${string.replaceAll('"', '""')}"`;
    }

    function downloadMappedRows() {
        const columns: (keyof TradeRecord)[] = [
            "company",
            "year",
            "origin",
            "originPort",
            "destination",
            "destinationPort",
            "textile",
            "quantity",
            "unit",
            "value",
        ];
        const rows = [
            columns.join(","),
            ...filtered.map((record) => columns.map((column) => csvCell(record[column])).join(",")),
        ];
        const blob = new Blob([rows.join("\n")], { type: "text/csv;charset=utf-8" });
        const url = URL.createObjectURL(blob);
        const anchor = document.createElement("a");
        anchor.href = url;
        anchor.download = "dutch-textile-trade-map-selection.csv";
        anchor.click();
        URL.revokeObjectURL(url);
    }
</script>

<svelte:head>
    <title>Textile Geographies — Dutch Textile Trade</title>
    <meta
        name="description"
        content="Search the textile data set by a range of archival modifiers—including color, pattern, process, fiber, or quality and visualize this geographically and infographically."
    />
</svelte:head>

<ResearchAppHeader active="textile-geographies" />

<section class="map-app">
    <div class="page-shell map-layout">
        <aside class="map-controls">
            <div class="control-heading">
                <div>
                    <Compass size={18} />
                    <h2>Filters</h2>
                </div>
                <button type="button" class="reset" onclick={resetFilters}>
                    <RotateCcw size={14} /> Reset
                </button>
            </div>

            <fieldset>
                <legend>Company network</legend>
                <div class="segmented">
                    <button class:active={!company} type="button" onclick={() => (company = "")}
                        >Both</button
                    >
                    <button
                        class:active={company === "VOC"}
                        type="button"
                        onclick={() => (company = "VOC")}
                    >
                        <CompanyMark company="VOC" inverted={company !== "VOC"} />
                    </button>
                    <button
                        class:active={company === "WIC"}
                        type="button"
                        onclick={() => (company = "WIC")}
                    >
                        <CompanyMark company="WIC" inverted={company !== "WIC"} />
                    </button>
                </div>
            </fieldset>

            <label class="field">
                <span>Textile name</span>
                <div class="input-with-icon">
                    <Search size={14} />
                    <input bind:value={textile} list="map-textiles" placeholder="All textiles" />
                </div>
                <datalist id="map-textiles">
                    {#each data.options.textiles as option}
                        <option value={option}></option>
                    {/each}
                </datalist>
            </label>

            <label class="field">
                <span>Archival modifier</span>
                <div class="input-with-icon">
                    <Search size={14} />
                    <input
                        bind:value={modifier}
                        list="map-modifiers"
                        placeholder="Color, pattern, process, fiber, or quality"
                    />
                </div>
                <datalist id="map-modifiers">
                    {#each modifiers as option}
                        <option value={option}></option>
                    {/each}
                </datalist>
            </label>

            <label class="field">
                <span>Origin or destination</span>
                <select bind:value={place}>
                    <option value="">All recorded regions</option>
                    {#each places as option}
                        <option value={option}>{option}</option>
                    {/each}
                </select>
            </label>

            <div class="years">
                <label>
                    <span>From</span>
                    <input type="number" min={firstYear} max={yearTo} bind:value={yearFrom} />
                </label>
                <i>—</i>
                <label>
                    <span>To</span>
                    <input type="number" min={yearFrom} max={lastYear} bind:value={yearTo} />
                </label>
            </div>

            <fieldset>
                <legend>Route weight</legend>
                <div class="segmented">
                    <button
                        class:active={metric === "records"}
                        type="button"
                        onclick={() => (metric = "records")}>Records</button
                    >
                    <button
                        class:active={metric === "value"}
                        type="button"
                        onclick={() => (metric = "value")}>Recorded value</button
                    >
                </div>
            </fieldset>

            <button class="download" type="button" onclick={downloadMappedRows}>
                <ArrowDownToLine size={15} /> Download this selection
            </button>
        </aside>

        <div class="map-stage">
            <div class="map-summary" aria-live="polite">
                <div><strong>{formatNumber(mappedRecords)}</strong><span>mapped records</span></div>
                <div><strong>{routes.length}</strong><span>distinct routes</span></div>
                <div><strong>{textileCount}</strong><span>textile names</span></div>
                <div>
                    <strong>{originCount} → {destinationCount}</strong><span
                        >origins / destinations</span
                    >
                </div>
            </div>

            <div class="map-wrap" bind:this={mapHost} style={`height: ${mapHeight}px`}>
                <div
                    class:ready={mapStatus === "ready"}
                    class="tile-map"
                    bind:this={mapContainer}
                    style={`height: ${mapHeight}px`}
                    aria-label="Interactive OpenFreeMap map of textile trade routes"
                ></div>
                {#if mapStatus === "loading"}
                    <div class="map-status" role="status">Loading OpenFreeMap…</div>
                {:else if mapStatus === "error"}
                    <div class="map-status error" role="alert">
                        <p>OpenFreeMap could not be loaded.</p>
                        <button type="button" onclick={initializeMap}
                            ><RotateCcw size={14} /> Retry</button
                        >
                    </div>
                {/if}
                <div class="legend" aria-hidden="true">
                    <span><i class="origin"></i> Origin</span>
                    <span><i class="destination"></i> Destination</span>
                    <span><b></b> More {metric === "records" ? "records" : "recorded value"}</span>
                </div>
            </div>

            {#if selectedRoute}
                <article class="selected-route">
                    <div class="selected-icon"><Route size={20} /></div>
                    <div>
                        <p>Selected route</p>
                        <h2>
                            {selectedRoute.origin}
                            <ArrowRight size={17} />
                            {selectedRoute.destination}
                        </h2>
                        <span>
                            {selectedRoute.records} records · {selectedRoute.textiles.size} textile names
                            ·
                            {formatNumber(selectedRoute.value)} guilders recorded
                        </span>
                    </div>
                    <button type="button" onclick={() => (selectedRouteKey = "")}>Clear</button>
                </article>
            {/if}
        </div>

        <aside class="route-ranking">
            <div class="ranking-head">
                <div>
                    <MapPin size={17} />
                    <h2>Leading routes</h2>
                </div>
                <span>Top 10</span>
            </div>
            {#if topRoutes.length}
                <ol>
                    {#each topRoutes as routeItem, index}
                        <li>
                            <button
                                class:active={selectedRouteKey === routeItem.key}
                                type="button"
                                onclick={() => selectRoute(routeItem)}
                                aria-pressed={selectedRouteKey === routeItem.key}
                            >
                                <span class="rank">{String(index + 1).padStart(2, "0")}</span>
                                <span class="route-name">
                                    <strong>{routeItem.origin}</strong>
                                    <i><ArrowRight size={12} /> {routeItem.destination}</i>
                                </span>
                                <span class="route-total">{formatNumber(routeItem[metric])}</span>
                                <span
                                    class="route-bar"
                                    style={`--route-width: ${(routeItem[metric] / routeMax) * 100}%`}
                                ></span>
                            </button>
                        </li>
                    {/each}
                </ol>
            {:else}
                <div class="empty">
                    <Route size={24} />
                    <p>No routes match this combination. Broaden one of the filters.</p>
                </div>
            {/if}
        </aside>
    </div>
</section>

<section class="map-notes page-shell">
    <div>
        <h2>A note about modifiers</h2>
    </div>
    <p>
        These fields refer to how the shipped textiles are described archivally. So ‘geography of
        interest’ does not include all textiles originating from a specific region, but rather how
        that textile is listed in cargo manifests. For example a textile described as ‘Coromandel’
        may ship from Batavia to the Dutch Republic; and not all textiles originating in the
        Coromandel Coast are described as such.
    </p>
</section>

<style>
    .map-app {
        padding: 1px 0;
        color: var(--paper);
        background: var(--ink);
    }

    .map-layout {
        display: grid;
        grid-template-columns: 15rem minmax(0, 1fr);
        padding-top: 1.25rem;
        padding-bottom: 1.25rem;
    }

    .map-controls,
    .route-ranking {
        min-width: 0;
        padding: 1.2rem;
        background: #22221d;
        border: 1px solid rgba(244, 239, 229, 0.14);
    }

    .control-heading,
    .ranking-head,
    .control-heading > div,
    .ranking-head > div {
        display: flex;
        gap: 0.55rem;
        align-items: center;
        justify-content: space-between;
    }

    .control-heading > div,
    .ranking-head > div {
        justify-content: flex-start;
    }

    .map-controls h2,
    .route-ranking h2 {
        margin: 0;
        font-family: var(--sans);
        font-size: 0.96rem;
        font-weight: 650;
        letter-spacing: -0.015em;
    }

    .reset {
        display: inline-flex;
        gap: 0.3rem;
        align-items: center;
        padding: 0;
        color: rgba(244, 239, 229, 0.62);
        background: none;
        border: 0;
        font-size: 0.72rem;
        cursor: pointer;
    }

    fieldset,
    .field,
    .years {
        margin: 1.35rem 0 0;
        padding: 0;
        border: 0;
    }

    legend,
    .field > span,
    .years label span {
        display: block;
        margin-bottom: 0.45rem;
        color: rgba(244, 239, 229, 0.58);
        font-family: var(--sans);
        font-size: 0.7rem;
        font-weight: 560;
        letter-spacing: 0;
    }

    .segmented {
        display: grid;
        grid-auto-flow: column;
        grid-auto-columns: 1fr;
        overflow: hidden;
        border-radius: 0.55rem;
    }

    .segmented button {
        min-height: 2.85rem;
        padding: 0.4rem;
        color: rgba(244, 239, 229, 0.68);
        background: transparent;
        border: 1px solid rgba(244, 239, 229, 0.2);
        font-size: 0.75rem;
        cursor: pointer;
    }

    .segmented button + button {
        border-left: 0;
    }

    .segmented button.active {
        color: var(--ink);
        background: var(--saffron);
    }

    .field input,
    .field select,
    .years input {
        width: 100%;
        min-height: 2.85rem;
        padding: 0.65rem 0.78rem;
        color: var(--paper);
        background: #171713;
        border: 1px solid rgba(244, 239, 229, 0.2);
        border-radius: 0.55rem;
        font-size: 0.8rem;
        box-shadow: inset 0 1px 0 rgba(255, 255, 255, 0.035);
    }

    .field select {
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

    .input-with-icon {
        position: relative;
    }

    .input-with-icon :global(svg) {
        position: absolute;
        top: 50%;
        left: 0.65rem;
        color: rgba(244, 239, 229, 0.46);
        transform: translateY(-50%);
    }

    .input-with-icon input {
        padding-left: 2rem;
    }

    .years {
        display: grid;
        grid-template-columns: 1fr auto 1fr;
        gap: 0.55rem;
        align-items: end;
    }

    .years i {
        padding-bottom: 0.55rem;
        color: rgba(244, 239, 229, 0.35);
        font-style: normal;
    }

    .download {
        display: flex;
        gap: 0.45rem;
        align-items: center;
        justify-content: center;
        width: 100%;
        min-height: 2.85rem;
        margin-top: 1.5rem;
        color: var(--paper);
        background: transparent;
        border: 1px solid rgba(244, 239, 229, 0.28);
        border-radius: 0.55rem;
        font-size: 0.74rem;
        font-weight: 700;
        cursor: pointer;
    }

    .download:hover {
        color: var(--ink);
        background: var(--paper);
    }

    .map-stage {
        min-width: 0;
        padding-left: 1.25rem;
    }

    .map-summary {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        margin-bottom: 1rem;
        border: 1px solid rgba(244, 239, 229, 0.14);
    }

    .map-summary > div {
        padding: 0.85rem 1rem;
        border-left: 1px solid rgba(244, 239, 229, 0.14);
    }

    .map-summary > div:first-child {
        border-left: 0;
    }

    .map-summary strong,
    .map-summary span {
        display: block;
    }

    .map-summary strong {
        font-family: var(--serif);
        font-size: 1.35rem;
        font-weight: 400;
    }

    .map-summary span {
        color: rgba(244, 239, 229, 0.48);
        font-family: var(--sans);
        font-size: 0.68rem;
        letter-spacing: 0;
    }

    .map-wrap {
        position: relative;
        width: 100%;
        overflow: hidden;
        background: #173742;
    }

    .tile-map {
        position: absolute;
        z-index: 1;
        inset: 0;
        width: 100%;
        opacity: 0;
        transition: opacity 320ms ease;
    }

    .tile-map.ready {
        opacity: 1;
    }

    :global(.leaflet-container.tile-map) {
        background: #d7d2c4;
        font-family: var(--sans);
    }

    .tile-map :global(.leaflet-control-zoom) {
        overflow: hidden;
        border: 1px solid rgba(23, 23, 17, 0.28);
        border-radius: 0;
        box-shadow: 0 0.35rem 1rem rgba(23, 23, 17, 0.14);
    }

    .tile-map :global(.leaflet-control-zoom a) {
        color: var(--ink);
        border-color: rgba(23, 23, 17, 0.16);
        background: rgba(251, 248, 241, 0.94);
        font-family: var(--sans);
        font-weight: 500;
    }

    .tile-map :global(.leaflet-control-zoom a:hover) {
        background: var(--paper);
    }

    .tile-map :global(.leaflet-control-attribution) {
        padding: 0.18rem 0.35rem;
        color: rgba(23, 23, 17, 0.68);
        background: rgba(251, 248, 241, 0.88);
        font-family: var(--sans);
        font-size: 0.56rem;
    }

    .tile-map :global(.leaflet-control-attribution a) {
        color: var(--indigo-deep);
    }

    .map-status {
        position: absolute;
        z-index: 2;
        top: 0.75rem;
        left: 0.75rem;
        padding: 0.45rem 0.6rem;
        color: rgba(244, 239, 229, 0.78);
        background: rgba(23, 23, 17, 0.84);
        font-family: var(--sans);
        font-size: 0.68rem;
        letter-spacing: 0;
    }

    .map-status.error {
        top: 50%;
        left: 50%;
        display: grid;
        justify-items: center;
        gap: 0.8rem;
        width: min(22rem, calc(100% - 2rem));
        padding: 1.2rem;
        text-align: center;
        transform: translate(-50%, -50%);
    }

    .map-status.error p {
        margin: 0;
        font-size: 0.78rem;
        letter-spacing: 0;
        text-transform: none;
    }

    .map-status.error button {
        display: inline-flex;
        gap: 0.4rem;
        align-items: center;
        padding: 0.5rem 0.75rem;
        color: var(--ink);
        border: 0;
        background: var(--paper);
        font-weight: 700;
        cursor: pointer;
    }

    .legend {
        position: absolute;
        z-index: 3;
        right: 0.75rem;
        bottom: 0.75rem;
        display: flex;
        flex-wrap: wrap;
        gap: 0.8rem;
        padding: 0.5rem 0.65rem;
        color: rgba(244, 239, 229, 0.72);
        background: rgba(23, 23, 17, 0.84);
        font-family: var(--sans);
        font-size: 0.66rem;
    }

    .legend span {
        display: flex;
        gap: 0.35rem;
        align-items: center;
    }

    .legend i {
        width: 0.45rem;
        height: 0.45rem;
        border-radius: 50%;
    }

    .legend .origin {
        background: #f1c96c;
    }

    .legend .destination {
        background: #c8634f;
    }

    .legend b {
        width: 1.4rem;
        height: 0.22rem;
        background: #f1c96c;
    }

    .selected-route {
        display: grid;
        grid-template-columns: auto 1fr auto;
        gap: 0.9rem;
        align-items: center;
        margin-top: 1rem;
        padding: 0.9rem;
        color: var(--ink);
        background: var(--saffron);
    }

    .selected-route p {
        margin: 0;
        font-family: var(--sans);
        font-size: 0.68rem;
        font-weight: 620;
        letter-spacing: 0;
    }

    .selected-route h2 {
        display: flex;
        gap: 0.35rem;
        align-items: center;
        margin: 0.15rem 0;
        font-family: var(--sans);
        font-size: 1rem;
        font-weight: 650;
    }

    .selected-route span {
        font-size: 0.66rem;
    }

    .selected-route button {
        padding: 0;
        background: none;
        border: 0;
        font-size: 0.65rem;
        text-decoration: underline;
        cursor: pointer;
    }

    .ranking-head {
        padding-bottom: 1rem;
        border-bottom: 1px solid rgba(244, 239, 229, 0.15);
    }

    .ranking-head > span {
        color: rgba(244, 239, 229, 0.42);
        font-family: var(--sans);
        font-size: 0.7rem;
    }

    .route-ranking ol {
        display: grid;
        grid-template-columns: repeat(2, minmax(0, 1fr));
        gap: 1px;
        margin: 0;
        padding: 0;
        list-style: none;
        background: rgba(244, 239, 229, 0.12);
    }

    .route-ranking li {
        min-width: 0;
        background: #22221d;
    }

    .route-ranking li button {
        position: relative;
        display: grid;
        grid-template-columns: 2.1rem minmax(0, 1fr) auto;
        gap: 0.45rem 0.8rem;
        align-items: start;
        width: 100%;
        min-height: 6rem;
        padding: 1rem 1.1rem 1.15rem;
        color: var(--paper);
        text-align: left;
        background: transparent;
        border: 0;
        cursor: pointer;
    }

    .route-ranking li button.active {
        color: var(--saffron);
        background: rgba(198, 149, 63, 0.07);
    }

    .rank {
        display: grid;
        place-items: center;
        width: 1.75rem;
        height: 1.75rem;
        color: rgba(244, 239, 229, 0.46);
        border: 1px solid rgba(244, 239, 229, 0.18);
        font-family: var(--sans);
        font-size: 0.65rem;
    }

    .route-total {
        padding-top: 0.15rem;
        color: rgba(244, 239, 229, 0.65);
        font-family: var(--sans);
        font-size: 0.75rem;
        font-variant-numeric: tabular-nums;
    }

    .route-name {
        min-width: 0;
    }

    .route-name strong,
    .route-name i {
        display: block;
        overflow-wrap: anywhere;
    }

    .route-name strong {
        font-family: var(--sans);
        font-size: 0.86rem;
        font-weight: 650;
        line-height: 1.25;
    }

    .route-name i {
        display: flex;
        gap: 0.2rem;
        align-items: center;
        margin-top: 0.25rem;
        color: rgba(244, 239, 229, 0.48);
        font-family: var(--reading);
        font-size: 0.78rem;
        font-style: normal;
        line-height: 1.25;
    }

    .route-bar {
        position: absolute;
        right: 1.1rem;
        bottom: 0.7rem;
        left: 4rem;
        height: 2px;
        background: rgba(244, 239, 229, 0.08);
    }

    .route-bar::after {
        display: block;
        width: var(--route-width);
        height: 100%;
        content: "";
        background: var(--saffron);
    }

    .empty {
        padding: 3rem 1rem;
        color: rgba(244, 239, 229, 0.5);
        text-align: center;
    }

    .empty :global(svg) {
        margin-bottom: 1rem;
    }

    .empty p {
        margin: 0;
        font-size: 0.72rem;
    }

    .map-notes {
        display: grid;
        grid-template-columns: minmax(0, 1fr) minmax(20rem, 0.55fr);
        gap: clamp(3rem, 8vw, 9rem);
        padding-top: clamp(6rem, 10vw, 10rem);
        padding-bottom: clamp(6rem, 10vw, 10rem);
    }

    .map-notes > div:first-child {
        align-self: start;
    }

    .map-notes h2 {
        max-width: 14ch;
        font-family: var(--serif);
        font-size: clamp(2.8rem, 5.5vw, 5.5rem);
        font-weight: 400;
        letter-spacing: -0.05em;
        line-height: 0.95;
    }

    .map-notes > p {
        align-self: end;
        margin: 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 1rem;
        line-height: 1.68;
    }

    .route-ranking {
        grid-column: 1 / -1;
        margin-top: 1rem;
    }

    .ranking-head {
        padding: 0 0 1rem;
        border-bottom: 1px solid rgba(244, 239, 229, 0.15);
    }

    @media (max-width: 980px) {
        .route-ranking ol {
            grid-template-columns: 1fr;
        }
    }

    @media (max-width: 850px) {
        .map-layout {
            grid-template-columns: 1fr;
        }

        .map-controls {
            display: grid;
            grid-template-columns: repeat(2, minmax(0, 1fr));
            gap: 0.85rem 1rem;
        }

        .control-heading,
        .download {
            grid-column: 1 / -1;
        }

        fieldset,
        .field,
        .years {
            margin-top: 0;
        }

        .map-stage {
            padding: 1rem 0 0;
        }

        .route-ranking ol {
            border: 0;
        }

        .map-notes {
            grid-template-columns: 1fr;
        }
    }

    @media (max-width: 620px) {
        .map-controls {
            display: block;
        }

        fieldset,
        .field,
        .years {
            margin-top: 1rem;
        }

        .map-summary {
            grid-template-columns: repeat(2, 1fr);
        }

        .map-summary > div:nth-child(3) {
            border-top: 1px solid rgba(244, 239, 229, 0.14);
            border-left: 0;
        }

        .map-summary > div:nth-child(4) {
            border-top: 1px solid rgba(244, 239, 229, 0.14);
        }

        .route-ranking ol {
            grid-template-columns: 1fr;
        }

        .selected-route {
            grid-template-columns: auto 1fr;
        }

        .selected-route button {
            grid-column: 2;
            justify-self: start;
        }

        .legend {
            left: 0.5rem;
            justify-content: center;
        }
    }
</style>
