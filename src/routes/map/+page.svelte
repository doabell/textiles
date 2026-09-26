<script lang="ts">
    import MultiSelect from "$lib/components/MultiSelect.svelte";
    import ChartDownload from "$lib/components/ChartDownload.svelte";
    import {
        downloadBlob,
        downloadChart,
        downloadMapImage,
        exportFilename,
    } from "$lib/utils/download";

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
    let controlsOpen = $state(true);
    let modifierMatch = $state<"all" | "any">("all");
    let textile = $state<string[]>([]);
    let modifier = $state<string[]>([]);
    let place = $state<string[]>([]);
    let yearFrom = $state(firstYear);
    let yearTo = $state(lastYear);
    let metric = $state<"records" | "value">("records");
    let selectedRouteKey = $state("");
    let mapHost = $state<HTMLDivElement>();
    let mapContainer = $state<HTMLDivElement>();
    let mapWidth = $state(1000);
    let mapStatus = $state<"loading" | "ready" | "error">("loading");
    let mapExportReady = $state(false);
    let interactiveMap: LeafletMap | null = null;
    let leaflet: typeof import("leaflet") | null = null;
    let routeLayer: LeafletLayerGroup | null = null;
    let pointLayer: LeafletLayerGroup | null = null;
    let routeRenderer: LeafletCanvas | null = null;
    const routeLines = new Map<string, LeafletPolyline>();
    const routeStyles = new Map<string, { color: string; weight: number }>();
    let destroyed = false;

    const mapHeight = $derived(Math.max(420, Math.min(820, mapWidth * 0.72)));
    const normalize = (value: string) => value.toLocaleLowerCase("en").trim();
    const modifiers = [
        ...new Set(
            initialData.records.flatMap((record) => [
                record.color,
                record.inferredColor,
                record.pattern,
                record.process,
                record.fiber,
                record.quality,
                record.geography,
                record.other,
            ]),
        ),
    ]
        .filter(Boolean)
        .sort((a, b) => a.localeCompare(b));

    const filtered = $derived.by(() =>
        data.records.filter((record) => {
            const terms = [
                record.color,
                record.inferredColor,
                record.pattern,
                record.process,
                record.fiber,
                record.quality,
                record.geography,
                record.other,
            ].map(normalize);
            const modifierMatches = modifier.map((value) => terms.includes(normalize(value)));
            return (
                (!company || record.company === company) &&
                (!textile.length ||
                    textile.some((value) => normalize(record.textile) === normalize(value))) &&
                (!modifier.length ||
                    (modifierMatch === "all"
                        ? modifierMatches.every(Boolean)
                        : modifierMatches.some(Boolean))) &&
                (!place.length ||
                    place.includes(record.origin) ||
                    place.includes(record.destination)) &&
                (record.year === null || (record.year >= yearFrom && record.year <= yearTo))
            );
        }),
    );

    type RouteSummary = {
        key: string;
        origin: string;
        destination: string;
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
                origin: record.origin || "Unrecorded origin",
                destination: record.destination || "Unrecorded destination",
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
            if (record.textile) current.textiles.add(record.textile);
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
    const textileCount = $derived(
        new Set(filtered.map((record) => record.textile).filter(Boolean)).size,
    );

    onMount(() => {
        controlsOpen = !window.matchMedia("(max-width: 900px)").matches;
        textile = new URL(window.location.href).searchParams.getAll("textile");
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
        mapExportReady = false;

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

            let loadedTiles = 0;
            let failedTiles = 0;
            const tiles = L.tileLayer(
                "https://tiles.openfreemap.org/natural_earth/ne2sr/{z}/{x}/{y}.png",
                {
                    minZoom: 1,
                    maxZoom: 5,
                    maxNativeZoom: 5,
                    noWrap: true,
                    bounds: [
                        [-85.051129, -180],
                        [85.051129, 180],
                    ],
                    crossOrigin: "anonymous",
                    detectRetina: false,
                    attribution:
                        '<a href="https://openfreemap.org/">OpenFreeMap</a> · <a href="https://www.naturalearthdata.com/">Natural Earth</a>',
                },
            );
            tiles.on({
                loading: () => {
                    mapExportReady = false;
                    loadedTiles = 0;
                    failedTiles = 0;
                },
                tileload: () => {
                    loadedTiles += 1;
                    if (!destroyed && interactiveMap === map) mapStatus = "ready";
                },
                tileerror: () => {
                    failedTiles += 1;
                },
                load: () => {
                    if (destroyed || interactiveMap !== map) return;
                    mapStatus = !loadedTiles && failedTiles ? "error" : "ready";
                    mapExportReady = failedTiles === 0;
                },
            });
            tiles.addTo(map);

            routeLayer = L.layerGroup().addTo(map);
            pointLayer = L.layerGroup().addTo(map);
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
        textile = [];
        modifier = [];
        place = [];
        yearFrom = firstYear;
        yearTo = lastYear;
        selectedRouteKey = "";
        modifierMatch = "all";
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
            "exchangeNumber",
            "source",
            "year",
            "origin",
            "originPort",
            "destination",
            "destinationPort",
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
        const rows = [
            columns.join(","),
            ...filtered.map((record) => columns.map((column) => csvCell(record[column])).join(",")),
        ];
        const blob = new Blob(["\uFEFF", rows.join("\n")], { type: "text/csv;charset=utf-8" });
        downloadBlob(
            blob,
            exportFilename(
                "map-records",
                [textile, company, place, modifier, yearFrom, yearTo],
                "csv",
            ),
        );
    }

    function exportRoutesImage() {
        return downloadChart({
            title: "Textile Geographies",
            context: [
                "Years: " + yearFrom + "–" + yearTo,
                "Textile: " + (textile.join(", ") || "All"),
                "Company: " + (company || "VOC + WIC"),
                "Region: " + (place.join(", ") || "All"),
                "Modifiers: " + (modifier.join(", ") || "All"),
                "Match: " + modifierMatch,
            ],
            series: [
                {
                    label: metric === "records" ? "Record count" : "Recorded value",
                    color: "#1f4654",
                },
            ],
            rows: topRoutes.map((row) => ({
                label: row.origin + " → " + row.destination,
                values: [row[metric]],
                display: [formatNumber(row[metric]) + (metric === "value" ? " ƒ" : "")],
            })),
            filename: exportFilename(
                "routes",
                [textile, company, place, modifier, modifierMatch, yearFrom, yearTo, metric],
                "png",
            ),
        });
    }
    function exportMapImage() {
        if (!mapContainer) return Promise.reject(new Error("Map unavailable"));
        return downloadMapImage(mapContainer, {
            title: "Textile Geographies",
            context: [
                "Years: " + yearFrom + "–" + yearTo,
                "Textile: " + (textile.join(", ") || "All"),
                "Region: " + (place.join(", ") || "All"),
                "Company: " + (company || "VOC + WIC"),
                "Modifiers: " + (modifier.join(", ") || "All"),
                "Measure: " + metric,
                "Match: " + modifierMatch,
            ],
            filename: exportFilename(
                "map",
                [textile, company, place, modifier, modifierMatch, yearFrom, yearTo, metric],
                "png",
            ),
        });
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
                <button
                    class="filter-toggle"
                    type="button"
                    aria-expanded={controlsOpen}
                    onclick={() => (controlsOpen = !controlsOpen)}
                    ><Compass size={17} /> Filters</button
                >
                <button type="button" class="reset" onclick={resetFilters}>
                    <RotateCcw size={14} /> Reset
                </button>
            </div>

            {#if controlsOpen}
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
                            <CompanyMark company="VOC" inverted={company === "VOC"} />
                        </button>
                        <button
                            class:active={company === "WIC"}
                            type="button"
                            onclick={() => (company = "WIC")}
                        >
                            <CompanyMark company="WIC" inverted={company === "WIC"} />
                        </button>
                    </div>
                </fieldset>

                <div class="field">
                    <MultiSelect
                        label="Textile name"
                        options={data.options.textiles}
                        bind:value={textile}
                    />
                </div>

                <div class="field">
                    <MultiSelect
                        label="Archival modifier"
                        options={modifiers}
                        bind:value={modifier}
                    />
                </div>
                <fieldset>
                    <legend>Match modifiers</legend>
                    <div class="segmented">
                        <button
                            type="button"
                            class:active={modifierMatch === "all"}
                            onclick={() => (modifierMatch = "all")}>All</button
                        ><button
                            type="button"
                            class:active={modifierMatch === "any"}
                            onclick={() => (modifierMatch = "any")}>Any</button
                        >
                    </div>
                </fieldset>

                <div class="field">
                    <MultiSelect
                        label="Origin or destination"
                        options={places}
                        bind:value={place}
                    />
                </div>

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
            {/if}
        </aside>

        <div class="map-stage">
            <div class="export-toolbar">
                <ChartDownload
                    action={exportMapImage}
                    label="Download map"
                    disabled={!mapExportReady}
                /><ChartDownload
                    action={exportRoutesImage}
                    label="Download routes"
                    disabled={!topRoutes.length}
                />
            </div>
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
                    aria-label="Textile trade map"
                ></div>
                {#if mapStatus === "loading"}
                    <div class="map-status" role="status">Loading OpenFreeMap…</div>
                {:else if mapStatus === "error"}
                    <div class="map-status error" role="alert">
                        <p>Map unavailable.</p>
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
                        <div class="selected-metrics">
                            <span>{selectedRoute.records} records</span>
                            <span>{selectedRoute.textiles.size} textiles</span>
                            <span>{formatNumber(selectedRoute.value)} guilders</span>
                        </div>
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
                    <p>No matching routes.</p>
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
    .export-toolbar {
        display: flex;
        justify-content: flex-end;
        flex-wrap: wrap;
        gap: 0.5rem;
        padding-bottom: 1rem;
    }
    .map-app {
        font-family: var(--sans);
        font-variant-numeric: tabular-nums lining-nums;
        background: var(--paper);
        color: var(--ink);
        padding: 2rem 0 4rem;
    }
    .map-layout {
        display: grid;
        grid-template-columns: 15rem minmax(0, 1fr);
        gap: 2.5rem;
        align-items: start;
    }
    .map-controls {
        position: sticky;
        top: 6rem;
        grid-row: 1 / 3;
        min-width: 0;
        padding-right: 1.8rem;
        border-right: 1px solid var(--line-strong);
    }
    .control-heading,
    .ranking-head,
    .ranking-head > div {
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 0.7rem;
    }
    .control-heading {
        padding-bottom: 1.25rem;
        border-bottom: 1px solid var(--line);
    }
    .control-heading button {
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
    .control-heading .filter-toggle {
        font-weight: 700;
        font-size: 0.95rem;
    }
    fieldset,
    .field,
    .years {
        display: block;
        padding: 0;
        margin: 1.3rem 0 0;
        border: 0;
    }
    legend,
    .years label span {
        display: block;
        margin-bottom: 0.5rem;
        color: var(--ink-soft);
        font-size: 0.8125rem;
        font-weight: 600;
    }
    .segmented {
        display: flex;
        border-bottom: 1px solid var(--line-strong);
    }
    .segmented button {
        flex: 1;
        display: grid;
        place-items: center;
        min-height: 2.8rem;
        padding: 0.4rem;
        border: 0;
        background: transparent;
        color: inherit;
        font-size: 0.8125rem;
        cursor: pointer;
    }
    .segmented button.active {
        background: var(--ink);
        color: var(--paper);
    }
    .years input {
        width: 100%;
        min-height: 2.8rem;
        padding: 0.55rem 0.65rem;
        border: 1px solid var(--line-strong);
        border-radius: 0;
        color: var(--ink);
        background: transparent;
        font-size: 0.875rem;
    }
    .years {
        display: grid;
        grid-template-columns: 1fr auto 1fr;
        gap: 0.4rem;
        align-items: end;
    }
    .years i {
        padding-bottom: 0.65rem;
        font-style: normal;
    }
    .download {
        display: flex;
        justify-content: center;
        align-items: center;
        gap: 0.5rem;
        width: 100%;
        min-height: 2.85rem;
        padding: 0.65rem 0.4rem;
        margin-top: 1.5rem;
        border: 0;
        background: var(--accent-fill);
        color: white;
        font-size: 0.8125rem;
        font-weight: 600;
        cursor: pointer;
    }
    button:focus-visible,
    input:focus-visible {
        outline: 2px solid var(--accent-fill);
        outline-offset: 3px;
    }
    .map-stage {
        min-width: 0;
    }
    .map-summary {
        display: grid;
        grid-template-columns: repeat(4, 1fr);
        gap: 1rem;
        padding: 0 0 1.8rem;
    }
    .map-summary strong,
    .map-summary span {
        display: block;
    }
    .map-summary strong {
        font-family: var(--sans);
        font-size: clamp(1.5rem, 2.8vw, 3rem);
        letter-spacing: -0.035em;
        font-weight: 500;
        line-height: 1.1;
        font-variant-numeric: tabular-nums lining-nums;
    }
    .map-summary span {
        margin-top: 0.45rem;
        font-size: 0.8125rem;
        color: var(--ink-soft);
    }
    .map-wrap {
        position: relative;
        width: 100%;
        overflow: hidden;
        background: #203535;
    }
    .tile-map {
        position: absolute;
        z-index: 1;
        inset: 0;
        width: 100%;
        opacity: 0;
        transition: opacity 300ms;
    }
    .tile-map.ready {
        opacity: 1;
    }
    :global(.leaflet-container.tile-map) {
        background: #d7d2c4;
        font-family: var(--sans);
    }
    .tile-map :global(.leaflet-control-zoom) {
        border: 0;
        border-radius: 0;
        box-shadow: none;
    }
    .tile-map :global(.leaflet-control-zoom a) {
        width: 38px;
        height: 38px;
        line-height: 38px;
        color: var(--ink);
        background: var(--paper);
        border-color: var(--line);
        font-weight: 400;
    }
    .tile-map :global(.leaflet-control-attribution) {
        padding: 0.2rem 0.4rem;
        background: color-mix(in srgb, var(--paper) 90%, transparent);
        font-size: 0.8125rem;
    }
    .tile-map :global(.leaflet-control-attribution a) {
        color: var(--ink);
    }
    .map-status {
        position: absolute;
        z-index: 2;
        top: 1rem;
        left: 1rem;
        padding: 0.6rem 0.8rem;
        background: var(--paper);
        color: var(--ink);
        font-size: 0.8125rem;
    }
    .map-status.error {
        top: 50%;
        left: 50%;
        display: grid;
        justify-items: center;
        gap: 1rem;
        width: min(21rem, calc(100% - 2rem));
        padding: 2rem;
        transform: translate(-50%, -50%);
    }
    .map-status.error p {
        margin: 0;
        font-size: 1.1rem;
    }
    .map-status.error button {
        display: flex;
        gap: 0.45rem;
        align-items: center;
        min-height: 2.8rem;
        padding: 0.5rem 1rem;
        border: 0;
        background: var(--accent-fill);
        color: white;
        cursor: pointer;
    }
    .legend {
        position: absolute;
        z-index: 3;
        left: 1rem;
        bottom: 1.5rem;
        display: flex;
        flex-wrap: wrap;
        gap: 1rem;
        padding: 0.6rem 0.8rem;
        background: #171711df;
        color: var(--inverse-ink);
        font-size: 0.8125rem;
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
        height: 0.2rem;
        background: #f1c96c;
    }
    .selected-route {
        display: grid;
        grid-template-columns: auto 1fr auto;
        gap: 1rem;
        align-items: center;
        padding: 1.2rem 0;
        border-bottom: 2px solid var(--accent-fill);
    }
    .selected-route p {
        margin: 0;
        color: var(--ink-soft);
        font-size: 0.8125rem;
    }
    .selected-route h2 {
        display: flex;
        align-items: center;
        flex-wrap: wrap;
        gap: 0.5rem;
        margin: 0.3rem 0;
        font: 600 1.1rem var(--sans);
    }
    .selected-metrics {
        display: flex;
        flex-wrap: wrap;
        gap: 0.3rem 1rem;
        font-size: 0.8125rem;
        color: var(--ink-soft);
    }
    .selected-route button {
        min-height: 2.75rem;
        padding: 0.5rem;
        border: 0;
        background: none;
        text-decoration: underline;
        cursor: pointer;
    }
    .route-ranking {
        grid-column: 2;
        min-width: 0;
    }
    .ranking-head {
        margin-bottom: 0.75rem;
    }
    .ranking-head h2 {
        margin: 0;
        font: 600 1.2rem var(--sans);
        letter-spacing: var(--display-tracking, -0.025em);
    }
    .ranking-head > span {
        font-size: 0.8125rem;
        color: var(--ink-soft);
    }
    .route-ranking ol {
        display: grid;
        grid-template-columns: 1fr 1fr;
        column-gap: 2rem;
        margin: 0;
        padding: 0;
        list-style: none;
    }
    .route-ranking li {
        min-width: 0;
        border-top: 1px solid var(--line-strong);
    }
    .route-ranking li button {
        position: relative;
        display: grid;
        grid-template-columns: 1.8rem minmax(0, 1fr) auto;
        gap: 0.75rem;
        width: 100%;
        min-height: 6rem;
        padding: 1rem 0 1.4rem;
        border: 0;
        background: none;
        color: inherit;
        text-align: left;
        cursor: pointer;
    }
    .route-ranking li button.active,
    .route-ranking li button:hover {
        color: var(--accent-fill);
    }
    .rank {
        padding-top: 0.15rem;
        color: var(--ink-soft);
        font-size: 0.8125rem;
        font-variant-numeric: tabular-nums lining-nums;
    }
    .route-name {
        min-width: 0;
    }
    .route-name strong {
        display: block;
        font-size: 0.93rem;
        font-weight: 600;
    }
    .route-name i {
        display: flex;
        align-items: center;
        gap: 0.3rem;
        margin-top: 0.2rem;
        font-size: 0.8125rem;
        color: var(--ink-soft);
        font-style: normal;
    }
    .route-total {
        font-size: 0.85rem;
        font-variant-numeric: tabular-nums lining-nums;
    }
    .route-bar {
        position: absolute;
        bottom: 0.7rem;
        left: 2.55rem;
        right: 0;
        height: 2px;
        background: var(--line);
    }
    .route-bar::after {
        content: "";
        display: block;
        width: var(--route-width);
        height: 100%;
        background: var(--accent-fill);
    }
    .empty {
        padding: 4rem 0;
        text-align: center;
        color: var(--ink-soft);
    }
    .empty p {
        font-size: 0.85rem;
    }
    .map-notes {
        display: grid;
        grid-template-columns: 1fr 2fr;
        gap: 3rem;
        padding-top: 4rem;
        padding-bottom: 5rem;
        border-top: 1px solid var(--line-strong);
    }
    .map-notes h2 {
        margin: 0;
        max-width: 14ch;
        font: 500 clamp(1.6rem, 3vw, 3rem)/1.1 var(--sans);
        letter-spacing: var(--display-tracking, -0.04em);
    }
    .map-notes p {
        margin: 0;
        max-width: 65ch;
        font-size: 0.98rem;
        line-height: 1.7;
        color: var(--ink-soft);
    }
    @media (max-width: 1100px) {
        .map-layout {
            grid-template-columns: 13rem minmax(0, 1fr);
            gap: 1.5rem;
        }
        .map-controls {
            padding-right: 1rem;
        }
        .route-ranking ol {
            column-gap: 1rem;
        }
    }
    @media (max-width: 900px) {
        .years input {
            font-size: 1rem;
        }
        .map-app {
            padding-top: 1rem;
        }
        .map-layout {
            display: block;
        }
        .map-controls {
            position: static;
            padding: 0 0 1.5rem;
            border-right: 0;
        }
        .control-heading {
            padding-bottom: 0.75rem;
        }
        .map-stage {
            padding-top: 1rem;
        }
        .route-ranking {
            margin-top: 2rem;
        }
        .map-summary strong {
            font-size: clamp(1.5rem, 4vw, 2.7rem);
        }
        .map-notes {
            grid-template-columns: 1fr;
            gap: 1.5rem;
        }
    }
    @media (max-width: 550px) {
        .map-summary {
            grid-template-columns: 1fr 1fr;
            gap: 1.5rem;
        }
        .map-summary strong {
            font-size: 2.2rem;
        }
        .route-ranking ol {
            grid-template-columns: 1fr;
        }
        .selected-route {
            grid-template-columns: 1fr auto;
        }
        .selected-icon {
            display: none;
        }
        .legend {
            right: 0.6rem;
            left: 0.6rem;
            gap: 0.7rem;
            font-size: 0.8125rem;
        }
    }
</style>
