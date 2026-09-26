import { readFileSync, readdirSync } from "node:fs";
import { resolve } from "node:path";
import { geoArea, geoCentroid } from "d3-geo";
import type { Feature, FeatureCollection, Geometry } from "geojson";
import { readRDataFrame } from "./rds.ts";

export type TradeRecord = {
    id: number;
    exchangeNumber: string;
    source: string;
    company: "VOC" | "WIC" | "Unknown";
    year: number | null;
    origin: string;
    destination: string;
    originPort: string;
    destinationPort: string;
    originLat: number | null;
    originLong: number | null;
    destinationLat: number | null;
    destinationLong: number | null;
    textile: string;
    quantity: number | null;
    unit: string;
    originalUnit: string;
    value: number | null;
    pricePerUnit: number | null;
    priceUnit: string;
    color: string;
    inferredColor: string;
    pattern: string;
    process: string;
    fiber: string;
    geography: string;
    quality: string;
    other: string;
};

let tradeRecords: TradeRecord[] | undefined;
let originalRecords: ReturnType<typeof readRDataFrame> | undefined;

function getOriginalRecords() {
    return (originalRecords ??= readRDataFrame(resolve(process.cwd(), "data", "week4.rds")));
}

export function getOriginalTradeCsv() {
    const records = getOriginalRecords();
    const columns = Object.keys(records[0] ?? {});
    const cell = (value: string | number | null) =>
        '"' + String(value ?? "").replaceAll('"', '""') + '"';
    return [
        columns.map(cell).join(","),
        ...records.map((row) => columns.map((column) => cell(row[column])).join(",")),
    ].join("\r\n");
}

function clean(value: string | number | null | undefined) {
    return value === null || value === undefined ? "" : String(value).trim();
}

function numberOrNull(value: string | number | null | undefined) {
    if (value === null || value === undefined || value === "") return null;
    const number = Number(value);
    return Number.isFinite(number) ? number : null;
}

function normalizeName(value: string) {
    return value
        .toLocaleLowerCase("en")
        .normalize("NFKD")
        .replace(/[^\p{L}\p{N}\s-]/gu, "")
        .replace(/\s+/g, " ")
        .trim();
}

// The source GeoJSON follows the RFC winding convention; D3 expects clockwise
// exterior rings for spherical polygons smaller than a hemisphere.
function sphericalGeometry(geometry: Geometry): Geometry {
    if (geometry.type === "GeometryCollection") {
        return { ...geometry, geometries: geometry.geometries.map(sphericalGeometry) };
    }
    if (geometry.type === "Polygon") {
        return geoArea(geometry) > 2 * Math.PI
            ? { ...geometry, coordinates: geometry.coordinates.map((ring) => [...ring].reverse()) }
            : geometry;
    }
    if (geometry.type === "MultiPolygon") {
        return {
            ...geometry,
            coordinates: geometry.coordinates.map((coordinates) => {
                const polygon = sphericalGeometry({ type: "Polygon", coordinates });
                return polygon.type === "Polygon" ? polygon.coordinates : coordinates;
            }),
        };
    }
    return geometry;
}

function getRegionCenters() {
    const directory = resolve(process.cwd(), "data", "geoJSON");
    const centers = new Map<string, [number, number]>();
    for (const file of readdirSync(directory).filter((file) => file.endsWith(".json"))) {
        const source = JSON.parse(readFileSync(resolve(directory, file), "utf8")) as
            Geometry | Feature | FeatureCollection;
        const geometry =
            source.type === "FeatureCollection"
                ? {
                      ...source,
                      features: source.features.map((feature) => ({
                          ...feature,
                          geometry: sphericalGeometry(feature.geometry),
                      })),
                  }
                : source.type === "Feature"
                  ? { ...source, geometry: sphericalGeometry(source.geometry) }
                  : sphericalGeometry(source);
        const center = geoCentroid(geometry);
        if (!center.every(Number.isFinite)) throw new Error("Invalid geography: " + file);
        centers.set(file.slice(0, -5), center);
    }
    return centers;
}

export function getTradeRecords() {
    if (tradeRecords) return tradeRecords;
    // Both original Shiny apps read this exact processed dataset. Its currency,
    // quantity and price calculations are preserved from data/clean.R.
    const rows = getOriginalRecords();
    const centers = getRegionCenters();
    tradeRecords = rows.map((row, index): TradeRecord => {
        const companyValue = clean(row.company);
        const originalUnit = clean(row.textile_unit);
        // clean.R converts these quantities to pieces before saving the RDS.
        const unit = ["el", "half ps.", "halve ps."].includes(originalUnit) ? "ps." : originalUnit;
        const origin = clean(row.orig_loc_region_modern);
        const destination = clean(row.dest_loc_region);
        // These are representative region positions, not inferred port positions.
        const originCenter = centers.get(origin);
        const destinationCenter = centers.get(destination);
        return {
            id: index + 1,
            exchangeNumber: clean(row.exchange_nr),
            source: clean(row.source),
            company: companyValue === "VOC" || companyValue === "WIC" ? companyValue : "Unknown",
            year: numberOrNull(row.orig_yr) ?? numberOrNull(row.dest_yr),
            origin,
            destination,
            originPort: clean(row.orig_loc_port_modern),
            destinationPort: clean(row.dest_loc_port),
            originLat: originCenter?.[1] ?? null,
            originLong: originCenter?.[0] ?? null,
            destinationLat: destinationCenter?.[1] ?? null,
            destinationLong: destinationCenter?.[0] ?? null,
            textile: clean(row.textile_name),
            quantity: numberOrNull(row.textile_quantity),
            unit,
            originalUnit,
            value: numberOrNull(row.total_value),
            pricePerUnit: numberOrNull(row.price_per_unit),
            priceUnit: unit,
            color: clean(row.textile_color_arch),
            inferredColor: clean(row.textile_color_inf),
            pattern: clean(row.textile_pattern_arch),
            process: clean(row.textile_process_arch),
            fiber: clean(row.textile_fiber_arch),
            geography: clean(row.textile_geography_arch),
            quality: clean(row.textile_quality_arch),
            other: clean(row.textile_other_unknown_arch),
        };
    });
    return tradeRecords;
}

export function getTextileStats(terms: string[]) {
    const normalizedTerms = new Set(terms.map(normalizeName));
    const records = getTradeRecords().filter((record) =>
        normalizedTerms.has(normalizeName(record.textile)),
    );

    const destinationCounts = new Map<string, number>();
    const originCounts = new Map<string, number>();

    for (const record of records) {
        if (record.destination) {
            destinationCounts.set(
                record.destination,
                (destinationCounts.get(record.destination) ?? 0) + 1,
            );
        }
        if (record.origin) {
            originCounts.set(record.origin, (originCounts.get(record.origin) ?? 0) + 1);
        }
    }

    const sortCounts = (counts: Map<string, number>) =>
        [...counts]
            .map(([name, count]) => ({ name, count }))
            .sort((a, b) => b.count - a.count || a.name.localeCompare(b.name))
            .slice(0, 4);

    const years = records
        .map((record) => record.year)
        .filter((year): year is number => year !== null);
    const companies = [
        ...new Set(records.map((record) => record.company).filter((item) => item !== "Unknown")),
    ];

    return {
        records: records.length,
        destinations: destinationCounts.size,
        origins: originCounts.size,
        companies,
        firstYear: years.length ? Math.min(...years) : null,
        lastYear: years.length ? Math.max(...years) : null,
        topDestinations: sortCounts(destinationCounts),
        topOrigins: sortCounts(originCounts),
    };
}

export function getExplorerOptions(records = getTradeRecords()) {
    const unique = (values: string[]) =>
        [...new Set(values.filter(Boolean))].sort((a, b) => a.localeCompare(b));

    return {
        textiles: unique(records.map((record) => record.textile)),
        origins: unique(records.map((record) => record.origin)),
        destinations: unique(records.map((record) => record.destination)),
        colors: unique(records.map((record) => record.color)),
        patterns: unique(records.map((record) => record.pattern)),
        processes: unique(records.map((record) => record.process)),
        fibers: unique(records.map((record) => record.fiber)),
        years: [
            ...new Set(
                records
                    .map((record) => record.year)
                    .filter((year): year is number => year !== null),
            ),
        ].sort((a, b) => a - b),
    };
}
