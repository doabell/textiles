import { readFileSync } from "node:fs";
import { resolve } from "node:path";
import { parse } from "csv-parse/sync";

type SourceRow = Record<string, string>;

export type TradeRecord = {
    id: number;
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
    value: number | null;
    pricePerUnit: number | null;
    color: string;
    pattern: string;
    process: string;
    fiber: string;
    quality: string;
};

let sourceRows: SourceRow[] | undefined;
let tradeRecords: TradeRecord[] | undefined;

const emptyValues = new Set(["", "NA", "N/A", "null", "undefined"]);

function clean(value: string | undefined) {
    if (!value || emptyValues.has(value.trim())) return "";
    return value.trim();
}

function numberOrNull(value: string | undefined) {
    const cleaned = clean(value).replace(/,/g, "");
    if (!cleaned) return null;
    const number = Number(cleaned);
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

function getSourceRows() {
    if (sourceRows) return sourceRows;

    const filePath = resolve(process.cwd(), "pictures", "datasets", "WIC_VOC_Cleaned.csv");
    sourceRows = parse(readFileSync(filePath, "utf8"), {
        columns: true,
        bom: true,
        skip_empty_lines: true,
        relax_column_count: true,
    }) as SourceRow[];

    return sourceRows;
}

export function getTradeRecords() {
    if (tradeRecords) return tradeRecords;

    tradeRecords = getSourceRows()
        .map((row, index): TradeRecord | null => {
            const textile = clean(row.textile_name);
            if (!textile) return null;

            const companyValue = clean(row.company);
            const company =
                companyValue === "VOC" || companyValue === "WIC"
                    ? companyValue
                    : ("Unknown" as const);
            const guildersPer = numberOrNull(row.guilders_per);
            const stuiversPer = numberOrNull(row.stuivers_per);
            const penningenPer = numberOrNull(row.pennigen_per);
            const calculatedPrice =
                guildersPer !== null || stuiversPer !== null || penningenPer !== null
                    ? (guildersPer ?? 0) + (stuiversPer ?? 0) / 20 + (penningenPer ?? 0) / 320
                    : null;

            return {
                id: index + 1,
                company,
                year: numberOrNull(row.orig_yr) ?? numberOrNull(row.dest_yr),
                origin: clean(row.orig_loc_region_modern) || clean(row.orig_loc_region),
                destination: clean(row.dest_loc_region) || clean(row.dest_loc_region_modern),
                originPort: clean(row.orig_loc_port_modern) || clean(row.orig_loc_port),
                destinationPort: clean(row.dest_loc_port_modern) || clean(row.dest_loc_port),
                originLat: numberOrNull(row.orig_loc_lat) ?? numberOrNull(row["orig_loc_lat...9"]),
                originLong:
                    numberOrNull(row.orig_loc_long) ?? numberOrNull(row["orig_loc_lat...10"]),
                destinationLat: numberOrNull(row.dest_loc_lat),
                destinationLong: numberOrNull(row.dest_loc_long),
                textile,
                quantity: numberOrNull(row.textile_quantity),
                unit: clean(row.textile_unit),
                value: numberOrNull(row.textile_value) ?? numberOrNull(row.total_value_1),
                pricePerUnit: calculatedPrice,
                color: clean(row.textile_color_arch) || clean(row.color_category),
                pattern: clean(row.textile_pattern_arch),
                process: clean(row.textile_process_arch),
                fiber: clean(row.textile_fiber_arch),
                quality: clean(row.textile_quality_arch) || clean(row.textile_quality_inferred),
            };
        })
        .filter((row): row is TradeRecord => row !== null);

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
