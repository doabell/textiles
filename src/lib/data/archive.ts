import { parse } from "csv-parse/browser/esm/sync";
import galleryCsv from "../../../pictures/datasets/material_gallery.csv?raw";

export type ArchiveItem = {
    id: string;
    identifier: string;
    type: "sample";
    title: string;
    artist: string;
    textile: string;
    primaryColor: string;
    secondaryColor: string;
    pattern: string;
    process: string;
    weave: string;
    fiber: string;
    geography: string;
    quality: string;
    sourceText: string;
    additionalInfo: string;
    collection: string;
    inventory: string;
    date: string;
    catalogueUrl: string;
    image: string;
    fullImage: string;
};

function value(input: unknown) {
    const text = String(input ?? "").trim();
    if (!text || text === "NA") return "";
    return text.replaceAll("�", "’");
}

function records(csv: string) {
    return parse(csv, {
        columns: true,
        bom: true,
        skip_empty_lines: true,
        relax_column_count: true,
    }) as Record<string, string>[];
}

function catalogueUrl(row: Record<string, string>) {
    return (
        [row.catalogue_url, row.catalogue_url_image]
            .map(value)
            .find((url) => /^https?:\/\//i.test(url)) ?? ""
    );
}

function splitValues(input: string) {
    return input
        .split(/[,;]/)
        .map((item) => item.trim())
        .filter(Boolean);
}

function uniqueValues(values: string[]) {
    return [...new Set(values)].sort((a, b) => a.localeCompare(b));
}

export const archiveItems: ArchiveItem[] = records(galleryCsv)
    .map((row) => {
        const id = value(row.mat_no);
        const identifier = value(row.textile_identifier);
        const textile = value(row.textile_name);
        const alternateTitle = value(row.title_other);
        const filename = value(row.image_filename_app).replace(/^www[\\/]/i, "");

        return {
            id,
            identifier,
            type: "sample" as const,
            title: textile || alternateTitle || "Unidentified textile",
            artist: "",
            textile: textile || "No known name",
            primaryColor: value(row.textile_color_visual),
            secondaryColor: "",
            pattern: value(row.textile_pattern_visual),
            process: value(row.textile_process_visual),
            weave: value(row.textile_weave_visual),
            fiber: value(row.textile_fiber_visual),
            geography:
                value(row.textile_geography_catalogue) ||
                value(row.orig_loc_region_catalogue) ||
                value(row.orig_loc_port_catalogue),
            quality: value(row.textile_quality_visual),
            sourceText: value(row.text_source),
            additionalInfo: value(row.addtl_info),
            collection: value(row.collection),
            inventory: value(row.id_no) || value(row.id_narrative),
            date: value(row.orig_date),
            catalogueUrl: catalogueUrl(row),
            image: filename ? `/gallery/thumbs/${filename}` : "",
            fullImage: filename ? `/gallery/full/${filename}` : "",
        };
    })
    .filter((item) => item.id && item.image)
    .sort((a, b) => a.id.localeCompare(b.id, undefined, { numeric: true }));

export const archiveOptions = {
    textiles: uniqueValues(
        archiveItems.map((item) => item.textile).filter((item) => item !== "No known name"),
    ),
    colors: uniqueValues(archiveItems.flatMap((item) => splitValues(item.primaryColor))),
    patterns: uniqueValues(archiveItems.flatMap((item) => splitValues(item.pattern))),
    processes: uniqueValues(archiveItems.flatMap((item) => splitValues(item.process))),
    weaves: uniqueValues(archiveItems.flatMap((item) => splitValues(item.weave))),
    fibers: uniqueValues(archiveItems.flatMap((item) => splitValues(item.fiber))),
};
