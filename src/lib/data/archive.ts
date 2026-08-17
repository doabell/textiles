import { parse } from "csv-parse/browser/esm/sync";
import samplesCsv from "../../../pictures/datasets/image_samples.csv?raw";
import paintingsCsv from "../../../pictures/datasets/painting_samples.csv?raw";

export type ArchiveItem = {
    id: string;
    type: "sample" | "painting";
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
    collection: string;
    inventory: string;
    date: string;
    catalogueUrl: string;
    image: string;
};

const imageModules = import.meta.glob("../../../pictures/img/*.jpg", {
    eager: true,
    query: "?url",
    import: "default",
}) as Record<string, string>;

const images = new Map(
    Object.entries(imageModules).map(([path, url]) => {
        const id = path.match(/([^/\\]+)\.jpg$/)?.[1] ?? path;
        return [id, url];
    }),
);

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

const samples: ArchiveItem[] = records(samplesCsv)
    .map((row) => {
        const id = value(row.image_ID);
        const textile = value(row.textile_name);
        return {
            id,
            type: "sample" as const,
            title: textile || "Unidentified textile sample",
            artist: "",
            textile: textile || "No known name",
            primaryColor: value(row.textile_color_visual_primary),
            secondaryColor: value(row.textile_color_visual_secondary),
            pattern: value(row.textile_pattern_visual),
            process: value(row.textile_process_visual),
            weave: value(row.textile_weave_visual),
            fiber: value(row.textile_fiber_visual),
            geography: value(row.textile_geography_catalogue),
            collection: value(row.collection),
            inventory: value(row.id_no),
            date: value(row.Date),
            catalogueUrl: value(row.catalogue_url),
            image: images.get(id) ?? "",
        };
    })
    .filter((item) => item.id && item.image);

const paintings: ArchiveItem[] = records(paintingsCsv)
    .map((row) => {
        const id = value(row.image_ID);
        return {
            id,
            type: "painting" as const,
            title: value(row.Title) || "Untitled pictorial record",
            artist: value(row.Artist),
            textile: value(row.textile_name) || "No known name",
            primaryColor: value(row.textile_color_visual_primary),
            secondaryColor: value(row.textile_color_visual_secondary),
            pattern: value(row.textile_pattern_visual),
            process: value(row.textile_process_visual),
            weave: "",
            fiber: "",
            geography: "",
            collection: value(row.collection),
            inventory: value(row.id_no),
            date: value(row.Date),
            catalogueUrl: value(row.catalogue_url),
            image: images.get(id) ?? "",
        };
    })
    .filter((item) => item.id && item.image);

export const archiveItems = [...samples, ...paintings];

export const archiveOptions = {
    colors: [
        ...new Set(
            archiveItems
                .flatMap((item) => [item.primaryColor, item.secondaryColor])
                .filter(Boolean),
        ),
    ].sort(),
    patterns: [...new Set(archiveItems.map((item) => item.pattern).filter(Boolean))].sort(),
    processes: [...new Set(archiveItems.map((item) => item.process).filter(Boolean))].sort(),
    fibers: [...new Set(archiveItems.map((item) => item.fiber).filter(Boolean))].sort(),
};
