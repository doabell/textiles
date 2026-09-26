export const designFeels = [
    { id: "editorial", label: "Editorial" },
    { id: "gallery", label: "Gallery" },
    { id: "immersive", label: "Immersive" },
    { id: "folio", label: "Folio" },
] as const;

export type DesignFeel = (typeof designFeels)[number]["id"];
export const defaultFeel: DesignFeel = "editorial";
export const feelStorageKey = "textiles-feel";
export function isDesignFeel(value: unknown): value is DesignFeel {
    return designFeels.some((feel) => feel.id === value);
}
