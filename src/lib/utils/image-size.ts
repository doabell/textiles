import dimensions from "$lib/data/image-dimensions.json";
export function imageSize(path: string): { width?: number; height?: number } {
    return (dimensions as Record<string, { width: number; height: number }>)[path] ?? {};
}
