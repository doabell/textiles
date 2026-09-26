export type ReferencePart = { text: string; number: number | null; id: string };
/** Keep source text intact; link only markers with an existing note. */
export function referenceParts(text: string, prefix: string, noteCount: number): ReferencePart[] {
    return text.split(/(\[\d+\])/g).map((part, index) => {
        const match = /^\[(\d+)\]$/.exec(part);
        const number = match ? Number(match[1]) : 0;
        return {
            text: part,
            number: number > 0 && number <= noteCount ? number : null,
            id: `${prefix}-${index}`,
        };
    });
}
