import { readFileSync } from "node:fs";

type Scalar = string | number | null;
type Value = Scalar | Vector | Pair;
type Vector = { values: Value[]; attributes: Pair | null };
type Pair = { tag: string | null; value: Value; next: Pair | null };

/**
 * Read the uncompressed XDR-v2 data frames used by the original Shiny apps.
 * Supports their atomic columns, factors and attributes; rejects other R objects.
 * Format: https://cran.r-project.org/doc/manuals/r-release/R-ints.html#Serialization-Formats
 */
export function readRDataFrame(file: string | Buffer): Record<string, Scalar>[] {
    const buffer = typeof file === "string" ? readFileSync(file) : file;
    let offset = 2;
    const references: string[] = [];
    const fail = (message: string): never => {
        throw new Error("Invalid research RDS at byte " + offset + ": " + message);
    };
    function requireBytes(length: number) {
        if (length < 0 || offset + length > buffer.length) fail("truncated data");
    }
    function integer() {
        requireBytes(4);
        const value = buffer.readInt32BE(offset);
        offset += 4;
        return value;
    }
    function length() {
        const value = integer();
        if (value < 0 || value > buffer.length) fail("unsupported vector length");
        return value;
    }
    function pair(value: Value): Pair | null {
        if (value === null) return null;
        if (typeof value !== "object" || !("tag" in value)) fail("invalid attributes");
        return value as Pair;
    }
    function vector(value: Value): Vector {
        if (value === null || typeof value !== "object" || !("values" in value))
            fail("expected a vector");
        return value as Vector;
    }
    function read(): Value {
        const flags = integer();
        const type = flags & 255;
        if (type === 254) return null;
        if (type === 255) {
            const index = flags >>> 8 || integer();
            return references[index - 1] ?? fail("invalid symbol reference");
        }
        if (type === 1) {
            const name = read();
            if (typeof name !== "string") return fail("invalid symbol");
            references.push(name);
            return name;
        }
        if (type === 2) {
            if (flags & 512) fail("unsupported pairlist attributes");
            const tag = flags & 1024 ? read() : null;
            if (tag !== null && typeof tag !== "string") return fail("invalid attribute name");
            const value = read();
            return { tag, value, next: pair(read()) };
        }
        if (type === 9) {
            const size = integer();
            if (size === -1) return null;
            requireBytes(size);
            const value = buffer.toString(
                flags & (4 << 12) ? "latin1" : "utf8",
                offset,
                offset + size,
            );
            offset += size;
            return value;
        }
        if (![10, 13, 14, 16, 19].includes(type)) fail("unsupported object type " + type);
        const count = length();
        requireBytes(count * (type === 14 ? 8 : 4));
        const values: Value[] = Array.from({ length: count }, () => {
            if (type === 10 || type === 13) {
                const value = integer();
                return value === -2147483648 ? null : value;
            }
            if (type === 14) {
                const value = buffer.readDoubleBE(offset);
                offset += 8;
                return Number.isNaN(value) ? null : value;
            }
            return read();
        });
        return { values, attributes: flags & 512 ? pair(read()) : null };
    }
    function attribute(value: Vector, name: string): Value {
        for (let entry = value.attributes; entry; entry = entry.next) {
            if (entry.tag === name) return entry.value;
        }
        return null;
    }
    if (buffer.subarray(0, 2).toString() !== "X\n") fail("expected uncompressed XDR");
    if (integer() !== 2) fail("expected serialization version 2");
    integer(); // Writer version.
    integer(); // Minimum reader version.
    const table = vector(read());
    if (offset !== buffer.length) fail("trailing data");
    const classes = vector(attribute(table, "class")).values;
    if (!classes.includes("data.frame")) fail("expected a data frame");
    const names = vector(attribute(table, "names")).values;
    if (names.length !== table.values.length || names.some((name) => typeof name !== "string"))
        fail("invalid columns");
    if (new Set(names).size !== names.length) fail("duplicate columns");
    const columns = table.values.map((value) => {
        const column = vector(value);
        const levelsValue = attribute(column, "levels");
        const levels = levelsValue === null ? null : vector(levelsValue).values;
        return column.values.map((cell) => {
            if (cell !== null && typeof cell !== "string" && typeof cell !== "number")
                return fail("non-atomic column");
            if (levels && cell !== null) {
                if (
                    typeof cell !== "number" ||
                    !Number.isInteger(cell) ||
                    cell < 1 ||
                    cell > levels.length
                )
                    return fail("invalid factor level");
                const label = levels[cell - 1];
                if (typeof label !== "string") return fail("invalid factor label");
                return label;
            }
            return cell;
        });
    });
    const rowCount = columns[0]?.length ?? 0;
    if (columns.some((column) => column.length !== rowCount)) fail("unequal column lengths");
    return Array.from({ length: rowCount }, (_, row) =>
        Object.fromEntries(names.map((name, col) => [name as string, columns[col][row]])),
    );
}
