import assert from "node:assert/strict";
import { createHash } from "node:crypto";
import { readFileSync } from "node:fs";
import { readRDataFrame } from "../src/lib/server/rds.ts";
import { parse } from "csv-parse/sync";
import { getOriginalTradeCsv, getTradeRecords } from "../src/lib/server/trade.ts";

const sources = ["data/week4.rds", "maps/week4.rds", "values/week4.rds"];
const bytes = sources.map((file) => readFileSync(file));
const hashes = bytes.map((buffer) => createHash("sha256").update(buffer).digest("hex"));
assert.equal(new Set(hashes).size, 1, "Both Shiny apps must use the same canonical dataset");
const original = readRDataFrame(bytes[0]);
const records = getTradeRecords();
const exported = parse(getOriginalTradeCsv(), { columns: true });
assert.equal(exported.length, original.length);
assert.deepEqual(Object.keys(exported[0]), Object.keys(original[0]));
for (let index = 0; index < original.length; index++) {
    for (const [column, value] of Object.entries(original[index])) {
        assert.equal(exported[index][column], String(value ?? ""));
    }
}
assert.equal(original.length, 31897);
assert.equal(records.length, original.length, "No original records may be dropped");
assert.equal(records.filter((row) => row.company === "VOC").length, 29901);
assert.equal(records.filter((row) => row.company === "WIC").length, 1996);
for (let index = 0; index < original.length; index++) {
    const row = original[index];
    const record = records[index];
    assert.equal(record.textile, String(row.textile_name ?? "").trim());
    assert.equal(record.source, String(row.source ?? "").trim());
    assert.equal(
        record.quantity,
        Number.isFinite(row.textile_quantity) ? row.textile_quantity : null,
    );
    assert.equal(record.value, Number.isFinite(row.total_value) ? row.total_value : null);
    assert.equal(
        record.pricePerUnit,
        Number.isFinite(row.price_per_unit) ? row.price_per_unit : null,
    );
    assert.equal(record.originalUnit, String(row.textile_unit ?? "").trim());
    const effectiveUnit = ["el", "half ps.", "halve ps."].includes(record.originalUnit)
        ? "ps."
        : record.originalUnit;
    assert.equal(record.unit, effectiveUnit);
    assert.equal(record.priceUnit, effectiveUnit);
}
const guinea = records.find((row) => row.exchangeNumber === "2016930");
assert.equal(guinea?.textile, "guinea cloth");
assert.equal(guinea?.quantity, 720);
assert.equal(guinea?.value, 5125.4, "Indian guilders already converted by clean.R");
assert.equal(guinea?.pricePerUnit, 5125.4 / 720);
assert.equal(records.find((row) => row.exchangeNumber === "2013678")?.value, 4.5);
assert.equal(records[0].value, 1842.25);
assert.equal(records.filter((row) => row.originalUnit === "half ps.").length, 25);
assert.ok(guinea.originLat < 0 && guinea.originLong > 100, "Jakarta must be in Indonesia");
assert.ok(guinea.destinationLat > 50 && guinea.destinationLong > 3 && guinea.destinationLong < 8);
const unmapped = records.filter((row) => row.originLat === null || row.destinationLat === null);
assert.equal(unmapped.length, 6);
assert.ok(
    unmapped.every((row) => row.destination === "Mauritius"),
    "Do not invent missing region coordinates",
);
assert.throws(() => readRDataFrame(bytes[0].subarray(0, 100)), /Invalid research RDS/);
assert.throws(() => readRDataFrame(Buffer.from("invalid")), /Invalid research RDS/);
const wrongVersion = Buffer.from(bytes[0]);
wrongVersion.writeInt32BE(3, 2);
assert.throws(() => readRDataFrame(wrongVersion), /serialization version 2/);
assert.throws(() => readRDataFrame(Buffer.concat([bytes[0], Buffer.from([0])])), /trailing data/);
console.log(
    "Data audit passed: 31,897 original records; source parity, values, units, geography and parser validation.",
);
