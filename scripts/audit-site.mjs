import { readdir, readFile, stat } from "node:fs/promises";
import path from "node:path";

const root = path.resolve("build");
const failures = [];
let pages = 0,
    localLinks = 0,
    externalLinks = 0,
    noteLinks = 0;
const exists = async (file) => {
    try {
        return (await stat(file)).isFile();
    } catch {
        return false;
    }
};
async function walk(folder) {
    const entries = await readdir(folder, { withFileTypes: true });
    for (const entry of entries) {
        const file = path.join(folder, entry.name);
        if (entry.isDirectory()) await walk(file);
        else if (entry.name.endsWith(".html")) await auditPage(file);
    }
}
async function auditPage(file) {
    pages++;
    const relative = path.relative(root, file).replaceAll("\\", "/");
    const pagePath = "/" + relative.replace(/index\.html$/, "");
    const html = (await readFile(file, "utf8"))
        .replace(/<!--[\s\S]*?-->/g, "")
        .replace(/(<script\b[^>]*>)[\s\S]*?<\/script>/gi, "$1</script>");
    const ids = new Set([...html.matchAll(/\bid=["\']([^"\']+)["\']/g)].map((match) => match[1]));
    const tags = html.match(/<(?:a|link|img|source|script|iframe|form)\b[^>]*>/gi) || [];
    for (const tag of tags) {
        const attributes = [...tag.matchAll(/\b(?:href|src|action)\s*=\s*["']([^"']*)["']/gi)];
        for (const [, raw] of attributes) {
            const value = raw.replaceAll("&amp;", "&");
            if (/^#(?:note|ref)-/.test(value)) {
                noteLinks++;
                if (!ids.has(value.slice(1)))
                    failures.push(relative + ": missing note anchor " + value);
            }
            if (!value || value.startsWith("#") || /^(data|mailto|tel|blob):/i.test(value))
                continue;
            let url;
            try {
                url = new URL(value, "https://local.invalid" + pagePath);
            } catch {
                failures.push(relative + ": invalid URL " + value);
                continue;
            }
            if (
                /^(www\.)?dutchtextiletrade\.org$/i.test(url.hostname) ||
                url.hostname === "dutchtextiletradeapps.shinyapps.io"
            ) {
                failures.push(relative + ": legacy destination " + value);
                continue;
            }
            if (url.hostname !== "local.invalid") {
                externalLinks++;
                continue;
            }
            localLinks++;
            const target = path.resolve(root, "." + decodeURIComponent(url.pathname));
            if (!target.startsWith(root + path.sep) && target !== root) {
                failures.push(relative + ": path escapes build " + value);
                continue;
            }
            if (!(await exists(target)) && !(await exists(path.join(target, "index.html"))))
                failures.push(relative + ": missing local destination " + value);
        }
    }
}
try {
    await walk(root);
    const workbook = await readFile(path.join(root, "SRC_Primary.xlsx"));
    if (workbook.readUInt32LE(0) !== 0x04034b50)
        failures.push("Primary sources workbook is not XLSX.");
} catch (error) {
    failures.push(error.message + " — run bun run build first.");
}
if (failures.length) {
    console.error([...new Set(failures)].join("\n"));
    process.exitCode = 1;
} else {
    console.log(
        `Site audit passed: ${pages} pages, ${localLinks} local references, ${externalLinks} permitted external references, ${noteLinks} note links. No legacy destinations.`,
    );
}
