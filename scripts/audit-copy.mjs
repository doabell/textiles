import { readFile, readdir } from "node:fs/promises";
import path from "node:path";
import process from "node:process";
import { parse } from "svelte/compiler";
import { projectTools } from "../src/lib/data/projects.ts";

const root = process.cwd();
const sourceRoots = ["maps", "values", "pictures"];
const sourceFiles = [
    "src/lib/data/original-page-copy.ts",
    "src/lib/data/original-textile-copy.ts",
    "src/lib/data/original-textile-media.json",
];
const visibleElements = new Set([
    "a",
    "label",
    "span",
    "small",
    "strong",
    "blockquote",
    "button",
    "dd",
    "dt",
    "figcaption",
    "h1",
    "h2",
    "h3",
    "h4",
    "h5",
    "h6",
    "legend",
    "li",
    "option",
    "p",
    "summary",
    "th",
]);
const copyAttributes = new Set(["alt", "aria-label", "content", "placeholder", "title"]);

async function filesBelow(directory, accept) {
    const entries = await readdir(path.join(root, directory), { withFileTypes: true });
    const files = [];

    for (const entry of entries) {
        const relative = path.join(directory, entry.name);
        if (entry.isDirectory()) files.push(...(await filesBelow(relative, accept)));
        else if (accept(relative)) files.push(relative);
    }

    return files;
}

function decodeEntities(value) {
    return value
        .replace(/&#x([0-9a-f]+);/gi, (_, code) => String.fromCodePoint(Number.parseInt(code, 16)))
        .replace(/&#(\d+);/g, (_, code) => String.fromCodePoint(Number.parseInt(code, 10)))
        .replaceAll("&nbsp;", " ")
        .replaceAll("&amp;", "&")
        .replaceAll("&quot;", '"')
        .replaceAll("&apos;", "'")
        .replaceAll("&lt;", "<")
        .replaceAll("&gt;", ">");
}

function normalize(value) {
    return decodeEntities(value)
        .replace(/<[^>]*>/g, " ")
        .replaceAll("\\n", " ")
        .replaceAll("\\t", " ")
        .replace(/[“”]/g, '"')
        .replace(/[‘’]/g, "'")
        .replace(/\s+/g, " ")
        .replace(/([([])\s+/g, "$1")
        .replace(/\s+([)\]])/g, "$1")
        .replace(/\s+([,.;:!?])/g, "$1")
        .trim()
        .toLocaleLowerCase("en");
}

function wordCount(value) {
    return value.match(/[\p{L}\p{N}]+(?:[’'/-][\p{L}\p{N}]+)*/gu)?.length ?? 0;
}

function expressionText(expression) {
    if (!expression || typeof expression !== "object") return "";
    if (expression.type === "Literal" && typeof expression.value === "string")
        return expression.value;
    if (expression.type === "TemplateLiteral") {
        return expression.quasis
            .map((part) => part.value?.cooked ?? part.value?.raw ?? "")
            .join(" ");
    }
    return "";
}

function expressionCopy(expression) {
    if (!expression || typeof expression !== "object") return [];
    if (expression.type === "ConditionalExpression") {
        return [...expressionCopy(expression.consequent), ...expressionCopy(expression.alternate)];
    }
    if (expression.type === "LogicalExpression") {
        return [...expressionCopy(expression.left), ...expressionCopy(expression.right)];
    }
    const text = expressionText(expression);
    return text ? [text] : [];
}

function staticText(node) {
    if (!node || typeof node !== "object") return "";
    if (node.type === "Text") return node.data ?? node.raw ?? "";
    if (node.type === "ExpressionTag") return expressionText(node.expression);
    if (Array.isArray(node)) return node.map(staticText).join(" ");
    if (node.fragment?.nodes) return staticText(node.fragment.nodes);
    return "";
}

function lineAt(source, offset) {
    return source.slice(0, offset).split("\n").length;
}

function collectMarkupCopy(source, filename) {
    const ast = parse(source, { filename, modern: true });
    const candidates = [];

    function add(value, node, kind) {
        const text = value.replace(/\s+/g, " ").trim();
        if (!text || !/[\p{L}\p{N}]/u.test(text)) return;
        if (/^[^\s]+\.(?:csv|xlsx?|pdf|zip)$/i.test(text)) return;
        candidates.push({ file: filename, line: lineAt(source, node.start ?? 0), kind, text });
    }

    function walk(node) {
        if (!node || typeof node !== "object") return;
        if (Array.isArray(node)) {
            node.forEach(walk);
            return;
        }

        if (node.type === "ExpressionTag") {
            for (const text of expressionCopy(node.expression)) add(text, node, "expression");
        }

        if (node.type === "RegularElement") {
            const nestedCopy = (node.fragment?.nodes ?? []).some(
                (child) =>
                    child.type === "RegularElement" &&
                    (visibleElements.has(child.name) || ["div", "select"].includes(child.name)),
            );
            const wrapper = ["a", "label", "span", "small", "strong"].includes(node.name);
            if (visibleElements.has(node.name) && !(wrapper && nestedCopy)) {
                add(staticText(node), node, node.name);
            }

            for (const attribute of node.attributes ?? []) {
                if (attribute.type === "Attribute" && copyAttributes.has(attribute.name)) {
                    add(staticText(attribute.value), attribute, attribute.name);
                    for (const part of Array.isArray(attribute.value)
                        ? attribute.value
                        : [attribute.value]) {
                        if (part?.type === "ExpressionTag") {
                            for (const text of expressionCopy(part.expression))
                                add(text, attribute, attribute.name);
                        }
                    }
                }
            }
        }

        if (node.fragment?.nodes) walk(node.fragment.nodes);
        for (const key of [
            "alternate",
            "body",
            "catch",
            "consequent",
            "fallback",
            "pending",
            "then",
        ]) {
            const branch = node[key];
            if (branch?.nodes) walk(branch.nodes);
            else if (branch && typeof branch === "object") walk(branch);
        }
    }

    walk(ast.fragment?.nodes ?? []);

    const visited = new WeakSet();
    function walkScript(node, parent) {
        if (!node || typeof node !== "object" || visited.has(node)) return;
        visited.add(node);

        const selectorArgument =
            parent?.type === "CallExpression" &&
            parent.arguments?.[0] === node &&
            parent.callee?.type === "MemberExpression" &&
            !parent.callee.computed &&
            ["querySelector", "querySelectorAll", "matches", "closest"].includes(
                parent.callee.property?.name,
            );
        if (selectorArgument) return;

        if (node.type === "Literal" && typeof node.value === "string") {
            // Imports, URLs, and MIME types are code, not interface copy.
            if (!/^(?:https?:\/\/|mailto:|data:|\.{0,2}\/|@|text\/|image\/)/i.test(node.value)) {
                add(node.value, node, "script");
            }
        } else if (node.type === "TemplateElement") {
            const value = node.value?.cooked ?? node.value?.raw ?? "";
            if (!/^(?:https?:\/\/|mailto:|data:|\.{0,2}\/|@)/i.test(value)) {
                add(value, node, "script");
            }
        }

        for (const [key, value] of Object.entries(node)) {
            if (["loc", "start", "end"].includes(key)) continue;
            if (Array.isArray(value)) value.forEach((child) => walkScript(child, node));
            else if (value && typeof value === "object") walkScript(value, node);
        }
    }

    walkScript(ast.instance?.content);
    walkScript(ast.module?.content);
    return candidates;
}

const rSources = (
    await Promise.all(
        sourceRoots.map((directory) => filesBelow(directory, (file) => /\.[Rr]$/.test(file))),
    )
).flat();
const corpusSource = (
    await Promise.all(
        [...sourceFiles, ...rSources].map((file) => readFile(path.join(root, file), "utf8")),
    )
).join("\n");
// These three entries retain the original website’s app documentation.
// Port-specific controls and the added explorer are deliberately excluded.
const originalProjects = projectTools
    .filter((tool) =>
        ["swatch-search", "textile-geographies", "textiles-modifiers-and-values"].includes(
            tool.slug,
        ),
    )
    .flatMap((tool) => [
        tool.title,
        tool.description,
        tool.creators,
        ...tool.instructions,
        ...tool.notes.flatMap((note) => [note.title, note.body]),
    ])
    .join("\n");
const corpus = normalize(
    `${corpusSource}\n${corpusSource.replaceAll("[insert date]", "")}\n${originalProjects}`,
);
const svelteFiles = await filesBelow("src", (file) => file.endsWith(".svelte"));
const candidates = (
    await Promise.all(
        svelteFiles.map(async (file) =>
            collectMarkupCopy(await readFile(path.join(root, file), "utf8"), file),
        ),
    )
).flat();
// The original footer supplies its year, citation URL, and access date dynamically.
// Keep this exact attribution exception local to the footer, not in the general corpus.
const originalFooterCopy = new Set(
    [
        "Copyright © Dutch Textile Trade",
        "Carrie Anderson and Marsely Kehoe. The Dutch Textile Trade Project.",
        "This work is licensed under a Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License.",
        // The same original license name is now its own link inside the sentence.
        "Creative Commons Attribution-NonCommercial-ShareAlike 4.0 International License",
    ].map(normalize),
);

// Original research copy remains verbatim. Added interface copy has a three-word limit.
const failures = candidates.filter((candidate) => {
    const normalized = normalize(candidate.text);
    const originalFooter =
        candidate.file.replaceAll("\\", "/") === "src/lib/components/SiteFooter.svelte" &&
        originalFooterCopy.has(normalized);
    return wordCount(normalized) > 3 && !corpus.includes(normalized) && !originalFooter;
});
const functionalKinds = new Set([
    "aria-label",
    "button",
    "legend",
    "option",
    "placeholder",
    "summary",
]);
const functionalFailures = candidates.filter(
    (candidate) => functionalKinds.has(candidate.kind) && wordCount(normalize(candidate.text)) > 3,
);

if (failures.length || functionalFailures.length) {
    if (functionalFailures.length) {
        console.error("Functional copy over 3 words:\n");
        for (const failure of functionalFailures) {
            console.error(`${failure.file}:${failure.line} [${failure.kind}] ${failure.text}`);
        }
        console.error("");
    }

    if (!failures.length) process.exitCode = 1;
}

if (failures.length) {
    console.error("Visible copy over 3 words without an original-site/app match:\n");
    for (const failure of failures) {
        console.error(`${failure.file}:${failure.line} [${failure.kind}] ${failure.text}`);
    }
    process.exitCode = 1;
} else if (!functionalFailures.length) {
    console.log(`Copy audit passed for ${candidates.length} visible strings.`);
}
