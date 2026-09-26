export function exportFilename(
    view: string,
    parts: (string | number | string[])[],
    extension: string,
) {
    const slug = [view, ...parts.flat()]
        .filter((value) => String(value).trim())
        .join("-")
        .normalize("NFKD")
        .replace(/[\u0300-\u036f]/g, "")
        .toLowerCase()
        .replace(/[^a-z0-9]+/g, "-")
        .replace(/^-|-$/g, "")
        .slice(0, 140)
        .replace(/-$/, "");
    return `dutch-textile-trade-${slug}-${new Date().toISOString().slice(0, 10)}.${extension}`;
}
export function downloadBlob(blob: Blob, filename: string) {
    const url = URL.createObjectURL(blob);
    const anchor = document.createElement("a");
    anchor.href = url;
    anchor.download = filename;
    document.body.append(anchor);
    anchor.click();
    anchor.remove();
    setTimeout(() => URL.revokeObjectURL(url), 1000);
}
export type ChartExport = {
    title: string;
    context: string[];
    series: { label: string; color: string }[];
    rows: { label: string; values: number[]; display: string[]; widths?: number[] }[];
    filename: string;
};
export async function downloadChart(chart: ChartExport) {
    await document.fonts.ready;
    const width = 1600,
        pad = 72,
        labelWidth = 320,
        barWidth = width - pad * 2 - labelWidth - 180;
    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d");
    if (!ctx) throw new Error("Canvas unavailable");
    ctx.font = '24px "Instrument Sans Variable", sans-serif';
    const wrap = (text: string, maxWidth: number) => {
        const lines: string[] = [];
        let line = "";
        for (const word of text.split(/\s+/)) {
            if (line && ctx.measureText(line + " " + word).width > maxWidth) {
                lines.push(line);
                line = word;
            } else line += (line ? " " : "") + word;
        }
        if (line) lines.push(line);
        return lines;
    };
    const contextLines = chart.context.flatMap((text) => wrap(text, width - 2 * pad));
    const legendWidth = (width - pad * 2) / Math.max(1, chart.series.length);
    const legendLines = chart.series.map((series) => wrap(series.label, legendWidth - 75));
    const legendTop = 218 + contextLines.length * 34;
    const heading = legendTop + Math.max(1, ...legendLines.map((lines) => lines.length)) * 30 + 15;
    const rowLabels = chart.rows.map((row) => wrap(row.label, labelWidth - 25));
    const rowHeights = rowLabels.map((lines) =>
        Math.max(chart.series.length * 36 + 40, lines.length * 30 + 28),
    );
    canvas.width = width;
    canvas.height = heading + rowHeights.reduce((sum, height) => sum + height, 0) + 85;
    ctx.fillStyle = "#f7f5ef";
    ctx.fillRect(0, 0, width, canvas.height);
    ctx.fillStyle = "#171711";
    ctx.font = '500 24px "Instrument Sans Variable", sans-serif';
    ctx.fillText("Dutch Textile Trade", pad, 58);
    ctx.font = '500 48px "Instrument Sans Variable", sans-serif';
    ctx.fillText(chart.title, pad, 126, width - 2 * pad);
    ctx.font = '24px "Instrument Sans Variable", sans-serif';
    ctx.fillStyle = "#56554f";
    contextLines.forEach((line, i) => ctx.fillText(line, pad, 174 + i * 34));
    chart.series.forEach((series, index) => {
        const legendX = pad + index * legendWidth;
        ctx.fillStyle = series.color;
        ctx.fillRect(legendX, legendTop - 11, 25, 5);
        ctx.fillStyle = "#171711";
        legendLines[index].forEach((line, lineIndex) =>
            ctx.fillText(line, legendX + 37, legendTop + lineIndex * 30),
        );
    });
    const max = Math.max(1, ...chart.rows.flatMap((row) => row.values));
    let rowOffset = 0;
    chart.rows.forEach((row, i) => {
        const y = heading + 28 + rowOffset;
        rowOffset += rowHeights[i];
        ctx.strokeStyle = "#d4d1c9";
        ctx.lineWidth = 1;
        ctx.beginPath();
        ctx.moveTo(pad, y - 8);
        ctx.lineTo(width - pad, y - 8);
        ctx.stroke();
        ctx.fillStyle = "#171711";
        ctx.font = '24px "Instrument Sans Variable", sans-serif';
        rowLabels[i].forEach((line, index) => ctx.fillText(line, pad, y + 27 + index * 30));
        row.values.forEach((value, seriesIndex) => {
            const barY = y + seriesIndex * 36 + 8;
            const fraction = row.widths ? row.widths[seriesIndex] / 100 : value / max;
            ctx.fillStyle = chart.series[seriesIndex].color;
            ctx.fillRect(pad + labelWidth, barY, Math.max(0, fraction * barWidth), 20);
            ctx.fillStyle = "#171711";
            ctx.font = '22px "Instrument Sans Variable", sans-serif';
            ctx.textAlign = "right";
            ctx.fillText(row.display[seriesIndex], width - pad, barY + 20);
            ctx.textAlign = "left";
        });
    });
    ctx.fillStyle = "#56554f";
    ctx.font = '20px "Instrument Sans Variable", sans-serif';
    ctx.fillText(new Date().toISOString().slice(0, 10), pad, canvas.height - 36);
    const blob = await new Promise<Blob>((resolve, reject) =>
        canvas.toBlob(
            (value) => (value ? resolve(value) : reject(new Error("Export unavailable"))),
            "image/png",
        ),
    );
    downloadBlob(blob, chart.filename);
}

export async function downloadMapImage(
    element: HTMLElement,
    options: { title: string; context: string[]; filename: string },
) {
    await document.fonts.ready;
    const bounds = element.getBoundingClientRect();
    const images = [...element.querySelectorAll<HTMLImageElement>(".leaflet-tile-loaded")];
    if (!images.length || images.some((image) => !image.complete || !image.naturalWidth))
        throw new Error("Map unavailable");
    const width = 1600,
        scale = width / bounds.width;
    const height = Math.round(bounds.height * scale);
    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d");
    if (!ctx) throw new Error("Canvas unavailable");
    ctx.font = '22px "Instrument Sans Variable", sans-serif';
    const contextLines = options.context.flatMap((text) => {
        const lines: string[] = [];
        let line = "";
        for (const word of text.split(/\s+/)) {
            if (line && ctx.measureText(line + " " + word).width > width - 120) {
                lines.push(line);
                line = word;
            } else line += (line ? " " : "") + word;
        }
        if (line) lines.push(line);
        return lines;
    });
    const header = 190 + contextLines.length * 32;
    canvas.width = width;
    canvas.height = header + height + 70;
    ctx.fillStyle = "#f7f5ef";
    ctx.fillRect(0, 0, width, canvas.height);
    ctx.fillStyle = "#171711";
    ctx.font = '500 24px "Instrument Sans Variable", sans-serif';
    ctx.fillText("Dutch Textile Trade", 60, 50);
    ctx.font = '500 48px "Instrument Sans Variable", sans-serif';
    ctx.fillText(options.title, 60, 116);
    ctx.font = '22px "Instrument Sans Variable", sans-serif';
    contextLines.forEach((line, i) => ctx.fillText(line, 60, 162 + i * 32));
    ctx.save();
    ctx.beginPath();
    ctx.rect(0, header, width, height);
    ctx.clip();
    ctx.fillStyle = getComputedStyle(element).backgroundColor;
    ctx.fillRect(0, header, width, height);
    for (const image of [...images, ...element.querySelectorAll<HTMLCanvasElement>("canvas")]) {
        const rect = image.getBoundingClientRect();
        ctx.drawImage(
            image,
            Math.round((rect.left - bounds.left) * scale),
            header + Math.round((rect.top - bounds.top) * scale),
            Math.round((rect.right - bounds.left) * scale) -
                Math.round((rect.left - bounds.left) * scale),
            Math.round((rect.bottom - bounds.top) * scale) -
                Math.round((rect.top - bounds.top) * scale),
        );
    }
    ctx.restore();
    ctx.fillStyle = "#56554f";
    ctx.font = '20px "Instrument Sans Variable", sans-serif';
    ctx.fillText("OpenFreeMap · Natural Earth", 60, canvas.height - 26);
    ctx.textAlign = "right";
    ctx.fillText(new Date().toISOString().slice(0, 10), width - 60, canvas.height - 26);
    const blob = await new Promise<Blob>((resolve, reject) =>
        canvas.toBlob(
            (value) => (value ? resolve(value) : reject(new Error("Export unavailable"))),
            "image/png",
        ),
    );
    downloadBlob(blob, options.filename);
}

export async function downloadImageBoard(
    items: { image: string; title: string; details: string[] }[],
    filename: string,
) {
    await document.fonts.ready;
    const images = await Promise.all(
        items.map(async (item) => {
            const image = new Image();
            image.src = item.image;
            await image.decode();
            return image;
        }),
    );
    const canvas = document.createElement("canvas");
    const ctx = canvas.getContext("2d");
    if (!ctx) throw new Error("Canvas unavailable");
    const width = 1600,
        pad = 64,
        gap = 48,
        imageHeight = 820;
    const column = (width - pad * 2 - gap * (items.length - 1)) / items.length;
    ctx.font = '24px "Instrument Sans Variable", sans-serif';
    const wrap = (text: string) => {
        const lines: string[] = [];
        let line = "";
        for (const word of text.split(/\s+/)) {
            if (line && ctx.measureText(line + " " + word).width > column) {
                lines.push(line);
                line = word;
            } else line += (line ? " " : "") + word;
        }
        if (line) lines.push(line);
        return lines;
    };
    const captions = items.map((item) => [item.title, ...item.details].flatMap(wrap));
    canvas.width = width;
    canvas.height = 240 + imageHeight + Math.max(...captions.map((lines) => lines.length)) * 34;
    ctx.fillStyle = "#f7f5ef";
    ctx.fillRect(0, 0, canvas.width, canvas.height);
    ctx.fillStyle = "#171711";
    ctx.font = '500 44px "Instrument Sans Variable", sans-serif';
    ctx.fillText("Swatch Search", pad, 86);
    images.forEach((image, i) => {
        const x = pad + i * (column + gap);
        const scale = Math.min(column / image.naturalWidth, imageHeight / image.naturalHeight);
        const w = image.naturalWidth * scale,
            h = image.naturalHeight * scale;
        ctx.drawImage(image, x + (column - w) / 2, 130 + (imageHeight - h) / 2, w, h);
        ctx.fillStyle = "#171711";
        ctx.font = '24px "Instrument Sans Variable", sans-serif';
        captions[i].forEach((line, lineIndex) => ctx.fillText(line, x, 990 + lineIndex * 34));
    });
    ctx.fillStyle = "#56554f";
    ctx.font = '20px "Instrument Sans Variable", sans-serif';
    ctx.fillText(
        "Dutch Textile Trade · " + new Date().toISOString().slice(0, 10),
        pad,
        canvas.height - 26,
    );
    const blob = await new Promise<Blob>((resolve, reject) =>
        canvas.toBlob(
            (value) => (value ? resolve(value) : reject(new Error("Export unavailable"))),
            "image/png",
        ),
    );
    downloadBlob(blob, filename);
}
