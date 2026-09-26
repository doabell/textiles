const localAssets: Record<string, string> = {
    "/wp-content/uploads/2023/03/SRC_Primary.xlsx": "/SRC_Primary.xlsx",
};

export function localHref(href: string): string {
    const path = href.replace(/^(?:https?:)?\/\/(?:www\.)?dutchtextiletrade\.org(?=\/|$)/i, "");
    if (/^https?:\/\/dutchtextiletradeapps\.shinyapps\.io\/maps\/?/i.test(path)) return "/map/";
    if (/^https?:\/\/dutchtextiletradeapps\.shinyapps\.io\/values\/?/i.test(path))
        return "/values/";
    return localAssets[path] ?? (path || "/");
}

export function localizeHtml(html: string): string {
    return html.replace(
        /href=(["'])(.*?)\1/g,
        (_match, quote, href) => `href=${quote}${localHref(href)}${quote}`,
    );
}
