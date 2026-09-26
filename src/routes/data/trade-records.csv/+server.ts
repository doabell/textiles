import { getOriginalTradeCsv } from "$lib/server/trade";

export const prerender = true;
export const trailingSlash = "never";

export function GET() {
    return new Response(getOriginalTradeCsv(), {
        headers: {
            "Content-Type": "text/csv; charset=utf-8",
            "Content-Disposition": 'attachment; filename="DutchTextileTrade.csv"',
        },
    });
}
