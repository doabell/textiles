import { error } from "@sveltejs/kit";
import { getTextileStats } from "$lib/server/trade";
import { getTextile, textiles } from "$lib/data/textiles";
import type { PageServerLoad } from "./$types";

export function entries() {
    return textiles.map((textile) => ({ slug: textile.slug }));
}

export const load: PageServerLoad = ({ params }) => {
    const textile = getTextile(params.slug);

    if (!textile) {
        error(404, "Textile entry not found");
    }

    const index = textiles.findIndex((entry) => entry.slug === textile.slug);
    const previous = textiles[(index - 1 + textiles.length) % textiles.length];
    const next = textiles[(index + 1) % textiles.length];

    return {
        textile,
        stats: getTextileStats(textile.dataTerms),
        previous: { slug: previous.slug, name: previous.shortName },
        next: { slug: next.slug, name: next.shortName },
    };
};
