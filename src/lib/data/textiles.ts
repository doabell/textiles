import { originalTextileCopy, type OriginalTextileCopy } from "./original-textile-copy";

type TextileMetadata = {
    slug: string;
    image: string;
    accent: "madder" | "indigo" | "saffron" | "moss";
    featured?: boolean;
    dataTerms: string[];
};

export type TextileEntry = TextileMetadata &
    Omit<OriginalTextileCopy, "footnotes"> & {
        shortName: string;
        footnotes: string[];
    };

// Primary images are textile examples from each original glossary page.
// Captions, credits, and contextual images remain in original-textile-media.json.
const textileMetadata: TextileMetadata[] = [
    {
        slug: "chintz-kalamkari",
        accent: "madder",
        featured: true,
        dataTerms: ["chintz/kalamkari"],
        image: "/textile-media/chintz-kalamkari/gallery-01.jpg",
    },
    {
        slug: "dongris",
        accent: "indigo",
        dataTerms: ["dongris"],
        image: "/textile-media/dongris/story-02.png",
    },
    {
        slug: "gingham",
        accent: "saffron",
        featured: true,
        dataTerms: ["gingham"],
        image: "/textile-media/gingham/story-02.jpeg",
    },
    {
        slug: "guinea-cloth",
        accent: "indigo",
        featured: true,
        dataTerms: ["guinea cloth"],
        image: "/textile-media/guinea-cloth/story-02.jpeg",
    },
    {
        slug: "lijnwaad-linen",
        accent: "moss",
        dataTerms: ["lijnwaad"],
        image: "/textile-media/lijnwaad-linen/story-01.jpeg",
    },
    {
        slug: "muslin",
        accent: "saffron",
        featured: true,
        dataTerms: [
            "adathaies",
            "alliballies",
            "bethilles",
            "caffa",
            "camcanys",
            "dimity",
            "douriasten",
            "guldars",
            "hammans",
            "jamdanies",
            "mallemolens",
            "sanen",
            "tanjeebs",
            "therindains",
        ],
        image: "/textile-media/muslin/gallery-03.jpg",
    },
    {
        slug: "negro-cloth",
        accent: "madder",
        dataTerms: ["negro kleden"],
        image: "/textile-media/negro-cloth/story-02.jpg",
    },
    {
        slug: "nickanees",
        accent: "indigo",
        featured: true,
        dataTerms: ["nickanees"],
        image: "/textile-media/nickanees/story-02.jpg",
    },
    {
        slug: "patola",
        accent: "madder",
        dataTerms: ["patola"],
        image: "/textile-media/patola/story-02.jpg",
    },
    {
        slug: "perpetuanen",
        accent: "madder",
        dataTerms: ["perpetuanen"],
        image: "/textile-media/perpetuanen/story-02.jpeg",
    },
    {
        slug: "platillas",
        accent: "moss",
        dataTerms: ["platillas"],
        image: "/textile-media/platillas/story-02.jpeg",
    },
    {
        slug: "sail-cloth",
        accent: "saffron",
        dataTerms: ["sail cloth"],
        image: "/textile-media/sail-cloth/story-01.jpeg",
    },
    {
        slug: "slaaplakens-bed-sheets",
        accent: "moss",
        dataTerms: ["slaaplakens"],
        image: "/textile-media/slaaplakens-bed-sheets/story-02.png",
    },
    {
        slug: "streep",
        accent: "indigo",
        dataTerms: ["streep"],
        image: "/textile-media/streep/story-02.jpeg",
    },
];

export const textiles: TextileEntry[] = textileMetadata.map((entry) => {
    const original = originalTextileCopy[entry.slug];
    return {
        ...entry,
        ...original,
        shortName: original.name,
        footnotes: original.footnotes ?? [],
    };
});

export const featuredTextiles = textiles.filter((textile) => textile.featured);

export function getTextile(slug: string) {
    return textiles.find((textile) => textile.slug === slug);
}
