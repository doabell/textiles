export type ProjectTool = {
    slug: string;
    title: string;
    eyebrow: string;
    description: string;
    href: string;
    action: string;
    creators: string;
    instructions: string[];
    notes: { title: string; body: string }[];
    accent: "indigo" | "madder" | "saffron";
};

export const projectTools: ProjectTool[] = [
    {
        slug: "trade-explorer",
        title: "Trade Data Explorer",
        eyebrow: "Trade Data Explorer",
        description: "",
        href: "/explore/",
        action: "Explore the records",
        creators: "",
        instructions: [],
        notes: [],
        accent: "madder",
    },
    {
        slug: "swatch-search",
        title: "Swatch Search",
        eyebrow: "Swatch Search",
        description:
            "Explore our growing database of textile samples and swatches by textile name (if you know it) or (if you don’t) search by attributes (modifiers) like color, pattern, process, weave structure, and fiber.",
        href: "/swatches/",
        action: "Search the swatches",
        creators: "Application created by: Drew Donahue",
        instructions: [
            "Searching by textile name: If you know the name of the textile you’re looking for, choose from the ‘Search by textile name’ field. You can refine your search, by selecting attributes (modifiers) from the color, pattern, process, weave structure, and fiber fields.",
            "By hovering over an image, you can see the associated textile name (if known), which you can then explore in the other apps. All unidentified textiles are currently designated “No Name.”",
            "Searching by textile modifiers: If you would like to search by modifiers (such as ‘blue’ or ‘checkered’), rather than by textile name, select ‘All Names’ in the first field and then select any desired modifiers.",
            "Comparing two textiles: Each swatch or sample has a unique textile ID. Use the textile ID to compare two different textiles using the Comparison Tool.",
            "Contributions: Would you like to add swatches or samples to our data set? Please contact us!",
        ],
        notes: [
            {
                title: "A note about modifiers",
                body: "When a textile name is selected, the modifier dropdown lists will only include those modifiers that relate to the selected textile. Choose AND to see results that match all of the selected modifiers. Choose OR to see results that match any of the selected modifiers. You can select more than one modifier in each field.",
            },
            {
                title: "A note about image quality",
                body: "We have tried to include the highest quality, openly available images, but even with good institutional digitization efforts, many of our swatches are small so will appear blurry or pixelated.",
            },
        ],
        accent: "madder",
    },
    {
        slug: "textiles-modifiers-and-values",
        title: "Textiles, Modifiers, and Values",
        eyebrow: "Textiles, Modifiers, and Values",
        description:
            "Explore specific textiles in greater detail, like the quantities, total values, or per-piece values of imported or exported textiles over time or across geographies. Users can compare different types (modifiers) of a given textile.",
        href: "/values/",
        action: "Compare textiles",
        creators:
            "Application created by: Yifei (Bell) Luo, Alec Gong, DJ Poulin, and Will Holzman",
        instructions: [
            "Choose a textile from the dropdown list on the upper left. Select modifier(s) for your selected textile, if any. The bar graph will generate visualizations that reflect your selections. X- and y-axis variables can also be changed.",
        ],
        notes: [
            {
                title: "A note about modifiers",
                body: "The modifier dropdown list will include only those modifiers that relate to the selected textile. Choose OR to see results that match any of the selected modifiers. Choose AND to see results that match all of the selected modifiers. You can select more than one modifier in each field.",
            },
        ],
        accent: "saffron",
    },
    {
        slug: "textile-geographies",
        title: "Textile Geographies",
        eyebrow: "Textile Geographies",
        description:
            "Search the textile data set by a range of archival modifiers—including color, pattern, process, fiber, or quality and visualize this geographically and infographically. Identify specific textile names of interest based on modifiers and geography.",
        href: "/map/",
        action: "Open map",
        creators:
            "Application created by: Yifei (Bell) Luo, Nicholas Sliter, Xingze Wang, Ev Berger-Wolf, Camryn Kluetmeir, Jason Richenbacher",
        instructions: [
            "Choose a trading company (or both), and origin or destination and quantity of textiles or their value at upper left. To generate the pie graph and bar chart, you will need to choose a region (by clicking on the map). Alternatively, you can choose a textile and/or modifiers to view the pie and bar chart in addition to the world map. The charts can be further refined by modifier. To view the relevant data for your search, click on the Data Table tab at the top. As you refine your search, extraneous data is filtered out, and to start over with the full data set, select Reset All in the upper left.",
            "The textile(s) of interest field can be left blank and as modifiers are chosen, the infographics provide names of textiles that fit those parameters. With multiple modifiers, the app searches for textiles that match all the selected modifiers, and when the result is no data, the pie and bar charts will disappear. If you’ve identified a specific textile you’re interested in, you may want to explore that textile in the Textiles, Modifiers, and Values app.",
        ],
        notes: [
            {
                title: "A note about modifiers",
                body: "These fields refer to how the shipped textiles are described archivally. So ‘geography of interest’ does not include all textiles originating from a specific region, but rather how that textile is listed in cargo manifests. For example a textile described as ‘Coromandel’ may ship from Batavia to the Dutch Republic; and not all textiles originating in the Coromandel Coast are described as such.",
            },
        ],
        accent: "indigo",
    },
];

export function getProjectTool(slug: string) {
    return projectTools.find((tool) => tool.slug === slug);
}
