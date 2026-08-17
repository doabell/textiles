import chintzImage from "../../../pictures/img/yshu6r3.jpg?url";
import dongrisImage from "../../../pictures/img/vy1Y0IU.jpg?url";
import ginghamImage from "../../../pictures/img/jsuXIBU.jpg?url";
import guineaImage from "../../../pictures/img/pIMXYre.jpg?url";
import linenImage from "../../../pictures/img/g661XYa.jpg?url";
import muslinImage from "../../../pictures/img/APyO1l0.jpg?url";
import negroClothImage from "../../../pictures/img/wuUqOIG.jpg?url";
import nickaneesImage from "../../../pictures/img/ttSsz9n.jpg?url";
import patolaImage from "../../../pictures/img/m2LKEP8.jpg?url";
import perpetuanenImage from "../../../pictures/img/nitmHp9.jpg?url";
import platillasImage from "../../../pictures/img/WunLu3R.jpg?url";
import sailClothImage from "../../../pictures/img/BMKcKqB.jpg?url";
import slaaplakensImage from "../../../pictures/img/QwjXL0R.jpg?url";
import streepImage from "../../../pictures/img/sM6V9Fo.jpg?url";
import { originalTextileCopy } from "./original-textile-copy";

export type TextileEntry = {
    slug: string;
    name: string;
    shortName: string;
    definition: string;
    variants: string[];
    related: string[];
    aat?: string;
    fiber: string;
    technique: string;
    origin: string;
    network: "VOC" | "WIC" | "VOC & WIC";
    image: string;
    imageAlt: string;
    imageCaption: string;
    imageCredit: string;
    catalogueUrl?: string;
    accent: "madder" | "indigo" | "saffron" | "moss";
    featured?: boolean;
    dataTerms: string[];
    essay: string[];
    relatedDescription?: string;
    footnotes?: string[];
    sourceUrl?: string;
};

const textileMetadata: TextileEntry[] = [
    {
        slug: "chintz-kalamkari",
        name: "Chintz / Kalamkari",
        shortName: "Chintz",
        definition:
            "An Indian patterned textile, usually cotton. The pattern is painted and/or printed in multiple stages of dyeing, mordanting, and resist-dyeing. It was later imitated with industrial processes.",
        variants: ["chintes", "tschijndes", "chint", "chitz", "chins", "tsjints", "sits", "tjita"],
        related: ["palampore", "percallen", "salempores", "kalamkari", "batik"],
        aat: "300132876",
        fiber: "Cotton",
        technique: "Painted, block printed, resist-dyed",
        origin: "Indian subcontinent",
        network: "VOC & WIC",
        image: chintzImage,
        imageAlt: "Floral chintz cotton with red, blue, and green flowers on a pale ground",
        imageCaption: "Chintz, block printed and painted cotton, eighteenth century.",
        imageCredit: "Victoria and Albert Museum, IS.54-1950",
        catalogueUrl: "https://collections.vam.ac.uk/item/O73115/",
        accent: "madder",
        featured: true,
        dataTerms: ["chintz", "chintes", "sits"],
        essay: [
            "Chintz is not a single pattern but a family of cottons made through exacting sequences of drawing, mordanting, dyeing, washing, and resisting. The durable reds and blues that made these cloths so desirable depended on highly specialized knowledge held by Indian makers.",
            "Dutch records flatten this technical and artistic range into unstable spellings and cargo categories. Read alongside surviving cloth, however, the trade data reveals a product continually adapted for markets in Southeast Asia, Europe, and the Atlantic world.",
            "The project uses “chintz” as a legible umbrella term while retaining historical variants. Kalamkari refers more specifically to cloth whose design was drawn or painted by hand.",
        ],
    },
    {
        slug: "dongris",
        name: "Dongris",
        shortName: "Dongris",
        definition:
            "A sturdy, utilitarian cotton cloth, either natural in color or bleached white. It was exported from India throughout Asia, to Europe, and to the Cape of Good Hope.",
        variants: ["dongry", "dongerijs", "dungary", "dungaree", "dongrijs", "poutkas"],
        related: ["sail cloth", "pautkas", "bafta"],
        aat: "300451339",
        fiber: "Cotton",
        technique: "Plain weave",
        origin: "India",
        network: "VOC",
        image: dongrisImage,
        imageAlt: "White and blue checked cotton swatch mounted on an archival page",
        imageCaption:
            "A bafta swatch from the same utilitarian cotton archive used for comparison.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 5",
        accent: "indigo",
        dataTerms: ["dongris", "dongrijs", "dongry"],
        essay: [
            "Dongris appear in VOC cargo lists as practical cottons rather than luxury goods. Surviving descriptions point to cloth that was commonly bleached, left raw, or finished with a colored border.",
            "Its usefulness made dongri easy to overlook: it could wrap gifts and cargo, become clothing, or serve alongside heavier canvas aboard ship. The modern word “dungaree” preserves part of this history.",
            "In the current project data, dongris belong to the VOC network and do not appear in WIC records.",
        ],
    },
    {
        slug: "gingham",
        name: "Gingham",
        shortName: "Gingham",
        definition:
            "An Indian textile made in multiple woven patterns, colors, and fiber types. Some ginghams had a textured surface made through doubled threads or mixed fibers.",
        variants: ["gingam", "ginghgangh", "ginggang", "Bombay stuffs"],
        related: ["taffachelas", "dronggangs", "pinasse", "sestienes", "izarees"],
        aat: "300451340",
        fiber: "Cotton and mixed fibers",
        technique: "Loom-patterned, sometimes ikat",
        origin: "India and Southeast Asia",
        network: "VOC & WIC",
        image: ginghamImage,
        imageAlt: "Narrow striped gingham swatch in muted red, indigo, and cream",
        imageCaption: "Gingham swatch carried to the Guinea Coast, 1788.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 19",
        accent: "saffron",
        featured: true,
        dataTerms: ["gingam", "gingham", "ginggang"],
        essay: [
            "Today gingham usually means an evenly checked cloth in two colors. Early modern cargo records describe something much less fixed: striped, checked, plain, and multicolored textiles with different weights and fiber combinations.",
            "The word traveled with the textile. Its shifting spellings and qualifiers record exchange between South and Southeast Asian producers, company clerks, and consumers across the Dutch trading world.",
            "Treating all gingham as the modern check would erase that variety. The glossary therefore pairs a broad definition with the modifiers attached to each archival shipment.",
        ],
    },
    {
        slug: "guinea-cloth",
        name: "Guinea Cloth",
        shortName: "Guinea Cloth",
        definition:
            "A category of Indian-made cotton cloths that were typically blue, white, blue-and-white striped, or checked. Guinea cloth circulated widely in Asia, Africa, and Europe.",
        variants: ["guinees", "guinea-stoff", "blue salempores"],
        related: ["corroots", "bajutapauts", "nickanees", "cambaay", "negroskleed"],
        aat: "300451341",
        fiber: "Cotton",
        technique: "Loom-patterned",
        origin: "India; named through Atlantic trade",
        network: "VOC & WIC",
        image: guineaImage,
        imageAlt: "Blue and white checked corrott cotton swatch on an archival page",
        imageCaption: "Corrott, a related blue-and-white cotton shipped to West Africa.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 7",
        accent: "indigo",
        featured: true,
        dataTerms: ["guinees", "guinea cloth", "guinees cloth"],
        essay: [
            "“Guinea cloth” is a market category rather than a place of manufacture. The textiles were made in India, while their name records the importance of West African demand within Dutch commercial systems.",
            "Blue-and-white cloth was never one uniform product. Guinea cloth sat among many related cottons that differed in stripe, check, density, quality, and price. Those distinctions mattered to local buyers even when European records obscured them.",
            "Its circulation is inseparable from the transatlantic slave trade. WIC invoices positioned cloth alongside other commodities used to purchase enslaved people, a context the project makes explicit rather than treating the records as neutral.",
        ],
    },
    {
        slug: "lijnwaad-linen",
        name: "Lijnwaad (linen)",
        shortName: "Lijnwaad",
        definition:
            "A category of European-made linens that could be bleached, unbleached, half-bleached, colored, striped, checked, or floral, often identified by their place of production.",
        variants: ["lijnwaad", "lijwaet", "lywaeten", "lijn", "linnen", "Holland cloth"],
        related: ["platillas", "slaaplakens", "servietten", "boekjes", "streep"],
        fiber: "Linen",
        technique: "Plain or loom-patterned weave",
        origin: "The Dutch Republic and Europe",
        network: "VOC & WIC",
        image: linenImage,
        imageAlt: "Blue and white checked rumal swatch mounted in an archival sample book",
        imageCaption:
            "A woven swatch from the project material archive, shown for structural comparison.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 10",
        accent: "moss",
        dataTerms: ["lijnwaad", "lijwaet", "linnen", "linen"],
        essay: [
            "Lijnwaad is both useful and slippery. In many records it refers to linen, but it can also operate as a generic word for cloth. Place names, qualities, colors, and measurements are therefore essential to interpretation.",
            "European linens moved to West Africa and the Americas in large quantities. Their plain appearance should not be mistaken for a simple history: grades, bleaching, finishing, and regional reputation all shaped value.",
            "The project retains the Dutch term because translating every instance as “linen” would imply a certainty that the archive does not always provide.",
        ],
    },
    {
        slug: "muslin",
        name: "Muslin",
        shortName: "Muslin",
        definition:
            "A lightweight, semi-transparent, finely woven cotton cloth, generally white and often embroidered or patterned with supplemental threads.",
        variants: ["muslin", "mousseline", "malmal", "mallemolens"],
        related: ["adathaies", "bethilles", "caffa", "dimity", "jamdanies", "tanjeebs"],
        aat: "300014087",
        fiber: "Cotton",
        technique: "Fine plain weave, often embroidered",
        origin: "Indian subcontinent",
        network: "VOC",
        image: muslinImage,
        imageAlt: "White embroidered muslin jama with a repeating geometric motif",
        imageCaption: "Muslin jama, India, eighteenth century.",
        imageCredit: "Victoria and Albert Museum, IS.8-1968",
        catalogueUrl: "https://collections.vam.ac.uk/item/O144142/",
        accent: "saffron",
        featured: true,
        dataTerms: ["malmal", "mallemolens", "bethilles", "caffa", "dimity", "jamdanies"],
        essay: [
            "“Muslin” is a modern umbrella for many cloths that company documents recorded under more specific names. Those terms could signal a place, quality, weave, finish, or local commercial category.",
            "The finest examples are almost weightless, their weave visible in the way the body or lining shows through. Embroidery and supplemental threads could turn that transparency into a field of ornament.",
            "Grouping the terms helps readers navigate the archive, while the related-textile list keeps the historical distinctions visible.",
        ],
    },
    {
        slug: "negro-cloth",
        name: "“Negro cloth”",
        shortName: "“Negro cloth”",
        definition:
            "A historical trade category applied to inexpensive cloth intended for enslaved and laboring people. The term is reproduced only to make colonial records searchable and must be read critically.",
        variants: ["negroskleed", "negro cloth", "negerkleed"],
        related: ["guinea cloth", "neganipauts", "osnaburg", "slave cloth"],
        fiber: "Usually cotton or linen",
        technique: "Plain or loom-patterned weave",
        origin: "A colonial market category",
        network: "VOC & WIC",
        image: negroClothImage,
        imageAlt: "Blue and white checked neganipauts swatch mounted on an archival page",
        imageCaption:
            "Neganipauts, one of the inexpensive cottons associated with colonial labor markets.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 16",
        accent: "madder",
        dataTerms: ["negroskleed", "negro cloth", "negerkleed"],
        essay: [
            "Company clerks categorized textiles by intended market and use. In this case, the label reduced people to a commercial class and treated clothing for enslaved laborers as another cost to be minimized.",
            "The category did not describe one stable textile. It could encompass coarse cotton or linen cloths sourced through different trading networks. Quality, durability, and price mattered more to officials than makers or wearers.",
            "Keeping the term visible—with context—allows researchers to find and challenge the structures embedded in the records.",
        ],
    },
    {
        slug: "nickanees",
        name: "Nickanees",
        shortName: "Nickanees",
        definition:
            "An inexpensive blue-and-white striped, loom-patterned cotton produced primarily in Gujarat and shipped to ports in South and Southeast Asia and Africa.",
        variants: ["niquanias", "nekanees", "nicanees", "necanias", "neckjes"],
        related: ["bajutapauts", "corroots", "guinea cloth"],
        aat: "300451342",
        fiber: "Cotton",
        technique: "Loom-patterned stripes",
        origin: "Gujarat, India",
        network: "VOC & WIC",
        image: nickaneesImage,
        imageAlt: "Indigo and white striped nickanees swatch mounted in an archival book",
        imageCaption: "Nickanees swatch carried to the Guinea Coast, 1788.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 18",
        accent: "indigo",
        featured: true,
        dataTerms: ["nicanees", "nickanees", "necanias", "niquanias"],
        essay: [
            "Nickanees are unusually recognizable within an archive full of unstable names. Surviving samples repeatedly show alternating blue and white stripes, sometimes arranged in bands of different widths.",
            "They circulated through both VOC and WIC systems and were issued as clothing to enslaved and imprisoned people in Batavia. That institutional use is part of the textile’s history, not an incidental detail.",
            "The consistency of the surviving pattern helps connect terse cargo entries to a material object.",
        ],
    },
    {
        slug: "patola",
        name: "Patola",
        shortName: "Patola",
        definition:
            "A prestigious resist-dyed silk textile from Gujarat, most characteristically made with double ikat: both warp and weft threads are patterned before they are woven.",
        variants: ["patolu", "patole", "patolen", "cinde", "tjinde"],
        related: ["ikat", "geringsing", "chinde"],
        fiber: "Silk",
        technique: "Double ikat",
        origin: "Gujarat, India",
        network: "VOC",
        image: patolaImage,
        imageAlt: "Indigo striped ikat swatch mounted on an archival page",
        imageCaption: "An ikat-woven gerras swatch from the project material archive.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 17",
        accent: "madder",
        dataTerms: ["patola", "patolen", "chinde", "tjinde"],
        essay: [
            "Patola required extraordinary planning. The pattern was bound and dyed into both sets of threads before weaving, so the design emerged only when warp and weft met on the loom.",
            "These silks traveled widely across the Indian Ocean, where they could function as elite dress, heirloom, ceremonial cloth, or diplomatic gift. Their meanings were produced by the communities that acquired and used them, not by European merchants alone.",
            "Historical names overlap with words used for other patterned textiles, especially chinde or tjinde, making visual and geographic evidence crucial.",
        ],
    },
    {
        slug: "perpetuanen",
        name: "Perpetuanen",
        shortName: "Perpetuanen",
        definition:
            "A hot-pressed, durable serge woven from wool in England, Flanders, and Holland, typically dyed a single color and traded primarily in Africa.",
        variants: ["peroupaets", "perpeta", "perpets", "perpetuanoes", "petuna", "perpetuaan"],
        related: ["serge", "saai", "kersey", "drogetten", "serges imperials"],
        aat: "300451343",
        fiber: "Wool",
        technique: "Twill weave, hot pressed",
        origin: "England, Flanders, and Holland",
        network: "WIC",
        image: perpetuanenImage,
        imageAlt: "Deep red floral silk velvet textile",
        imageCaption:
            "A red velvet in the visual reference archive, shown for color and surface comparison.",
        imageCredit: "Victoria and Albert Museum, 281:1 to 3-1893",
        catalogueUrl: "https://collections.vam.ac.uk/item/O147464/",
        accent: "madder",
        dataTerms: ["perpetuanen", "perpetuaan", "perpets"],
        essay: [
            "Perpetuanen took their name from durability. Closely woven and pressed, the wool held saturated color and endured handling better than many lighter fabrics.",
            "Blue and green dominate the project records, with red, purple, yellow, brown, and undyed cloth also appearing. Color and finish shaped how the fabric operated as dress, status marker, and trade good.",
            "Most documented shipments moved through the Atlantic network, where woolens joined linen and Indian cotton in exchanges on the West African coast.",
        ],
    },
    {
        slug: "platillas",
        name: "Platillas",
        shortName: "Platillas",
        definition:
            "A fine bleached linen associated with Silesia and perhaps also Hamburg, Flanders, and Germany. Platillas were often shipped by the schock, a unit of four pieces.",
        variants: ["plathilios", "platilhas", "platillos"],
        related: ["silesias", "slaaplakens", "lijnwaad", "lhymenias"],
        aat: "300451344",
        fiber: "Linen",
        technique: "Fine plain weave, bleached",
        origin: "Central and Northern Europe",
        network: "WIC",
        image: platillasImage,
        imageAlt: "Small pale platillas textile sample attached to an archival page",
        imageCaption: "Platillas sample carried on De Vrouwe Maria Geertruida, 1788.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 22",
        accent: "moss",
        dataTerms: ["platillas", "platillos", "plathilios"],
        essay: [
            "Platillas were valued as fine, light, bleached linens. Records often identify them through European regions of production while tracking their movement into West African commerce.",
            "One WIC invoice equated a quantity of platillas with enslaved human lives. That brutal arithmetic demonstrates why value data from colonial archives cannot be presented as abstract economics.",
            "Comparing platillas with broader linen categories helps distinguish a named grade from the many other European cloths traveling the same routes.",
        ],
    },
    {
        slug: "sail-cloth",
        name: "Sail Cloth",
        shortName: "Sail Cloth",
        definition:
            "A durable plain cloth of varying fibers and weights, used for sails, clothing, hammocks, and other purposes. European and Asian sail cloth circulated throughout the VOC network.",
        variants: ["sailcloth", "zeildoek", "zeyldoek"],
        related: ["dongris", "everdoek", "henepdoek", "zeilkleden"],
        aat: "300014079",
        fiber: "Hemp, flax, cotton, or mixed fibers",
        technique: "Dense plain weave",
        origin: "Europe and Asia",
        network: "VOC",
        image: sailClothImage,
        imageAlt: "Eighteenth-century floral printed and resist-dyed cotton length",
        imageCaption: "An eighteenth-century cotton length from the material archive.",
        imageCredit: "Victoria and Albert Museum, IS.102-1948",
        catalogueUrl: "https://collections.vam.ac.uk/item/O455712/",
        accent: "saffron",
        dataTerms: ["sail cloth", "sailcloth", "zeildoek"],
        essay: [
            "Sail cloth was defined by performance as much as fiber. Weight, density, and finish varied with a sail’s position and with the material available in a particular port.",
            "Company records also place the cloth away from ships: in hammocks, work clothing, and other durable objects. “Sail clothing” can mean ready-made garments for sailors and should not automatically be read as clothing made from sail cloth.",
            "The textile is common in VOC records and notably rare in the WIC material currently assembled by the project.",
        ],
    },
    {
        slug: "slaaplakens-bed-sheets",
        name: "Slaaplakens (bed sheets)",
        shortName: "Slaaplakens",
        definition:
            "Linen bed sheets that became popular trade items at West African posts. Sources sometimes describe them as “old,” meaning second-hand, and identify Dutch sheets as especially desirable.",
        variants: ["slaplagen", "sheets", "bed sheets"],
        related: ["linen", "lijnwaad", "platillas"],
        aat: "300204598",
        fiber: "Linen",
        technique: "Plain weave; sometimes pieced",
        origin: "The Dutch Republic and Europe",
        network: "WIC",
        image: slaaplakensImage,
        imageAlt: "White embroidered cotton and silk textile with small colored floral motifs",
        imageCaption: "A pale embroidered cloth in the project visual reference collection.",
        imageCredit: "Victoria and Albert Museum, IS.141-1954",
        catalogueUrl: "https://collections.vam.ac.uk/item/O63176/",
        accent: "moss",
        dataTerms: ["slaaplakens", "slaaplaken", "sheets"],
        essay: [
            "A familiar domestic object became a specialized commodity within Atlantic trade. Sheets were measured, graded, packed, and sent to company posts in response to specific demand.",
            "References to “old” sheets raise questions about second-hand markets and the reassignment of value across regions. Dutch merchants participated in, but did not create or control, those local systems of preference.",
            "Slaaplakens connect household linen, women’s production and supply work, and the violent commercial structures of the WIC.",
        ],
    },
    {
        slug: "streep",
        name: "Streep",
        shortName: "Streep",
        definition:
            "A European-made striped cloth, probably linen, often associated with Haarlem and circulated through the Dutch Republic, West Africa, and the Americas.",
        variants: ["streep", "stripe", "streepen", "striep", "striepe"],
        related: ["lijnwaad", "lhymenias", "platillas", "slaaplakens", "boekjes"],
        fiber: "Probably linen",
        technique: "Loom-patterned stripes",
        origin: "Haarlem and the Dutch Republic",
        network: "WIC",
        image: streepImage,
        imageAlt: "Red, white, and blue striped bontjes textile swatch on an archival page",
        imageCaption:
            "A striped bontjes swatch from the project archive, shown for pattern comparison.",
        imageCredit: "Nationaal Archief, WIC Archive, 1.05.01.02, no. 179, sample 21",
        accent: "indigo",
        dataTerms: ["streep", "streepen", "striep", "striepe"],
        essay: [
            "Invoices often pair streep with Haarlem, the Dutch Republic’s primary center for linen production. The records rarely state a fiber outright, so that association remains a strong interpretation rather than an absolute fact.",
            "Women appear among the suppliers of Haarlem striped cloth, evidence of their active role in the global linen trade. The textiles then moved into WIC and MCC commerce in West Africa and the Americas.",
            "The simple label “stripe” conceals differences in color, scale, quality, and regional reputation that mattered to historical buyers.",
        ],
    },
];

export const textiles: TextileEntry[] = textileMetadata.map((entry) => {
    const original = originalTextileCopy[entry.slug];

    return {
        ...entry,
        name: original.name,
        shortName: original.name,
        definition: original.definition,
        variants: original.variants.length ? original.variants : entry.variants,
        relatedDescription: original.relatedDescription,
        essay: original.essay,
        footnotes: original.footnotes ?? [],
        sourceUrl: original.sourceUrl,
    };
});

export const featuredTextiles = textiles.filter((textile) => textile.featured);

export function getTextile(slug: string) {
    return textiles.find((textile) => textile.slug === slug);
}
