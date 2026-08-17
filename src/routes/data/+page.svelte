<script lang="ts">
    import { ArrowDownToLine, FileSpreadsheet } from "@lucide/svelte";
    import OriginalCopy from "$lib/components/OriginalCopy.svelte";
    import { originalPageCopy } from "$lib/data/original-page-copy";
    import { assetPath } from "$lib/utils/asset-path";
    import fullWorkbook from "../../../data/WICVOCDataAll_071723.xlsx?url";
    import cleanedCsv from "../../../pictures/datasets/WIC_VOC_Cleaned.csv?url";

    const body = originalPageCopy.data.html.replace(/^\s*<p[^>]*>[\s\S]*?<\/p>/, "").trim();
</script>

<svelte:head>
    <title>Our Data — Dutch Textile Trade</title>
    <meta
        name="description"
        content="Our data derives from Dutch East and West India Company archives and documents, which account for the movement and value of goods as they related to the Dutch economy, and which were produced from a Dutch perspective that justified territorial expansion and the enslavement of human beings in the name of trade, profit, and exploration."
    />
</svelte:head>

<div class="page-shell">
    <header class="page-intro">
        <div>
            <h1>Our Data</h1>
        </div>
    </header>
</div>

<section class="download-section page-shell">
    <div>
        <p>Download csv file of dataset, last updated July 17, 2023.</p>
    </div>
    <div class="downloads">
        <a href={assetPath(cleanedCsv)} download>
            <FileSpreadsheet size={24} strokeWidth={1.4} />
            <span>WIC_VOC_Cleaned.csv</span>
            <ArrowDownToLine size={18} />
        </a>
        <a href={assetPath(fullWorkbook)} download>
            <FileSpreadsheet size={24} strokeWidth={1.4} />
            <span>WICVOCDataAll_071723.xlsx</span>
            <ArrowDownToLine size={18} />
        </a>
    </div>
</section>

<section class="data-copy page-shell">
    <aside>Our Data</aside>
    <OriginalCopy html={body} />
</section>

<style>
    .download-section {
        display: grid;
        grid-template-columns: minmax(18rem, 0.65fr) 1fr;
        gap: clamp(3rem, 8vw, 9rem);
        padding-top: clamp(3rem, 6vw, 5rem);
        padding-bottom: clamp(3rem, 6vw, 5rem);
        border-bottom: 1px solid var(--line);
    }

    .download-section > div:first-child p {
        max-width: 28rem;
        font-family: var(--reading);
        font-size: clamp(1.18rem, 2vw, 1.55rem);
        line-height: 1.45;
    }

    .downloads {
        border-top: 1px solid var(--line-strong);
    }

    .downloads a {
        display: grid;
        grid-template-columns: auto 1fr auto;
        gap: 1rem;
        align-items: center;
        min-height: 6rem;
        padding: 1.2rem;
        border-bottom: 1px solid var(--line-strong);
        font-family: var(--sans);
        font-size: 0.62rem;
        text-decoration: none;
    }

    .downloads a:hover {
        color: var(--paper);
        background: var(--indigo);
    }

    .data-copy {
        display: grid;
        grid-template-columns: minmax(7.5rem, 10rem) minmax(0, 65ch);
        gap: clamp(2.5rem, 6vw, 6rem);
        justify-content: center;
        padding-top: clamp(3.5rem, 7vw, 6rem);
        padding-bottom: clamp(5rem, 9vw, 8rem);
    }

    .data-copy aside {
        align-self: start;
        padding-top: 0.35rem;
        color: var(--madder);
        border-top: 1px solid var(--line-strong);
        font-family: var(--sans);
        font-size: 0.74rem;
        font-weight: 650;
        letter-spacing: 0;
    }

    @media (max-width: 760px) {
        .download-section,
        .data-copy {
            grid-template-columns: 1fr;
        }
    }
</style>
