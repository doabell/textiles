<script lang="ts">
    import OriginalCopy from "$lib/components/OriginalCopy.svelte";
    import { originalPageCopy } from "$lib/data/original-page-copy";

    const accessedDate = new Intl.DateTimeFormat("en-US", {
        year: "numeric",
        month: "long",
        day: "numeric",
    }).format(new Date());
    const sourceHtml = originalPageCopy.about.html.replaceAll("[insert date]", accessedDate);
    const match = sourceHtml.match(/^\s*<p[^>]*>([\s\S]*?)<\/p>([\s\S]*)$/);
    const opening = match?.[1] ?? "";
    const body = match?.[2] ?? sourceHtml;
</script>

<svelte:head>
    <title>About — Dutch Textile Trade Project</title>
    <meta
        name="description"
        content="Welcome to the Dutch Textile Trade Project. This project aims to understand the circulation of textiles on Dutch ships around the world in the seventeenth and eighteenth century, by examining data drawn from trade records alongside samples of textiles and visual culture depicting the use of textiles. The Visual Textile Glossary is our centerpiece, providing each historical textile term with a short definition and a longer essay contextualizing that textile’s production and circulation, with visual and material examples, and you can explore and download the relevant data."
    />
</svelte:head>

<div class="page-shell">
    <header class="page-intro">
        <div>
            <h1>About</h1>
        </div>
        <div class="lede original-opening">{@html opening}</div>
    </header>
</div>

<section class="about-copy page-shell">
    <div class="copy-marker">
        <span>About the project</span>
    </div>
    <OriginalCopy html={body} />
</section>

<style>
    .page-intro h1 {
        max-width: 9ch;
    }

    .original-opening :global(p) {
        margin: 0;
    }

    .about-copy {
        display: grid;
        grid-template-columns: minmax(7.5rem, 10rem) minmax(0, 65ch);
        gap: clamp(2.5rem, 6vw, 6rem);
        justify-content: center;
        padding-top: clamp(3.5rem, 7vw, 6rem);
        padding-bottom: clamp(5rem, 9vw, 8rem);
    }

    .copy-marker {
        padding-top: 0.35rem;
        color: var(--madder);
        border-top: 1px solid var(--line-strong);
        font-family: var(--sans);
        font-size: 0.74rem;
        font-weight: 650;
        letter-spacing: 0;
    }

    @media (max-width: 700px) {
        .about-copy {
            grid-template-columns: 1fr;
        }
    }
</style>
