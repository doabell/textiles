<script lang="ts">
    import OriginalCopy from "$lib/components/OriginalCopy.svelte";
    import { originalPageCopy } from "$lib/data/original-page-copy";

    const sourceHtml = originalPageCopy.sources.html
        .replace(
            "/wp-content/uploads/2023/03/SRC_Primary.xlsx",
            "https://dutchtextiletrade.org/wp-content/uploads/2023/03/SRC_Primary.xlsx",
        )
        .replace(
            "<strong>Secondary Sources</strong>",
            '<strong id="secondary">Secondary Sources</strong>',
        );
</script>

<svelte:head>
    <title>Sources — Dutch Textile Trade</title>
    <meta name="description" content="Primary sources for the project data:" />
</svelte:head>

<div class="page-shell">
    <header class="page-intro">
        <div>
            <h1>Sources</h1>
        </div>
    </header>
</div>

<section class="sources-copy page-shell">
    <aside aria-label="Page sections">
        <a href="#primary">Primary sources</a>
        <a href="#secondary">Secondary sources</a>
    </aside>
    <div id="primary">
        <OriginalCopy html={sourceHtml} />
    </div>
</section>

<style>
    .sources-copy {
        display: grid;
        grid-template-columns: minmax(7.5rem, 10rem) minmax(0, 65ch);
        gap: clamp(2.5rem, 6vw, 6rem);
        justify-content: center;
        padding-top: clamp(3.5rem, 7vw, 6rem);
        padding-bottom: clamp(5rem, 9vw, 8rem);
    }

    aside {
        position: sticky;
        top: 7rem;
        display: grid;
        align-self: start;
        border-top: 1px solid var(--line-strong);
    }

    aside a {
        padding: 0.8rem 0;
        color: var(--ink-soft);
        border-bottom: 1px solid var(--line);
        font-family: var(--sans);
        font-size: 0.72rem;
        font-weight: 620;
        letter-spacing: 0;
        text-decoration: none;
    }

    :global(#secondary) {
        scroll-margin-top: 8rem;
    }

    @media (max-width: 700px) {
        .sources-copy {
            grid-template-columns: 1fr;
        }

        aside {
            position: static;
        }
    }
</style>
