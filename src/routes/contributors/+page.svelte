<script lang="ts">
    import OriginalCopy from "$lib/components/OriginalCopy.svelte";
    import { originalPageCopy } from "$lib/data/original-page-copy";
</script>

<svelte:head>
    <title>Contributors — Dutch Textile Trade</title>
    <meta
        name="description"
        content="Co-PIs: Carrie Anderson, Middlebury College & Marsely Kehoe, Independent Researcher"
    />
</svelte:head>

<div class="page-shell">
    <header class="page-intro">
        <div>
            <h1>Contributors</h1>
        </div>
    </header>
</div>

<section class="contributors-copy page-shell">
    <div class="copy-marker">
        <span>Contributors</span>
    </div>
    <OriginalCopy html={originalPageCopy.contributors.html} />
</section>

<style>
    .contributors-copy {
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
        .contributors-copy {
            grid-template-columns: 1fr;
        }
    }
</style>
