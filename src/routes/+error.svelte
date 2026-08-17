<script lang="ts">
    import { page } from "$app/state";
    import { ArrowLeft } from "@lucide/svelte";
</script>

<svelte:head>
    <title
        >{page.status === 404 ? "Page not found" : "Something went wrong"} — Dutch Textile Trade</title
    >
</svelte:head>

<section class="error-page page-shell">
    <p class="eyebrow">Error {page.status}</p>
    <h1>{page.status === 404 ? "This thread ends here." : "The archive could not be opened."}</h1>
    <p>
        {page.status === 404
            ? "The page may have moved, or the historical term may use a different spelling."
            : page.error?.message || "Please return to the project and try another path."}
    </p>
    <a class="button" href="/"><ArrowLeft size={16} /> Return to the project</a>
</section>

<style>
    .error-page {
        display: grid;
        align-content: center;
        justify-items: start;
        min-height: 70svh;
        padding-top: 5rem;
        padding-bottom: 5rem;
    }

    h1 {
        max-width: 10ch;
        margin-bottom: 1.5rem;
        font-family: var(--serif);
        font-size: clamp(4rem, 9vw, 9rem);
        font-weight: 400;
        letter-spacing: -0.065em;
        line-height: 0.86;
    }

    .error-page > p:not(.eyebrow) {
        max-width: 35rem;
        margin-bottom: 2rem;
        color: var(--ink-soft);
        font-family: var(--serif);
        font-size: 1.1rem;
    }
</style>
