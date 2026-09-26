<script lang="ts">
    import { page } from "$app/state";
    import { ArrowLeft } from "@lucide/svelte";
</script>

<svelte:head>
    <title>{page.status === 404 ? "Page not found" : "Unable to load"} — Dutch Textile Trade</title>
</svelte:head>

<section class="error-page page-shell">
    <p class="eyebrow">Error {page.status}</p>
    <h1>{page.status === 404 ? "Page not found" : "Unable to load"}</h1>
    <a class="button" href="/"><ArrowLeft size={16} /> Back home</a>
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
        font-family: var(--editorial-font);
        font-size: clamp(3rem, 6vw, 6rem);
        font-weight: var(--display-weight, 400);
        letter-spacing: var(--display-tracking, -0.065em);
        line-height: 1.03;
    }
</style>
