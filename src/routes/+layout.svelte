<script lang="ts">
    import { onNavigate } from "$app/navigation";
    import { navigating, page } from "$app/state";
    import "@fontsource-variable/instrument-sans/wght.css";
    import "@fontsource-variable/newsreader/standard.css";
    import "@fontsource-variable/newsreader/standard-italic.css";
    import "../app.css";
    import "$lib/design/feels.css";
    import SiteFooter from "$lib/components/SiteFooter.svelte";
    import SiteHeader from "$lib/components/SiteHeader.svelte";

    let { children } = $props();
    const socialImage = $derived(new URL("/og.png", page.url).href);

    onNavigate((navigation) => {
        if (
            !document.startViewTransition ||
            window.matchMedia("(prefers-reduced-motion: reduce)").matches
        )
            return;
        return new Promise<void>((resolve) => {
            const transition = document.startViewTransition(async () => {
                resolve();
                await navigation.complete;
            });
            void transition.finished.catch(() => undefined);
        });
    });
</script>

<svelte:head>
    <title>Dutch Textile Trade Project</title>
    <meta
        name="description"
        content="This project aims to understand the circulation of globally-sourced textiles on Dutch ships around the world in the seventeenth and eighteenth centuries by examining data drawn from trade records alongside samples of textiles and visual culture depicting textiles in use."
    />
    <meta property="og:type" content="website" />
    <meta property="og:site_name" content="Dutch Textile Trade Project" />
    <meta property="og:title" content="Dutch Textile Trade Project" />
    <meta
        property="og:description"
        content="This project aims to understand the circulation of globally-sourced textiles on Dutch ships around the world in the seventeenth and eighteenth centuries by examining data drawn from trade records alongside samples of textiles and visual culture depicting textiles in use."
    />
    <meta property="og:image" content={socialImage} />
    <meta name="twitter:card" content="summary_large_image" />
    <meta name="twitter:title" content="Dutch Textile Trade Project" />
    <meta
        name="twitter:description"
        content="This project aims to understand the circulation of globally-sourced textiles on Dutch ships around the world in the seventeenth and eighteenth centuries by examining data drawn from trade records alongside samples of textiles and visual culture depicting textiles in use."
    />
    <meta name="twitter:image" content={socialImage} />
</svelte:head>

<a class="skip-link" href="#main-content">Skip to content</a>
<SiteHeader />
{#if navigating.to}<div
        class="navigation-progress"
        role="progressbar"
        aria-label="Loading"
    ></div>{/if}
<main id="main-content">
    {@render children()}
</main>
<SiteFooter />
