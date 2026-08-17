<script lang="ts">
    import { page } from "$app/state";
    import "@fontsource-variable/instrument-sans/wght.css";
    import "@fontsource-variable/source-sans-3/wght.css";
    import "../app.css";
    import SiteFooter from "$lib/components/SiteFooter.svelte";
    import SiteHeader from "$lib/components/SiteHeader.svelte";

    let { children } = $props();
    const socialImage = $derived(new URL("/og.png", page.url).href);
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
<main id="main-content">
    {@render children()}
</main>
<SiteFooter />
