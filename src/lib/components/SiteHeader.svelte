<script lang="ts">
    import { page } from "$app/state";
    import { Menu, X } from "@lucide/svelte";
    import BrandMark from "./BrandMark.svelte";

    let menuOpen = $state(false);

    const nav = [
        { href: "/textiles/", label: "Textiles" },
        { href: "/explore/", label: "Explore" },
        { href: "/map/", label: "Map" },
        { href: "/values/", label: "Compare" },
        { href: "/swatches/", label: "Swatches" },
        { href: "/about/", label: "About" },
    ];

    const isActive = (href: string) =>
        href === "/" ? page.url.pathname === "/" : page.url.pathname.startsWith(href);
</script>

<header class="site-header">
    <a
        class="brand"
        href="/"
        aria-label="Dutch Textile Trade Project, home"
        onclick={() => (menuOpen = false)}
    >
        <BrandMark compact />
        <span>
            <strong>Dutch Textile Trade</strong>
            <small>Dutch Textile Trade Project</small>
        </span>
    </a>

    <nav class="desktop-nav" aria-label="Primary navigation">
        {#each nav as item}
            <a class:active={isActive(item.href)} href={item.href}>{item.label}</a>
        {/each}
    </nav>

    <button
        class="menu-button"
        type="button"
        aria-label={menuOpen ? "Close navigation" : "Open navigation"}
        aria-expanded={menuOpen}
        onclick={() => (menuOpen = !menuOpen)}
    >
        {#if menuOpen}
            <X size={22} strokeWidth={1.7} />
        {:else}
            <Menu size={22} strokeWidth={1.7} />
        {/if}
    </button>

    {#if menuOpen}
        <nav class="mobile-nav" aria-label="Mobile navigation">
            {#each nav as item, index}
                <a
                    class:active={isActive(item.href)}
                    href={item.href}
                    onclick={() => (menuOpen = false)}
                >
                    <span>0{index + 1}</span>
                    {item.label}
                </a>
            {/each}
            <div class="mobile-meta">
                <a href="/data-visualization/" onclick={() => (menuOpen = false)}
                    >All research apps</a
                >
                <a href="/data/" onclick={() => (menuOpen = false)}>Our data</a>
                <a href="/sources/" onclick={() => (menuOpen = false)}>Sources</a>
            </div>
        </nav>
    {/if}
</header>
