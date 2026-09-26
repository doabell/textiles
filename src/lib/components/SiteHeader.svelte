<script lang="ts">
    import { afterNavigate } from "$app/navigation";
    import { page } from "$app/state";
    import { Home as HomeIcon, Menu, X } from "@lucide/svelte";
    import BrandMark from "./BrandMark.svelte";
    import FeelSwitcher from "./FeelSwitcher.svelte";

    let menuOpen = $state(false);
    let menuButton: HTMLButtonElement;
    let headerElement: HTMLElement;

    afterNavigate(() => {
        menuOpen = false;
    });

    function closeMenu(event: KeyboardEvent) {
        if (event.key === "Escape" && menuOpen) {
            menuOpen = false;
            menuButton?.focus();
        }
    }

    const nav = [
        { href: "/", label: "Home" },
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

<svelte:window
    onkeydown={closeMenu}
    onclick={(event) => {
        if (menuOpen && !event.composedPath().includes(headerElement)) menuOpen = false;
    }}
/>

<header class="site-header" bind:this={headerElement}>
    <a class="brand" href="/" aria-label="Home" onclick={() => (menuOpen = false)}>
        <BrandMark compact />
        <span>
            <strong>Dutch Textile Trade</strong>
        </span>
    </a>

    <div class="desktop-actions">
        <nav class="desktop-nav" aria-label="Primary navigation">
            {#each nav as item}
                <a
                    class:active={isActive(item.href)}
                    aria-current={isActive(item.href) ? "page" : undefined}
                    href={item.href}>{item.label}</a
                >
            {/each}
        </nav>

        <FeelSwitcher />
    </div>

    <div class="mobile-actions">
        <a
            class="mobile-home"
            href="/"
            aria-label="Home"
            aria-current={page.url.pathname === "/" ? "page" : undefined}
            ><HomeIcon size={18} /><span>Home</span></a
        >
        <button
            bind:this={menuButton}
            class="menu-button"
            aria-controls="mobile-navigation"
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
    </div>

    {#if menuOpen}
        <nav id="mobile-navigation" class="mobile-nav" aria-label="Mobile navigation">
            {#each nav as item}
                <a
                    aria-current={isActive(item.href) ? "page" : undefined}
                    class:active={isActive(item.href)}
                    href={item.href}
                    onclick={() => (menuOpen = false)}
                >
                    {item.label}
                </a>
            {/each}
            <FeelSwitcher
                onselect={() => {
                    menuOpen = false;
                    requestAnimationFrame(() => {
                        const target = menuButton?.getClientRects().length
                            ? menuButton
                            : headerElement?.querySelector<HTMLButtonElement>(
                                  ".desktop-actions .feel-trigger",
                              );
                        target?.focus();
                    });
                }}
            />
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
