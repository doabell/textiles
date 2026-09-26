<script lang="ts">
    import { afterNavigate, replaceState } from "$app/navigation";
    import { page } from "$app/state";
    import { onMount, tick } from "svelte";
    import { Check, SlidersHorizontal } from "@lucide/svelte";
    import {
        designFeels,
        defaultFeel,
        feelStorageKey,
        isDesignFeel,
        type DesignFeel,
    } from "$lib/design/feels";

    let current = $state<DesignFeel>(defaultFeel);
    let open = $state(false);
    let root: HTMLDivElement;
    let trigger: HTMLButtonElement;
    let panel = $state<HTMLDivElement>();
    let { onselect }: { onselect?: () => void } = $props();
    const id = $props.id();

    function sync() {
        const value = document.documentElement.dataset.feel;
        current = isDesignFeel(value) ? value : defaultFeel;
    }
    function apply(value: DesignFeel) {
        document.documentElement.dataset.feel = value;
        document.documentElement.style.colorScheme = value === "immersive" ? "dark" : "light";
        try {
            localStorage.setItem(feelStorageKey, value);
        } catch {
            /* Storage is optional. */
        }
        window.dispatchEvent(new Event("textiles:feel"));
    }
    afterNavigate(() => {
        const value = page.url.searchParams.get("feel");
        if (isDesignFeel(value)) apply(value);
        sync();
        open = false;
    });
    onMount(() => {
        sync();
        window.addEventListener("textiles:feel", sync);
        return () => window.removeEventListener("textiles:feel", sync);
    });
    async function choose(value: DesignFeel) {
        apply(value);
        const url = new URL(page.url);
        url.searchParams.set("feel", value);
        replaceState(url, page.state);
        open = false;
        onselect?.();
        await tick();
        if (trigger?.isConnected) trigger.focus();
    }
    async function toggle() {
        open = !open;
        if (open) {
            await tick();
            panel?.querySelector<HTMLButtonElement>('[aria-pressed="true"]')?.focus();
        }
    }
    function close(event: KeyboardEvent) {
        if (event.key === "Escape" && open) {
            event.stopPropagation();
            open = false;
            trigger?.focus();
        }
    }
    function scope(node: HTMLElement) {
        node.addEventListener("keydown", close);
        return {
            destroy() {
                node.removeEventListener("keydown", close);
            },
        };
    }
</script>

<svelte:window
    onpointerdown={(event) => {
        if (open && !event.composedPath().includes(root)) open = false;
    }}
/>
<div class="feel-switcher" bind:this={root} use:scope>
    <button
        class="feel-trigger"
        type="button"
        bind:this={trigger}
        onclick={toggle}
        aria-label="Change feel"
        aria-expanded={open}
        aria-controls={id}
    >
        <SlidersHorizontal size={16} strokeWidth={1.6} /><span>Feel</span>
        <strong
            >{#each designFeels as feel}<span class="feel-name" data-feel-label={feel.id}
                    >{feel.label}</span
                >{/each}</strong
        >
    </button>
    {#if open}
        <div class="feel-panel" {id} bind:this={panel} role="group" aria-label="Design feels">
            {#each designFeels as feel}
                <button
                    class="feel-option"
                    type="button"
                    aria-pressed={current === feel.id}
                    onclick={() => choose(feel.id)}
                >
                    <span class={"feel-sample " + feel.id} aria-hidden="true"
                        ><i></i><i></i><i></i></span
                    >
                    <span>{feel.label}</span>
                    {#if current === feel.id}<Check size={15} />{/if}
                </button>
            {/each}
        </div>
    {/if}
</div>

<style>
    .feel-switcher {
        position: relative;
    }
    .feel-trigger {
        display: flex;
        align-items: center;
        gap: 0.5rem;
        min-height: 2.75rem;
        padding: 0.5rem 0.65rem;
        color: var(--ink);
        background: transparent;
        border: 1px solid var(--line-strong);
        font: var(--type-label);
        white-space: nowrap;
    }
    .feel-trigger strong {
        font-weight: 500;
        color: var(--ink-soft);
    }
    .feel-name {
        display: none;
    }
    :global(html[data-feel="editorial"]) .feel-name[data-feel-label="editorial"],
    :global(html[data-feel="gallery"]) .feel-name[data-feel-label="gallery"],
    :global(html[data-feel="immersive"]) .feel-name[data-feel-label="immersive"],
    :global(html[data-feel="folio"]) .feel-name[data-feel-label="folio"] {
        display: inline;
    }
    .feel-panel {
        position: absolute;
        z-index: 80;
        right: 0;
        top: calc(100% + 0.7rem);
        width: 16rem;
        display: grid;
        padding: 0.5rem;
        border: 1px solid var(--line-strong);
        background: var(--paper);
        box-shadow: 0 0.8rem 2.5rem #0002;
    }
    .feel-option {
        display: grid;
        grid-template-columns: 3.5rem 1fr 1rem;
        align-items: center;
        gap: 0.8rem;
        min-height: 4.5rem;
        padding: 0.5rem;
        border: 0;
        text-align: left;
        color: var(--ink);
        background: transparent;
        font: var(--type-label);
    }
    .feel-option:hover,
    .feel-option[aria-pressed="true"] {
        background: var(--paper-deep);
    }
    .feel-sample {
        display: grid;
        align-content: center;
        gap: 4px;
        height: 3rem;
        padding: 8px;
        border: 1px solid #0003;
        background: #f7f5ef;
    }
    .feel-sample i {
        height: 2px;
        background: #171711;
    }
    .feel-sample i:first-child {
        width: 55%;
        height: 5px;
    }
    .feel-sample i:last-child {
        width: 75%;
        background: #b9402b;
    }
    .feel-sample.gallery {
        background: #fff;
        gap: 3px;
    }
    .feel-sample.gallery i:first-child {
        height: 8px;
        width: 100%;
    }
    .feel-sample.gallery i:last-child {
        background: #1744d4;
    }
    .feel-sample.immersive {
        background: #191c19;
        padding-top: 20px;
    }
    .feel-sample.immersive i {
        background: #eeede6;
    }
    .feel-sample.immersive i:last-child {
        background: #edbc78;
    }
    .feel-sample.folio {
        background: #f4dcd6;
        border: 1px solid #692638;
        grid-template-columns: 1fr 1fr;
        gap: 4px;
    }
    .feel-sample.folio i {
        background: #692638;
    }
    .feel-sample.folio i:first-child {
        grid-row: 1 / 3;
        width: 100%;
        height: 1.8rem;
        background: #fff8ef;
        border: 1px solid #692638;
    }
    .feel-sample.folio i:last-child {
        width: 100%;
    }
    @media (min-width: 761px) and (max-width: 1250px) {
        .feel-trigger strong {
            display: none;
        }
    }
    @media (max-width: 760px) {
        .feel-trigger {
            width: 100%;
            justify-content: flex-start;
            margin-top: 1rem;
            min-height: 3rem;
        }
        .feel-trigger strong {
            margin-left: auto;
        }
        .feel-panel {
            position: static;
            width: 100%;
            margin-top: 0.5rem;
            box-shadow: none;
        }
    }
</style>
