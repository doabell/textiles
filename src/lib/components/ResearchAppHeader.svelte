<script lang="ts">
    import { Plus } from "@lucide/svelte";
    import { projectTools } from "$lib/data/projects";
    let {
        active,
    }: {
        active:
            | "trade-explorer"
            | "textile-geographies"
            | "textiles-modifiers-and-values"
            | "swatch-search";
    } = $props();
    const current = $derived(projectTools.find((tool) => tool.slug === active) ?? projectTools[0]!);
</script>

<header class="research-header page-shell">
    <div class="research-title">
        <h1>{current.title}</h1>
        {#if current.description}
            <details class="research-about">
                <summary>About <Plus size={16} strokeWidth={1.5} aria-hidden="true" /></summary>
                <p>{current.description}</p>
            </details>
        {/if}
    </div>
</header>

<style>
    .research-header {
        padding-top: clamp(2.5rem, 5vw, 4.5rem);
    }
    .research-title {
        display: grid;
        grid-template-columns: minmax(0, 1fr) auto;
        align-items: end;
        gap: 2rem;
    }
    h1 {
        max-width: 25ch;
        margin: 0;
        font-family: var(--display-font, var(--sans));
        font-size: clamp(2.5rem, 5vw, 5.2rem);
        font-weight: var(--display-weight, 500);
        letter-spacing: var(--display-tracking, -0.045em);
        line-height: 1.05;
    }
    .research-about {
        max-width: 34rem;
    }
    .research-about summary {
        display: flex;
        align-items: center;
        justify-content: flex-end;
        gap: 1rem;
        min-height: 3rem;
        font-family: var(--sans);
        font-size: 0.875rem;
        list-style: none;
    }
    .research-about summary::-webkit-details-marker {
        display: none;
    }
    .research-about summary :global(svg) {
        transition: transform 180ms ease;
    }
    .research-about[open] summary :global(svg) {
        transform: rotate(45deg);
    }
    .research-about[open] {
        grid-column: 1 / -1;
    }
    .research-about[open] summary {
        justify-content: flex-start;
    }
    .research-about p {
        max-width: 65ch;
        margin: 0.4rem 0 0;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: clamp(1.1875rem, 1.5vw, 1.375rem);
        line-height: 1.65;
    }
    @media (max-width: 680px) {
        .research-title {
            gap: 1rem;
        }
        h1 {
            font-size: clamp(2.3rem, 8.5vw, 4rem);
        }
    }
</style>
