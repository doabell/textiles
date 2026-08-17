<script lang="ts">
    let { html, compact = false }: { html: string; compact?: boolean } = $props();
</script>

<div class:compact class="original-copy">
    {@html html}
</div>

<style>
    .original-copy {
        max-width: 65ch;
        color: #3d3c34;
        font-family: var(--reading);
        font-size: clamp(1.03rem, 1.05vw, 1.1rem);
        font-variation-settings: "wght" 410;
        line-height: 1.68;
        text-wrap: pretty;
    }

    .original-copy.compact {
        max-width: 60ch;
    }

    :global(.original-copy p) {
        margin: 0 0 1.15rem;
    }

    :global(.original-copy p:has(> strong:only-child)) {
        margin-top: 3rem;
        margin-bottom: 1.1rem;
        padding-bottom: 0.7rem;
        color: var(--ink);
        font-family: var(--sans);
        font-size: 1.22rem;
        font-weight: 650;
        letter-spacing: -0.025em;
        line-height: 1.3;
        border-bottom: 1px solid var(--line);
    }

    :global(.original-copy p:has(> strong:first-child):not(:has(> strong:only-child))) {
        margin-top: 2.5rem;
    }

    :global(.original-copy p > strong:first-child) {
        display: block;
        margin-bottom: 0.45rem;
        font-family: var(--sans);
        font-size: 1.03rem;
        font-weight: 650;
        letter-spacing: -0.015em;
        line-height: 1.35;
    }

    :global(.original-copy ul),
    :global(.original-copy ol) {
        display: grid;
        gap: 0.7rem;
        margin: 0 0 1.6rem;
        padding-left: 1.25rem;
    }

    :global(.original-copy a) {
        color: var(--madder-dark);
        overflow-wrap: anywhere;
        text-decoration-thickness: 1px;
        text-underline-offset: 0.22rem;
    }

    :global(.original-copy em) {
        font-style: italic;
    }

    :global(.original-copy strong) {
        color: var(--ink);
    }
</style>
