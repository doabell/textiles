<script lang="ts">
    import { localizeHtml } from "$lib/utils/local-links";
    let { html, compact = false }: { html: string; compact?: boolean } = $props();
    const content = $derived(
        localizeHtml(html).replace(
            /<p[^>]*>\s*<strong>([\s\S]*?)<\/strong>\s*<\/p>/g,
            "<h2>$1</h2>",
        ),
    );
</script>

<div class:compact class="original-copy">{@html content}</div>

<style>
    .original-copy {
        max-width: var(--reading-width);
        color: var(--ink);
        font: var(--type-body);
        text-wrap: pretty;
    }
    .original-copy.compact {
        max-width: 58ch;
    }
    :global(.original-copy p) {
        margin: 0 0 1.25em;
    }
    :global(.original-copy h2) {
        margin: 2.8em 0 1em;
        padding-top: 1.2rem;
        border-top: 1px solid var(--line);
        font: var(--type-section);
        letter-spacing: var(--display-tracking, -0.025em);
    }
    :global(.original-copy h3) {
        margin: 2rem 0 0.75rem;
        font: 550 1rem/1.5 var(--sans);
    }
    :global(.original-copy > :first-child) {
        margin-top: 0;
    }
    :global(.original-copy ul),
    :global(.original-copy ol) {
        display: grid;
        gap: 0.65em;
        margin: 0 0 1.5em;
        padding-left: 1.25em;
    }
    :global(.original-copy a) {
        color: var(--madder-dark);
        overflow-wrap: anywhere;
        text-decoration-thickness: 1px;
        text-underline-offset: 0.18em;
    }
    :global(.original-copy strong) {
        font-weight: 600;
    }
    :global(.original-copy em) {
        font-style: italic;
    }
</style>
