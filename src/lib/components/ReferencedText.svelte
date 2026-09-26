<script lang="ts">
    import { referenceParts } from "$lib/utils/footnotes";
    let {
        text,
        scope,
        section,
        noteCount,
        onfollow,
    }: {
        text: string;
        scope: string;
        section: string;
        noteCount: number;
        onfollow: (number: number, reference: string) => void;
    } = $props();
    const parts = $derived(referenceParts(text, `ref-${scope}-${section}`, noteCount));
</script>

{#each parts as part}{#if part.number}<a
            class="note-reference"
            id={part.id}
            href={`#note-${scope}-${part.number}`}
            role="doc-noteref"
            aria-label={`Note ${part.number}`}
            data-sveltekit-reload
            onclick={(event) => {
                if (
                    !event.metaKey &&
                    !event.ctrlKey &&
                    !event.altKey &&
                    !event.shiftKey &&
                    event.button === 0
                )
                    onfollow(part.number!, part.id);
            }}>{part.text}</a
        >{:else}{part.text}{/if}{/each}

<style>
    .note-reference {
        display: inline-block;
        position: relative;
        top: -0.12em;
        min-width: 1.5rem;
        min-height: 1.5rem;
        padding: 0.15rem 0.2rem;
        color: var(--madder);
        border-bottom: 1px solid currentColor;
        font: 600 0.8125rem/1.4 var(--sans);
        text-align: center;
        text-decoration: none;
        white-space: nowrap;
        scroll-margin-block: 8rem 3rem;
    }
    .note-reference:hover,
    .note-reference:focus-visible,
    .note-reference:target {
        color: var(--ink);
        background: var(--paper-deep);
        border-color: var(--madder);
    }
</style>
