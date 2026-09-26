<script lang="ts">
    import { Download, LoaderCircle } from "@lucide/svelte";
    let {
        action,
        disabled = false,
        label = "Download image",
    }: { action: () => Promise<void>; disabled?: boolean; label?: string } = $props();
    let busy = $state(false);
    let failed = $state(false);
    async function save() {
        if (busy) return;
        busy = true;
        failed = false;
        try {
            await action();
        } catch {
            failed = true;
        } finally {
            busy = false;
        }
    }
</script>

<button type="button" class="chart-download" disabled={disabled || busy} onclick={save}>
    {#if busy}<LoaderCircle size={16} />{:else}<Download size={16} />{/if}
    {busy ? "Preparing image" : failed ? "Retry image" : label}
</button>

<style>
    .chart-download {
        display: inline-flex;
        align-items: center;
        justify-content: center;
        gap: 0.5rem;
        min-height: 2.8rem;
        padding: 0.65rem 1rem;
        border: 1px solid color-mix(in srgb, currentColor 35%, transparent);
        color: inherit;
        background: transparent;
        font: var(--type-label);
    }
    .chart-download:hover:not(:disabled) {
        background: color-mix(in srgb, currentColor 8%, transparent);
    }
</style>
