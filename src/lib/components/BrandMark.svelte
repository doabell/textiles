<script lang="ts">
    let { compact = false }: { compact?: boolean } = $props();
</script>

<span class:compact class="brand-mark" aria-hidden="true">
    {#each Array(16) as _, index}
        <i class:accent={index === 1 || index === 6 || index === 11 || index === 12}></i>
    {/each}
</span>

<style>
    .brand-mark {
        display: grid;
        grid-template-columns: repeat(4, 0.36rem);
        grid-template-rows: repeat(4, 0.36rem);
        gap: 0.12rem;
        width: max-content;
        padding: 0.43rem;
        border: 1px solid color-mix(in srgb, currentColor 38%, transparent);
        color: inherit;
    }

    i {
        display: block;
        width: 0.36rem;
        height: 0.36rem;
        background: currentColor;
        opacity: 0.82;
    }

    i.accent {
        background: var(--madder);
        opacity: 1;
    }

    .compact {
        grid-template-columns: repeat(4, 0.28rem);
        grid-template-rows: repeat(4, 0.28rem);
        gap: 0.1rem;
        padding: 0.34rem;
    }

    .compact i {
        width: 0.28rem;
        height: 0.28rem;
    }
</style>
