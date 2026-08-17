<script lang="ts">
    type Company = "VOC" | "WIC";

    let {
        company,
        showLabel = true,
        inverted = false,
    }: { company: Company; showLabel?: boolean; inverted?: boolean } = $props();

    const marks = {
        VOC: "/marks/voc.png",
        WIC: "/marks/wic.png",
    } satisfies Record<Company, string>;
</script>

<span class:inverted class="company-mark" title={`${company} historical monogram`}>
    <img src={marks[company]} alt="" aria-hidden="true" />
    {#if showLabel}<span>{company}</span>{/if}
</span>

<style>
    .company-mark {
        display: inline-flex;
        gap: 0.38rem;
        align-items: center;
        justify-content: center;
        line-height: 1;
    }

    img {
        width: 1.15rem;
        height: 1.15rem;
        object-fit: contain;
        filter: none;
    }

    .inverted img {
        filter: invert(1);
    }
</style>
