<script lang="ts">
    import {
        ArrowRight,
        ArrowUpRight,
        ChartNoAxesColumnIncreasing,
        Database,
        Route,
        Search,
    } from "@lucide/svelte";
    import { projectTools } from "$lib/data/projects";

    const icons: Record<string, typeof Database> = {
        "trade-explorer": Database,
        "swatch-search": Search,
        "textiles-modifiers-and-values": ChartNoAxesColumnIncreasing,
        "textile-geographies": Route,
    };
</script>

<svelte:head>
    <title>Data Visualization — Dutch Textile Trade</title>
    <meta
        name="description"
        content="These dynamic apps allow users to explore the project data by geography, date, company, textile name and/or descriptors of the textiles found in archival sources (modifiers), and value of textiles, producing data visualizations."
    />
</svelte:head>

<div class="page-shell">
    <header class="page-intro visualization-intro">
        <div>
            <h1>Data visualization</h1>
        </div>
        <p class="lede">
            These dynamic apps allow users to explore the project data by geography, date, company,
            textile name and/or descriptors of the textiles found in archival sources (modifiers),
            and value of textiles, producing data visualizations.
        </p>
    </header>
</div>

<section class="tools page-shell">
    {#each projectTools as tool, index}
        {@const Icon = icons[tool.slug] ?? Database}
        <article class={tool.accent}>
            <div class="tool-top">
                <span>0{index + 1}</span>
                <Icon size={26} strokeWidth={1.3} />
            </div>
            <p class="tool-eyebrow">{tool.eyebrow}</p>
            <h2>{tool.title}</h2>
            <p class="tool-description">{tool.description}</p>
            <div class="tool-links">
                <a class="button" href={tool.href}>{tool.action} <ArrowRight size={15} /></a>
                <a href={`/projects/${tool.slug}/`}>Read instructions <ArrowUpRight size={13} /></a>
            </div>
        </article>
    {/each}
</section>

<style>
    .page-intro h1 {
        max-width: 10ch;
    }

    .visualization-intro {
        align-items: center;
        padding-top: clamp(3.5rem, 7vw, 6rem);
        padding-bottom: clamp(3rem, 6vw, 5rem);
    }

    .visualization-intro h1 {
        font-size: clamp(3.4rem, 7vw, 7rem);
    }

    .tools {
        display: grid;
        grid-template-columns: repeat(2, 1fr);
        gap: 1px;
        padding-top: clamp(2.5rem, 5vw, 4.5rem);
        padding-bottom: clamp(5rem, 9vw, 8rem);
        background: var(--line-strong);
        background-clip: content-box;
    }

    .tools article {
        display: flex;
        flex-direction: column;
        min-height: 24rem;
        padding: clamp(1.3rem, 3vw, 2.2rem);
        background: var(--paper);
    }

    .tool-top {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding-bottom: 1.2rem;
        border-bottom: 1px solid var(--line);
    }

    .tool-top span,
    .tool-eyebrow {
        color: var(--madder);
        font-family: var(--sans);
        font-size: 0.55rem;
        letter-spacing: 0.08em;
        text-transform: uppercase;
    }

    .indigo .tool-top span,
    .indigo .tool-eyebrow {
        color: var(--indigo);
    }

    .saffron .tool-top span,
    .saffron .tool-eyebrow {
        color: #956c24;
    }

    .tool-eyebrow {
        margin-top: 2.5rem;
        margin-bottom: 1rem;
    }

    .tools h2 {
        margin-bottom: 1.2rem;
        font-family: var(--serif);
        font-size: clamp(2rem, 3.1vw, 3.15rem);
        font-weight: 400;
        letter-spacing: -0.05em;
        line-height: 0.95;
    }

    .tool-description {
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 0.94rem;
        line-height: 1.58;
    }

    .tool-links {
        display: flex;
        flex-wrap: wrap;
        gap: 1.1rem;
        align-items: center;
        margin-top: auto;
        padding-top: 2rem;
    }

    .tool-links > a:not(.button) {
        display: inline-flex;
        align-items: center;
        gap: 0.35rem;
        color: var(--ink-soft);
        font-size: 0.65rem;
        font-weight: 700;
        text-underline-offset: 0.3rem;
    }

    @media (max-width: 900px) {
        .tools {
            grid-template-columns: 1fr;
        }

        .tools article {
            min-height: 21rem;
        }

        .tool-eyebrow {
            margin-top: 3rem;
        }
    }
</style>
