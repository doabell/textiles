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
    {#each projectTools as tool}
        {@const Icon = icons[tool.slug] ?? Database}
        <article class={tool.accent}>
            <div class="tool-top">
                <Icon size={26} strokeWidth={1.3} />
            </div>
            <h2>{tool.title}</h2>
            {#if tool.description}<p class="tool-description">{tool.description}</p>{/if}
            <div class="tool-links">
                <a class="button" href={tool.href}>{tool.action} <ArrowRight size={15} /></a>
                {#if tool.instructions.length}<a href={`/projects/${tool.slug}/`}
                        >Original instructions <ArrowUpRight size={13} /></a
                    >{/if}
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
        padding-top: clamp(3rem, 6vw, 5rem);
        padding-bottom: clamp(3rem, 6vw, 5rem);
    }

    .visualization-intro h1 {
        font-size: clamp(3rem, 5.7vw, 6rem);
    }

    .tools {
        display: grid;
        grid-template-columns: 1fr;
        gap: 0;
        padding-top: clamp(2.5rem, 5vw, 4.5rem);
        padding-bottom: clamp(5rem, 9vw, 8rem);
    }

    .tools article {
        display: grid;
        grid-template-columns: 2rem 0.9fr 1fr;
        gap: 1.5rem 2rem;
        align-items: start;
        padding: 2.3rem 0;
        border-top: 1px solid var(--line);
        background: var(--paper);
    }

    .tool-top {
        display: flex;
        align-items: center;
        justify-content: space-between;
        padding-top: 0.4rem;
        color: var(--madder-dark);
    }

    .tools h2 {
        margin-bottom: 1.2rem;
        font-family: var(--editorial-font);
        font-size: clamp(1.75rem, 2.7vw, 2.7rem);
        font-weight: var(--display-weight, 400);
        letter-spacing: var(--display-tracking, -0.025em);
        line-height: 1.08;
    }

    .tool-description {
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 1.1875rem;
        line-height: 1.58;
    }

    .tool-links {
        display: flex;
        flex-wrap: wrap;
        gap: 1.1rem;
        align-items: center;
        margin-top: auto;
        padding-top: 0;
        grid-column: 2 / -1;
    }

    .tool-links > a:not(.button) {
        display: inline-flex;
        align-items: center;
        gap: 0.35rem;
        color: var(--ink-soft);
        font-size: 0.85rem;
        font-weight: 700;
        text-underline-offset: 0.3rem;
    }

    @media (max-width: 900px) {
        .tools {
            grid-template-columns: 1fr;
        }

        .tools article {
            grid-template-columns: 1.5rem minmax(0, 1fr);
            gap: 1rem;
        }
        .tool-description {
            grid-column: 2;
            margin-top: 0;
        }
        .tools h2 {
            margin-bottom: 0;
        }
    }
</style>
