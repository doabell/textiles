<script lang="ts">
    import { ArrowUpRight } from "@lucide/svelte";
    import { projectTools } from "$lib/data/projects";
</script>

<svelte:head>
    <title>Data Visualization — Dutch Textile Trade</title>
    <meta
        name="description"
        content="These dynamic apps allow users to explore the project data by geography, date, company, textile name and/or descriptors of the textiles found in archival sources (modifiers), and value of textiles, producing data visualizations."
    />
</svelte:head>

<div class="page-shell">
    <header class="page-intro projects-intro">
        <div>
            <h1>Data Visualization</h1>
        </div>
        <p class="lede">
            These dynamic apps allow users to explore the project data by geography, date, company,
            textile name and/or descriptors of the textiles found in archival sources (modifiers),
            and value of textiles, producing data visualizations.
        </p>
    </header>
</div>

<section class="project-list page-shell">
    {#each projectTools as tool}
        <a href={`/projects/${tool.slug}/`}>
            <div>
                <h2>{tool.title}</h2>
                <p>{tool.description}</p>
            </div>
            <ArrowUpRight size={18} />
        </a>
    {/each}
</section>

<style>
    .projects-intro {
        align-items: center;
        padding-top: clamp(3.5rem, 7vw, 6rem);
        padding-bottom: clamp(3rem, 6vw, 5rem);
    }

    .projects-intro h1 {
        font-size: clamp(3.4rem, 7vw, 7rem);
    }

    .project-list {
        padding-top: clamp(2.5rem, 5vw, 4.5rem);
        padding-bottom: clamp(5rem, 9vw, 8rem);
        border-top: 1px solid var(--line-strong);
    }

    .project-list a {
        display: grid;
        grid-template-columns: 1fr auto;
        gap: 2rem;
        align-items: start;
        padding: clamp(1.5rem, 3vw, 2.6rem) 0;
        border-bottom: 1px solid var(--line-strong);
        text-decoration: none;
    }

    .project-list h2 {
        margin-bottom: 1rem;
        font-family: var(--editorial-font);
        font-size: clamp(2rem, 3.5vw, 3.7rem);
        font-weight: var(--display-weight, 400);
        letter-spacing: var(--display-tracking, -0.05em);
        line-height: 0.95;
    }

    .project-list p {
        max-width: 48rem;
        color: var(--ink-soft);
        font-family: var(--reading);
        font-size: 0.98rem;
        line-height: 1.58;
    }
</style>
